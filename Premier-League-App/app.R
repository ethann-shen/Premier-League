library(tidyverse)
library(shiny)
library(shinydashboard)
library(lubridate)
library(RColorBrewer)
library(caret)
library(kernlab)
library(DT)
library(ggrepel)
library(knitr)
library(kableExtra)
library(klaR)
library(dplyr)

#=========================================================== Raw Data ===========================================================#

PL_data_app = read_csv("premier_league_data.csv") 

PL_data_app <- PL_data_app[1:3420,] %>%
    mutate(Date = dmy(Date)) %>%
    filter(Date >= "2010-07-14") %>%
    mutate(HT_Points = case_when(
        FTR == "H" ~ 3,
        FTR == "D" ~ 1,
        FTR == "A" ~ 0
    ),
    AT_Points = case_when(
        FTR == "A" ~ 3,
        FTR == "D" ~ 1,
        FTR == "H" ~ 0
    ),
    Season = case_when(
        Date >= "2018-08-01" & Date <= "2019-06-01" ~ "2018-2019",
        Date >= "2017-08-01" & Date <= "2018-06-01" ~ "2017-2018",
        Date >= "2016-08-01" & Date <= "2017-06-01" ~ "2016-2017",
        Date >= "2015-08-01" & Date <= "2016-06-01" ~ "2015-2016",
        Date >= "2014-08-01" & Date <= "2015-06-01" ~ "2014-2015",
        Date >= "2013-08-01" & Date <= "2014-06-01" ~ "2013-2014",
        Date >= "2012-08-01" & Date <= "2013-06-01" ~ "2012-2013",
        Date >= "2011-08-01" & Date <= "2012-06-01" ~ "2011-2012",
        Date >= "2010-08-01" & Date <= "2011-06-01" ~ "2010-2011"
    ))

#=========================================================== Data for Goals Plot ===========================================================#

home_app <- PL_data_app %>%
    dplyr::select(Date, HomeTeam, FTHG, Season) %>%
    rename(Team = HomeTeam,
           FTG = FTHG) %>%
    mutate(home_away = "Home")

away_app <- PL_data_app %>%
    dplyr::select(Date, AwayTeam, FTAG, Season) %>%
    rename(Team = AwayTeam,
           FTG = FTAG)  %>%
    mutate(home_away = "Away")

all_goals_app <- rbind(home_app, away_app) %>%
    arrange(Date) %>%
    group_by(Team, Season) %>%
    mutate(cumgoals = cumsum(FTG),
           final_id = row_number()) %>%
    arrange(desc(Season), desc(final_id), desc(cumgoals))

#=========================================================== Data for Standings Plot ===========================================================#

home_points_app <- PL_data_app %>%
    dplyr::select(Date, HomeTeam, HT_Points, Season) %>%
    rename(Team = HomeTeam,
           FT_Points = HT_Points)

away_points_app <- PL_data_app %>%
    dplyr::select(Date, AwayTeam, AT_Points, Season) %>%
    rename(Team = AwayTeam,
           FT_Points = AT_Points)

team_points_app <- rbind(home_points_app, away_points_app) %>%
    arrange(Date) %>%
    group_by(Team, Season) %>%
    mutate(cumpoints = cumsum(FT_Points)) %>%
    mutate(final_id = row_number()) %>%
    arrange(desc(Season), desc(cumpoints)) 

#=========================================================== Data to Display ===========================================================#

display_data <- merge(all_goals_app, team_points_app, by = c("Date", "Team", "Season", "final_id")) %>%
    arrange(desc(Season), Date, Team) %>%
    rename("Location" = home_away,
           "Full Time Goals" = FTG,
           "Cumulative Goals" = cumgoals,
           "Cumulative Points" = cumpoints)

# Goals 
all_goals_app_shiny <- display_data %>%
    dplyr::select(Date, Team, "Full Time Goals", "Cumulative Goals", "Location", Season)


# Standings
team_points_app_shiny <- display_data %>%
    mutate(Outcome = case_when(
        FT_Points == 3 ~ "Win", 
        FT_Points == 1 ~ "Draw", 
        FT_Points == 0 ~ "Loss"
    ))  %>%
    dplyr::select(Date, Team, Outcome, "Cumulative Points", "Location", Season) 


# Plot Colors 

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col.pal = c(col_vector[5:25])

#=========================================================== Data for Modeling ===========================================================#

PL_modeling_app <- PL_data_app %>%
    dplyr::select(-Div, -HTR, -Referee, -FTHG, -FTAG, -HTHG, -HTAG) %>%
    dplyr::select(Date, HT_Points, AT_Points, everything()) 

cols <- colnames(PL_modeling_app)
away_modeling_app <- PL_modeling_app[grep(c("^[^H]"), cols)] %>%
    mutate(home_away = "Away") %>%
    mutate(win_loss = case_when(
        AT_Points == 3 ~ 1,
        AT_Points == 1 ~ 0,
        AT_Points == 0 ~ -1
    )) %>%
    dplyr::select(win_loss, everything())
home_modeling_app <- PL_modeling_app[grep(c("^[^A]"), cols)] %>%
    mutate(home_away = "Home") %>%
    mutate(win_loss = case_when(
        HT_Points == 3 ~ 1,
        HT_Points == 1 ~ 0,
        HT_Points == 0 ~ -1
    )) %>%
    dplyr::select(win_loss, everything())

new_col_names = c("win_loss", "date", "points", "team", "full_time_results", "shots", "shots_on_target", "fouls", "corners", "yellow_cards", "red_cards", "season", "home_away")
names(away_modeling_app) <- new_col_names
names(home_modeling_app) <- new_col_names

PL_modeling_combined_app <- rbind(home_modeling_app, away_modeling_app)%>%
    arrange(desc(season), date) %>%
    group_by(season, team) %>%
    mutate(sums = zoo::rollsumr(points, k=5, fill=NA),
           win_loss = as.factor(win_loss)) %>%
    arrange(desc(season), team) %>%
    ungroup() %>%
    mutate(Group = rep(1:(n()/38), each = 38))

win_streak_app = c(if_else(PL_modeling_combined_app[1,]$win_loss == 1, 1, 0))
wins = 0

for (row in 2:nrow(PL_modeling_combined_app)) {
    if (PL_modeling_combined_app[row,]$Group == PL_modeling_combined_app[row-1,]$Group) {
        if (PL_modeling_combined_app[row,]$points == 3) {
            wins = wins + 1
            win_streak_app = c(win_streak_app, wins)
        } else {
            wins = 0
            win_streak_app = c(win_streak_app, wins)
        }
    } else {
        wins = 0
        if (PL_modeling_combined_app[row,]$points == 3) {
            wins = wins + 1
            win_streak_app = c(win_streak_app, wins)
        } else {
            wins = 0
            win_streak_app = c(win_streak_app, wins)
        }
    }
}

PL_modeling_combined_app$win_streak <- win_streak_app

unbeaten_streak_app = c(if_else(PL_modeling_combined_app[1,]$win_loss == -1, 0, 1))
unbeaten = 0
for (row in 2:nrow(PL_modeling_combined_app)) {
    if (PL_modeling_combined_app[row,]$Group == PL_modeling_combined_app[row-1,]$Group) {
        if (PL_modeling_combined_app[row,]$points == 3 | PL_modeling_combined_app[row,]$points == 1) {
            unbeaten = unbeaten + 1
            unbeaten_streak_app = c(unbeaten_streak_app, unbeaten)
        } else {
            unbeaten = 0
            unbeaten_streak_app = c(unbeaten_streak_app, unbeaten)
        }
    } else {
        unbeaten = 0
        if (PL_modeling_combined_app[row,]$points == 3 | PL_modeling_combined_app[row,]$points == 1) {
            unbeaten = unbeaten + 1
            unbeaten_streak_app = c(unbeaten_streak_app, unbeaten)
        } else {
            unbeaten = 0
            unbeaten_streak_app = c(unbeaten_streak_app, unbeaten)
        }
    }
}

PL_modeling_combined_app$unbeaten_streak <- unbeaten_streak_app

PL_modeling_final_app <- PL_modeling_combined_app %>%
    arrange(desc(season), date) %>%
    dplyr::select(-full_time_results, -Group, -season, -points)


#=========================================================== Modeling ===========================================================#

# Commented out to deploy to server. If running in local, uncomment all code in this section. 

# Train/Test

# training_app <- PL_modeling_final_app %>%
#     filter(date < "2018-08-10") %>%
#     na.omit()
# 
# testing_app <- PL_modeling_final_app %>%
#     filter(date >= "2018-08-10") %>%
#     na.omit()
# 
# # Train Control & Grids
# set.seed(123)
# 
# train_control <- trainControl(method="cv",
#                               number=10,
#                               search = "grid")
# nb_grid <- expand.grid(
#     usekernel = c(TRUE, FALSE),
#     fL = 0:5,
#     adjust = seq(0, 5, by = 1)
# )
# 
# rf_grid_baseline <- expand.grid(mtry = c(1:(ncol(training_app) - 6)))
# 
# rf_grid_sums <- expand.grid(mtry = c(1:(ncol(training_app) - 5)))
# 
# rf_grid_win_streak <- expand.grid(mtry = c(1:(ncol(training_app) - 4)))
# 
# rf_grid_win_unbeaten_streak <- expand.grid(mtry = c(1:(ncol(training_app) - 3)))
# 
# svm_grid <- expand.grid(C = seq(3**-4, 3**3, by=3**(1/2)))
# 
# # Baseline
# 
# # NB
# 
# features <- setdiff(names(training_app), "win_loss")
# x <- training_app[, features]
# x_baseline <- x %>%
#     dplyr::select(-team, -date, -sums, -win_streak, -unbeaten_streak)
# y <- training_app$win_loss
# 
# nb_baseline <- train(
#     x = x_baseline,
#     y = y,
#     method = "nb",
#     trControl = train_control,
#     tuneGrid = nb_grid
# )
# 
# # RF
# 
# rf_baseline <- train(win_loss ~ .- team - date - sums - win_streak - unbeaten_streak,
#                      data = training_app,
#                      method = "rf",
#                      metric = "Accuracy",
#                      trControl = train_control,
#                      tuneGrid = rf_grid_baseline)
# 
# # SVM
# set.seed(123)
# svm_baseline <- train(
#     win_loss ~ .- team - date - sums - win_streak - unbeaten_streak,
#     data = training_app,
#     method = "svmLinear",
#     trControl = train_control,
#     tuneGrid = svm_grid,
#     preProcess = c("center","scale")
# )
# 
# # With Sums
# 
# # NB
# x_sums <- x %>%
#     dplyr::select(-team, -date, -win_streak, -unbeaten_streak)
# 
# nb_sums <- train(
#     x = x_sums,
#     y = y,
#     method = "nb",
#     trControl = train_control,
#     tuneGrid = nb_grid
# )
# 
# # RF
# 
# rf_sums <- train(win_loss ~ .- team - date - win_streak - unbeaten_streak,
#                  data = training_app,
#                  method = "rf",
#                  metric = "Accuracy",
#                  trControl = train_control,
#                  tuneGrid = rf_grid_sums)
# 
# # SVM
# 
# svm_sums <- train(
#     win_loss ~ .- team - date - win_streak - unbeaten_streak,
#     data = training_app,
#     method = "svmLinear",
#     trControl = train_control,
#     tuneGrid = svm_grid,
#     preProcess = c("center","scale")
# )
# 
# # With Sums + Win Streak
# 
# # NB
# 
# x_win_streak <- x %>%
#     dplyr::select(-team, -date, -unbeaten_streak)
# 
# nb_win_streak <- train(
#     x = x_win_streak,
#     y = y,
#     method = "nb",
#     trControl = train_control,
#     tuneGrid = nb_grid
# )
# 
# # RF
# 
# rf_win_streak <- train(win_loss ~ .- team - date - unbeaten_streak,
#                        data = training_app,
#                        method = "rf",
#                        metric = "Accuracy",
#                        trControl = train_control,
#                        tuneGrid = rf_grid_win_streak)
# 
# # SVM
# 
# svm_win_streak <- train(
#     win_loss ~ .- team - date - unbeaten_streak,
#     data = training_app,
#     method = "svmLinear",
#     trControl = train_control,
#     tuneGrid = svm_grid,
#     preProcess = c("center","scale")
# )
# 
# # With Sums, Win Streak, Unbeaten Streak
# 
# # NB
# 
# x_win_unbeaten_streak <- x %>%
#     dplyr::select(-team, -date)
# 
# nb_win_unbeaten_streak <- train(
#     x = x_win_unbeaten_streak,
#     y = y,
#     method = "nb",
#     trControl = train_control,
#     tuneGrid = nb_grid
# )
# 
# # RF
# 
# rf_win_unbeaten_streak <- train(win_loss ~ .- team - date,
#                                 data = training_app,
#                                 method = "rf",
#                                 metric = "Accuracy",
#                                 trControl = train_control,
#                                 tuneGrid = rf_grid_win_unbeaten_streak)
# 
# # SVM
# 
# svm_win_unbeaten_streak <- train(
#     win_loss ~ .- team - date,
#     data = training_app,
#     method = "svmLinear",
#     trControl = train_control,
#     tuneGrid = svm_grid,
#     preProcess = c("center","scale")
# )


#=========================================================== Shiny Dashboard ===========================================================#

ui = dashboardPage(
    dashboardHeader(title = "Premier League Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "intro",  icon = icon("home")),
            menuItem("Data", tabName = "data", icon = icon("table")) ,
            menuItem("Visualizations", tabName = "vis", icon = icon("chart-line")),
            menuItem("Models", tabName = "prediction", icon = icon("cogs"))
        )
    ),
    
    dashboardBody(
        tabItems(
            
            #=========================================================== Intro Tab ===========================================================#
            tabItem(tabName = "intro", 
                    fluidPage(
                        box(title = "Introduction",  
                            solidHeader = TRUE,
                            status = "primary",
                            width = 1000,
                            htmlOutput("introText")
                        ), br(), 
                        htmlOutput("picture")
                        
                       # tags$img(src="PL-logo.pgg")
                         #tags$img(src="PL-logo.png")
                        #tags$img(HTML('<img src="PL-logo.png" height="400px" style="float:right"/>'))
                       # imageOutput("image")
                      # div(img(src = "PL-Logo.png", align = "center", height = 300), style="text-align: center;")
                       
                       
                        
                        
                    ),
                    #img(src='PL-Logo.png', align = "right")
                    #img(src="PL-logo.png", width=100, align = "center")
                    #HTML(div(img(src = "PL-logo.png", align = "center", width = "75%"), style="text-align: center;"))
            ),
            
            #=========================================================== Data Tab ===========================================================#
            tabItem(tabName = "data",
                    fluidPage(
                        box(title = "Datasets", status = "primary", solidHeader = TRUE, width = "100%",
                            box(title = "Raw Data", status = "primary", width = "100%",
                                htmlOutput("raw_data_text"),
                                br(),
                                DTOutput("raw_data")
                            ),
                            box(title = "Cumulative Points", status = "primary", 
                                DTOutput("team_points_data")
                            ),
                            box(title = "Cumulative Goals Scored", status = "primary", 
                                DTOutput("team_goals_data")
                            )
                        )
                    )
            ),
            #=========================================================== Plots Tab ===========================================================#
            tabItem(tabName = "vis", 
                    fluidPage(
                        box(title = "Visualizations", 
                            status = "primary",
                            solidHeader = TRUE,
                            width = "100%",
                            box(title = "League Standings",
                                status = "primary",
                                selectizeInput("points_season",
                                               label = h4("Select Season"),
                                               choices = unique(team_points_app$Season),
                                               selected = 1
                                ),
                                checkboxInput("points_change_team", "View Specific Teams", value = FALSE),
                                conditionalPanel(
                                    "input.points_change_team == true",
                                    htmlOutput("points_teamSelector")
                                    
                                ),
                                
                                br(),
                                br(), 
                                
                                plotOutput("points_plot"),
                                # https://stackoverflow.com/questions/24652658/suppress-warning-message-in-r-console-of-shiny
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden; }",
                                           ".shiny-output-error:before { visibility: hidden; }"
                                )
                            ),
                            
                            box(title = "Goals Scored", 
                                status = "primary",
                                selectizeInput("goal_season",
                                               label = h4("Select Season"),
                                               choices = unique(team_points_app$Season),
                                               selected = 1
                                ),
                                checkboxInput("goal_change_team", "View Specific Teams", value = FALSE),
                                conditionalPanel(
                                    "input.goal_change_team == true",
                                    htmlOutput("goals_teamSelector")
                                    
                                ),
                                
                                br(),
                                br(),
                                
                                plotOutput("goals_plot"),
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden; }",
                                           ".shiny-output-error:before { visibility: hidden; }"
                                )
                            )
                        )
                    )
            ),
            #=========================================================== Models Tab ===========================================================#
            tabItem(tabName = "prediction", 
                    fluidPage(
                        box(title = "Models",
                            status = "primary",
                            solidHeader = TRUE,
                            width = "100%",
                            box(title = "Summary of Results", 
                                width = "100%", status = "primary", 
                                htmlOutput("generalize_models")
                            ),
                            selectizeInput("model_type",
                                           label = h4("Select Model"),
                                           choices = c("Random Forest", "SVM", "Naive Bayes"),
                                           selected = 1), 
                            box(title = "Baseline Models",
                                status = "primary",
                                htmlOutput("baseline_text"),
                                br(),
                                tabBox(width = "100%",
                                       id = "tabset1", 
                                       tabPanel("Model Output",
                                                verbatimTextOutput("baseline_output")),
                                       tabPanel("Diagnostic Plot - Cross Validation",
                                                plotOutput("diag_plot_cv_baseline")),
                                       tabPanel("Confusion Matrix",
                                                htmlOutput("baseline_cm"))
                                )
                            ),
                            box(title = "Models with Past Performance",
                                status = "primary",
                                htmlOutput("sums_text"),
                                br(),
                                tabBox(width = "100%",
                                       id = "tabset2", 
                                       tabPanel("Model Output",
                                                verbatimTextOutput("sum_output")),
                                       tabPanel("Diagnostic Plot - Cross Validation",
                                                plotOutput("diag_plot_cv_sum")),
                                       tabPanel("Confusion Matrix",
                                                htmlOutput("sums_cm"))
                                )
                            ),
                            box(title = "Models with Past Performance and Win Streak",
                                status = "primary",
                                htmlOutput("win_streak_text"),
                                br(),
                                tabBox(width = "100%",
                                       id = "tabset3", 
                                       tabPanel("Model Output",
                                                verbatimTextOutput("win_streak_output")),
                                       tabPanel("Diagnostic Plot - Cross Validation",
                                                plotOutput("diag_plot_cv_win_streak")),
                                       tabPanel("Confusion Matrix",
                                                htmlOutput("win_streak_cm"))
                                )
                            ),
                            box(title = "Models with Past Performance, Win Streak and Unbeaten Streak",
                                status = "primary",
                                htmlOutput("unbeaten_streak_text"),
                                br(),
                                tabBox(width = "100%",
                                       id = "tabset4", 
                                       tabPanel("Model Output",
                                                verbatimTextOutput("win_unbeaten_streak_output")),
                                       tabPanel("Diagnostic Plot - Cross Validation",
                                                plotOutput("diag_plot_cv_win_unbeaten_streak")),
                                       tabPanel("Confusion Matrix",
                                                htmlOutput("win_unbeaten_streak_cm"))
                                )
                            )
                            
                        )
                    )
            )
        )
    )
)

#=========================================================== Server ===========================================================#

server = function(input, output) {
    src = "PL-Logo.jpg"
    output$picture<-renderText({c('<img src="',src,'">')})
    output$image <- renderImage({
        list(
            src = "www/PL-Logo.png",
            contentType = "image/png",
            alt = "PL"
        )
    })
    
    #=========================================================== Intro Tab ===========================================================#
    
    output$introText <- renderUI({
        HTML("<p>The purpose of this project is two-fold: 1. to gain more exposure to Shiny and 2. to apply machine learning models.
                 The purpose of this project is two-fold: 1. to gain more exposure to Shiny and 2. to apply machine learning models. 
                I have always enjoyed watching soccer and am a huge 
                <a href='https://www.youtube.com/watch?v=zIlwMVkeJ5E' target = '_blank' style = 'color:#C8102E'> Liverpool </a>
                fan, so this project uses data from the 
                <a href = 'https://www.premierleague.com' target = '_blank' style = 'color: #41057e'> English Premier League</a>.</p>
                
                <p>I want to highlight two tabs: <b> Visualizations </b> and <b> Models</b>. The <b> Visualizations </b> tab includes two dynamic plots, with the option of changing to a specific season
            and teams the user is interested in. The <b> Models </b> tab includes three models (more in the future) that seek to classify match outcomes (win, draw, loss).
            As the game is extremely unpredictable and thus exciting, I wanted to see if matches can be classified using features relevant to
            current and past performance.</p>"
             
        )
    })
    
    #=========================================================== Data Tab ===========================================================#
    
    output$raw_data_text <- renderUI({
        HTML("<p>The source of the original data is from
                <a href='https://datahub.io/sports-data/english-premier-league' target = '_blank'> Datahub.io</a>, which includes data from the
                 2010-2011 to the 2018-2019 seasons. I scraped each season's data using <i>jsonlite</i> and then combined them into one data frame.<p/>")
    })
    
    output$raw_data <- renderDT({
        PL_data_app
    },
    selection = "single",
    rownames = FALSE,
    options = list(scrollX = TRUE)
    )
    
    output$team_goals_data <- renderDT({
        all_goals_app_shiny
    },
    selection = "single",
    rownames = FALSE,
    options = list(scrollX = TRUE))
    
    output$team_points_data <- renderDT({
        team_points_app_shiny
    },
    selection = "single",
    rownames = FALSE,
    options = list(scrollX = TRUE))
    
    #=========================================================== Plots Tab ===========================================================#
    
    # https://sites.temple.edu/psmgis/2017/07/26/r-shiny-task-create-an-input-select-box-that-is-dependent-on-a-previous-input-choice/
    
    # making choice of teams dependent on chosen season 
    
    output$goals_teamSelector <- renderImage({
        
        data_available = team_points_app[team_points_app$Season == input$goal_season, "Team"]
        selectInput(inputId = "goals_Team", 
                    label = h4("Select Teams"), 
                    choices = sort(unique(data_available)$Team), 
                    multiple = TRUE)
    })
    
    
    output$points_teamSelector <- renderUI({
        
        data_available = team_points_app[team_points_app$Season == input$points_season, "Team"]
        selectInput(inputId = "points_Team", 
                    label = h4("Select Teams"), 
                    choices = sort(unique(data_available)$Team), 
                    multiple = TRUE)
    })
    
    #=========================================================== Standings Plot ===========================================================#
    
    output$points_plot <- renderPlot({
        
        one_season <- team_points_app %>%
            filter(Season == input$points_season) 
        
        top6teams_points <- one_season %>%
            group_by(Team) %>%
            filter(cumpoints == max(cumpoints) & final_id == max(final_id)) %>%
            arrange(desc(cumpoints))
        
        score_cutoff_points <- top6teams_points$cumpoints[6]
        lower_cutoff_points <- top6teams_points$cumpoints[18]
        
        
        one_season$Team <- reorder(one_season$Team, -one_season$cumpoints)
        teams = unique(as.character(one_season$Team))
        
        one_season <- one_season %>%
            ungroup() %>%
            mutate(Team=fct_relevel(Team, levels = teams),
                   label = if_else((final_id == max(final_id) & cumpoints >= score_cutoff_points) | 
                                       (final_id == max(final_id) & cumpoints <= lower_cutoff_points), 
                                   paste0(as.character(Team),": ", cumpoints), 
                                   "")) 
        g <- one_season %>%
            ggplot(aes(x = Date, y = cumpoints, color = Team, group = Team)) +
            geom_line() +
            theme_minimal() + 
            labs(title = paste("Premier League Standings from", input$points_season, "Season"),
                 x="Date",
                 y = "Cumulative Points",
                 caption = paste("Champion:", as.character(one_season[one_season$cumpoints == max(one_season$cumpoints), ]$Team))) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5),
                  plot.caption = element_text(face = "bold"),
                  legend.title = element_text(hjust = 0.5)) +
            scale_color_manual(values = col.pal) + 
            guides(col = guide_legend(ncol=2)) + 
            geom_label_repel(aes(label=as.character(label)),
                             show.legend = FALSE) +
            scale_x_date(date_labels = "%b '%y", 
                         date_breaks = "1 month")
        
        if (input$points_change_team) {
            one_team <- one_season %>%
                filter(Team %in% input$points_Team) %>%
                mutate(label = if_else(final_id == max(final_id), 
                                       paste0(as.character(Team),": ", cumpoints), 
                                       "")) 
            
            if (nrow(one_team) == 0) {
                g <- ggplot() + theme_void()
                
            } else { 
                vals <- paste(input$points_Team,  collapse=", ")
                g <- one_team %>% 
                    ggplot(aes(x = Date, y = cumpoints, color = Team, group = Team)) +
                    geom_line() +
                    theme_minimal() + 
                    labs(title = paste("Premier League Standings from", input$points_season, "Season"),
                         subtitle = paste("Teams:", vals),
                         x="Date",
                         y = "Cumulative Points",
                         caption = paste("Champion:", as.character(one_season[one_season$cumpoints == max(one_season$cumpoints), ]$Team))) +
                    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                          plot.subtitle = element_text(hjust = 0.5),
                          plot.caption = element_text(face = "bold"),
                          legend.title = element_text(hjust = 0.5)) +
                    guides(col = guide_legend(ncol=2)) + 
                    scale_color_manual(values = col.pal) +
                    geom_label_repel(aes(label=as.character(label)),
                                     show.legend = FALSE) +
                    scale_x_date(date_labels = "%b '%y", 
                                 date_breaks = "1 month")
            }
        }
        
        g
        
    })
    
    #=========================================================== Goals Scored Plot ===========================================================#
    
    output$goals_plot <- renderPlot({
        
        one_season <- all_goals_app %>%
            filter(Season == input$goal_season)
        
        # Creating Cutoff 
        
        top6teams_points <- one_season %>%
            group_by(Team) %>%
            filter(cumgoals == max(cumgoals) & final_id == max(final_id)) %>%
            arrange(desc(cumgoals))
        
        goals_cutoff <- top6teams_points$cumgoals[6]
        
        one_season$Team <- reorder(one_season$Team, -one_season$cumgoals)
        teams = unique(as.character(one_season$Team))
        
        one_season <- one_season %>%
            ungroup() %>%
            mutate(Team=fct_relevel(Team, levels = teams),
                   label = if_else(final_id == max(final_id) & cumgoals >= goals_cutoff,
                                   paste0(as.character(Team),": ", cumgoals),
                                   ""))
        
        # Actual Plot 
        
        g <- one_season %>%
            ggplot(aes(x = Date, y = cumgoals, color = Team, group = Team)) +
            geom_line() +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                  legend.title = element_text(hjust = 0.5)) +
            labs(title = paste("Goals Scored from", input$goal_season, "Season"),
                 x="Date",
                 y = "Cumulative Goals") +
            scale_color_manual(values = col.pal) +
            guides(col = guide_legend(ncol=2)) +
            geom_label_repel(aes(label=as.character(label)),
                             show.legend = FALSE) +
            scale_x_date(date_labels = "%b '%y",
                         date_breaks = "1 month")
        
        # Selecting Teams 
        
        if (input$goal_change_team) {
            one_team <- one_season %>%
                filter(Team %in% input$goals_Team) %>%
                mutate(label = if_else(final_id == max(final_id),
                                       paste0(as.character(Team),": ", cumgoals),
                                       ""))
            
            if (nrow(one_team) == 0) {
                g <- ggplot() + theme_void()
                
            } else {
                vals <- paste(input$goals_Team,  collapse=", ")
                g <- one_team %>%
                    ggplot(aes(x = Date, y = cumgoals, color = Team, group = Team)) +
                    geom_line() +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                          plot.subtitle = element_text(hjust = 0.5),
                          legend.title = element_text(hjust = 0.5)) +
                    labs(title = paste("Goals Scored from", input$goal_season, "Season"),
                         subtitle = paste("Teams:", vals),
                         x="Date",
                         y = "Cumulative Goals") +
                    guides(col = guide_legend(ncol=2)) +
                    scale_color_manual(values = col.pal) +
                    geom_label_repel(aes(label=as.character(label)),
                                     show.legend = FALSE) +
                    scale_x_date(date_labels = "%b '%y",
                                 date_breaks = "1 month")
            }
        }
        
        g
    })
    
    
    #=========================================================== Prediction/Models Tab ===========================================================#
    
    #=========================================================== Text ===========================================================#
    
    output$generalize_models <- renderUI({
        HTML("The classifiers have a rough time classifying <i>Draws</i> when only using features corresponding to current performance. 
             This is reasonable because the features corresponding to current performance reflect whether a team performs better or worse than another team. 
             Compared to the baseline model, the addition of a team's previous 5 performances increase the accuracy of the classifier by 0.07 - 0.1
             and the addition of a team's win streak further increases the accuracy of the classifier by another 0.18 - 0.2. However, both models still struggle to 
             predict <i>Draws</i>. The addition of a team's unbeaten streak solves this issue, as all classifiers have an accuracy rate of above 0.97.")
    })
    
    output$baseline_text <- renderUI({
        HTML("This is the baseline model, including only features pertinent to the corresponding game. It uses the following predictors: <i>shots</i>, <i>shots_on_target</i>, <i>fouls</i>,  <i>corners</i>, 
             <i>yellow_cards</i>, <i>red_cards</i> and <i>home_away</i>")
    })
    
    output$sums_text <- renderUI({
        HTML("This model factors in current and previous performance. It uses the following predictors:  <i>shots</i>, <i>shots_on_target</i>, <i>fouls</i>,  <i>corners</i>, 
             <i>yellow_cards</i>, <i>red_cards</i>, <i>home_away</i> and <i>sums</i>. <i>sums</i> is calculated by summing the number of points gained 
             from a team's previous four matches and the current match. For example, if the outcome of a team's first 5 games are W, W, D, L, W, that team's <i>sums</i> value would 10 points.
             The maximum value is 15 and the minimum value is 0.")
    })
    
    output$win_streak_text <- renderUI({
        HTML("This model factors in current and previous performance. It uses the following predictors:  <i>shots</i>, <i>shots_on_target</i>, <i>fouls</i>,  <i>corners</i>, 
             <i>yellow_cards</i>, <i>red_cards</i>, <i>home_away</i>, <i>sums</i> and <i>win_streak</i>. <i>win_streak</i> is a team's current win streak. 
             Once a team draws or loses a match, the number resets to 0. 
             This value resets every season." )
    })
    
    output$unbeaten_streak_text <- renderUI({
        HTML("This model factors in current and previous performance. It uses the following predictors:  <i>shots</i>, <i>shots_on_target</i>, <i>fouls</i>,  <i>corners</i>, 
             <i>yellow_cards</i>, <i>red_cards</i>, <i>home_away</i>, <i>sums</i>, <i>win_streak</i> and <i>unbeaten_streak</i>. <i>unbeaten_streak</i> is a team's 
             current unbeaten streak. Once a team loses a match, the number resets to 0. This value resets every season." ) 
    })
    
    #=========================================================== Model Output ===========================================================#
    
    print_tbl_txt <- function(prediction) {
        cm = table(prediction, testing_app$win_loss)
        rownames(cm) <- c("Predicted Loss", "Predicted Draw", "Predicted Win")
        colnames(cm) <- c("Actual Loss", "Actual Draw", "Actual Win")
        tbl <- cm %>%
            kable("html") %>%
            kable_styling(bootstrap_options = c("striped", "hover"))
        mcr <- paste("Accuracy :" , round(sum(diag(cm))/sum(cm), 4))
        
        #https://stackoverflow.com/questions/41466748/how-to-show-text-and-table-in-shiny
        
        HTML(paste(tbl, mcr, sep = "<br/>"))
    }
    
    # Baseline 
    
    output$baseline_output <- renderPrint({
        if (input$model_type == "Naive Bayes") {
            print(nb_baseline)
        } else if (input$model_type == "Random Forest"){
            print(rf_baseline)
        } else {
            print(svm_baseline)
        }
    })
    
    output$diag_plot_cv_baseline <- renderPlot({
        if (input$model_type == "Naive Bayes") {
            plot(nb_baseline)
        } else if (input$model_type == "Random Forest"){
            plot(rf_baseline)
        } else {
            plot(svm_baseline)
        }
    })
    
    output$baseline_cm <- renderPrint({
        if (input$model_type == "Naive Bayes") {
            prediction = predict(nb_baseline, testing_app)
            
            # function at beginning of server 
            print_tbl_txt(prediction)
        } else if (input$model_type == "Random Forest"){
            prediction = predict(rf_baseline, testing_app)
            print_tbl_txt(prediction)
        } else {
            prediction = predict(svm_baseline, testing_app)
            print_tbl_txt(prediction)
        }
    })
    
    # Past Performance 
    
    output$sum_output <- renderPrint({
        if (input$model_type == "Naive Bayes") {
            print(nb_sums)
        } else if (input$model_type == "Random Forest"){
            print(rf_sums)
        } else {
            print(svm_sums)
        }
    })
    output$diag_plot_cv_sum <- renderPlot({
        if (input$model_type == "Naive Bayes") {
            plot(nb_sums)
        } else if (input$model_type == "Random Forest"){
            plot(rf_sums)
        } else {
            plot(svm_sums)
        }
    })
    
    output$sums_cm <- renderPrint({
        if (input$model_type == "Naive Bayes") {
            prediction = predict(nb_sums, testing_app)
            print_tbl_txt(prediction)
        } else if (input$model_type == "Random Forest"){
            prediction = predict(rf_sums, testing_app)
            print_tbl_txt(prediction)
        } else {
            prediction = predict(svm_sums, testing_app)
            print_tbl_txt(prediction)
        }
    })
    
    # Past Performance & Win Streak 
    
    output$win_streak_output <- renderPrint({
        if (input$model_type == "Naive Bayes") {
            print(nb_win_streak)
        } else if (input$model_type == "Random Forest"){
            print(rf_win_streak)
        } else {
            print(svm_win_streak)
        }
    })
    output$diag_plot_cv_win_streak <- renderPlot({
        if (input$model_type == "Naive Bayes") {
            plot(nb_win_streak)
        } else if (input$model_type == "Random Forest"){
            plot(rf_win_streak)
        } else {
            plot(svm_win_streak)
        }
    })
    
    output$win_streak_cm <- renderPrint({
        if (input$model_type == "Naive Bayes") {
            prediction = predict(nb_win_streak, testing_app)
            print_tbl_txt(prediction)
        } else if (input$model_type == "Random Forest"){
            prediction = predict(rf_win_streak, testing_app)
            print_tbl_txt(prediction)
        } else {
            prediction = predict(svm_win_streak, testing_app)
            print_tbl_txt(prediction)
        }
    })
    
    # Past Performance & Win Streak & Unbeaten Streak
    
    output$win_unbeaten_streak_output <- renderPrint({
        if (input$model_type == "Naive Bayes") {
            print(nb_win_unbeaten_streak)
        } else if (input$model_type == "Random Forest"){
            print(rf_win_unbeaten_streak)
        } else {
            print(svm_win_unbeaten_streak)
        }
    })
    output$diag_plot_cv_win_unbeaten_streak <- renderPlot({
        if (input$model_type == "Naive Bayes") {
            plot(nb_win_unbeaten_streak)
        } else if (input$model_type == "Random Forest"){
            plot(rf_win_unbeaten_streak)
        } else {
            plot(svm_win_unbeaten_streak)
        }
    })
    
    output$win_unbeaten_streak_cm <- renderPrint({
        if (input$model_type == "Naive Bayes") {
            prediction = predict(nb_win_unbeaten_streak, testing_app)
            print_tbl_txt(prediction)
        } else if (input$model_type == "Random Forest"){
            prediction = predict(rf_win_unbeaten_streak, testing_app)
            print_tbl_txt(prediction)
        } else {
            prediction = predict(svm_win_unbeaten_streak, testing_app)
            print_tbl_txt(prediction)
        }
    })
    
}

shinyApp(ui = ui, server = server)



