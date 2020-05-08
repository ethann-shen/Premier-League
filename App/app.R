library(tidyverse)
library(lubridate)
library(ggrepel)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(DT)
library(knitr)
library(kableExtra)
library(kernlab)

# Data 

PL_data_app = read_csv("premier_league_data.csv") 

PL_data_app <- PL_data_app %>%
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

# Data for Goals Plot 

home_app <- PL_data_app %>%
    select(Date, HomeTeam, FTHG, Season) %>%
    rename(Team = HomeTeam,
           FTG = FTHG) %>%
    mutate(home_away = "Home")

away_app <- PL_data_app %>%
    select(Date, AwayTeam, FTAG, Season) %>%
    rename(Team = AwayTeam,
           FTG = FTAG)  %>%
    mutate(home_away = "Away")

all_goals_app <- rbind(home_app, away_app) %>%
    arrange(Date) %>%
    group_by(Team, Season) %>%
    mutate(cumgoals = cumsum(FTG),
           final_id = row_number()) %>%
    arrange(desc(Season), desc(final_id), desc(cumgoals))

# Data for Standings Plot

home_points_app <- PL_data_app %>%
    select(Date, HomeTeam, HT_Points, Season) %>%
    rename(Team = HomeTeam,
           FT_Points = HT_Points)

away_points_app <- PL_data_app %>%
    select(Date, AwayTeam, AT_Points, Season) %>%
    rename(Team = AwayTeam,
           FT_Points = AT_Points)

team_points_app <- rbind(home_points_app, away_points_app) %>%
    arrange(Date) %>%
    group_by(Team, Season) %>%
    mutate(cumpoints = cumsum(FT_Points)) %>%
    mutate(final_id = row_number()) %>%
    arrange(desc(Season), desc(cumpoints)) 

# Data to Display 

display_data <- merge(all_goals_app, team_points_app, by = c("Date", "Team", "Season", "final_id")) %>%
    arrange(desc(Season), Date, Team) %>%
    rename("Location" = home_away,
           "Full Time Goals" = FTG,
           "Cumulative Goals" = cumgoals,
           "Cumulative Points" = cumpoints)

# Goals 
all_goals_app_shiny <- display_data %>%
    select(Date, Team, "Full Time Goals", "Cumulative Goals", "Location", Season)


# Standings
team_points_app_shiny <- display_data %>%
    mutate(Outcome = case_when(
        FT_Points == 3 ~ "Win", 
        FT_Points == 1 ~ "Draw", 
        FT_Points == 0 ~ "Loss"
    ))  %>%
    select(Date, Team, Outcome, "Cumulative Points", "Location", Season) 


# Plot Colors 

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col.pal = c(col_vector[5:25])

# Data for Modeling 

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
    select(win_loss, everything())
home_modeling_app <- PL_modeling_app[grep(c("^[^A]"), cols)] %>%
    mutate(home_away = "Home") %>%
    mutate(win_loss = case_when(
        HT_Points == 3 ~ 1,
        HT_Points == 1 ~ 0,
        HT_Points == 0 ~ -1
    )) %>%
    select(win_loss, everything())

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
    select(-full_time_results, -Group, -season, -points)


#============================================ Modeling ============================================#

# Train/Test

training_app <- PL_modeling_final_app %>%
    filter(date < "2018-08-10") %>%
    na.omit()

testing_app <- PL_modeling_final_app %>%
    filter(date >= "2018-08-10") %>%
    na.omit()

# Train Control & Grids
set.seed(123)

train_control <- trainControl(method="cv", 
                              number=10, 
                              search = "grid")
nb_grid <- expand.grid(
    usekernel = c(TRUE, FALSE),
    fL = 0:5,
    adjust = seq(0, 5, by = 1)
)

rf_grid_baseline <- expand.grid(mtry = c(1:(ncol(training_app) - 6)))

rf_grid_sums <- expand.grid(mtry = c(1:(ncol(training_app) - 5)))

rf_grid_win_streak <- expand.grid(mtry = c(1:(ncol(training_app) - 4)))

svm_grid <- expand.grid(C = seq(3**-4, 3**3, by=3**(1/2)))

# Baseline

# NB 

features <- setdiff(names(training_app), "win_loss")
x <- training_app[, features]
x_baseline <- x %>%
    dplyr::select(-team, -date, -sums, -win_streak, -unbeaten_streak) 
y <- training_app$win_loss

nb_baseline <- train(
    x = x_baseline,
    y = y,
    method = "nb",
    trControl = train_control,
    tuneGrid = nb_grid
)

# RF

rf_baseline <- train(win_loss ~ .- team - date - sums - win_streak - unbeaten_streak,
                     data = training_app,
                     method = "rf", 
                     metric = "Accuracy", 
                     trControl = train_control,
                     tuneGrid = rf_grid_baseline)

# SVM 
set.seed(123)
svm_baseline <- train(
    win_loss ~ .- team - date - sums - win_streak - unbeaten_streak, 
    data = training_app, 
    method = "svmLinear",
    trControl = train_control,
    tuneGrid = svm_grid,
    preProcess = c("center","scale")
)

# With Sums

# NB 
x_sums <- x %>%
    dplyr::select(-team, -date, -win_streak, -unbeaten_streak) 

nb_sums <- train(
    x = x_sums,
    y = y,
    method = "nb",
    trControl = train_control,
    tuneGrid = nb_grid
)

# RF

rf_sums <- train(win_loss ~ .- team - date - win_streak - unbeaten_streak,
                 data = training_app,
                 method = "rf", 
                 metric = "Accuracy", 
                 trControl = train_control,
                 tuneGrid = rf_grid_sums)

# SVM 

svm_sums <- train(
    win_loss ~ .- team - date - win_streak - unbeaten_streak,
    data = training_app, 
    method = "svmLinear",
    trControl = train_control,
    tuneGrid = svm_grid,
    preProcess = c("center","scale")
)

# With Sums + Win Streak 

# NB 

x_win_streak <- x %>%
    dplyr::select(-team, -date, -unbeaten_streak) 

nb_win_streak <- train(
    x = x_win_streak,
    y = y,
    method = "nb",
    trControl = train_control,
    tuneGrid = nb_grid
)

# RF 

rf_win_streak <- train(win_loss ~ .- team - date - unbeaten_streak,
                       data = training_app,
                       method = "rf", 
                       metric = "Accuracy", 
                       trControl = train_control,
                       tuneGrid = rf_grid_win_streak)

# SVM 

svm_win_streak <- train(
    win_loss ~ .- team - date - unbeaten_streak,
    data = training_app, 
    method = "svmLinear",
    trControl = train_control,
    tuneGrid = svm_grid,
    preProcess = c("center","scale")
)

# App 

shinyApp(
    
    #============================================ UI ============================================#
    
    ui = dashboardPage(
        dashboardHeader(title = "Premier League Analysis"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Introduction", tabName = "intro",  icon = icon("home")),
                menuItem("Data", tabName = "data", icon = icon("table")) ,
                menuItem("Visualizations", tabName = "vis", icon = icon("chart-line")),
                menuItem("Prediction", tabName = "prediction", icon = icon("cogs"))
            )
        ),
        
        dashboardBody(
            tabItems(
                tabItem(tabName = "intro", 
                        fluidPage(
                            box(title = "Introduction",  
                                solidHeader = TRUE,
                                status = "primary",
                                width = 1000,
                                htmlOutput(outputId = "introText")
                            )
                        )
                ),
                
                tabItem(tabName = "data",
                        fluidRow(
                            box(title = "All Data", status = "primary", solidHeader = TRUE, width = "100%",
                                box(title = "Data", status = "primary", solidHeader = TRUE, width = "100%",
                                    DTOutput("raw_data")
                                ),
                                box(title = "Goals", status = "primary", solidHeader = TRUE,
                                    DTOutput("team_goals_data")
                                ),
                                box(title = "Points", status = "primary", solidHeader = TRUE,
                                    DTOutput("team_points_data")
                                )
                            )
                        )
                ),
                
                tabItem(tabName = "vis", 
                        fluidPage(
                            box(title = "Visualizations", 
                                status = "primary",
                                solidHeader = TRUE,
                                width = "100%",
                                box(title = "Goals Scored", 
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
                                    # https://stackoverflow.com/questions/24652658/suppress-warning-message-in-r-console-of-shiny
                                    tags$style(type="text/css",
                                               ".shiny-output-error { visibility: hidden; }",
                                               ".shiny-output-error:before { visibility: hidden; }"
                                    )
                                ),
                                
                                box(title = "League Standings",
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
                                )
                                
                            )
                        )
                ),
                tabItem(tabName = "prediction", 
                        fluidRow(
                            box(title = "Models",
                                status = "primary",
                                solidHeader = TRUE,
                                width = "100%",
                                selectizeInput("model_type",
                                               label = h4("Select Model"),
                                               choices = c("Naive Bayes", "Random Forest", "SVM"),
                                               selected = 1), 
                                
                                box(title = "Baseline Models",
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
                                box(title = "Models with Past Performance & Win Streak",
                                    tabBox(width = "100%",
                                           id = "tabset3", 
                                           tabPanel("Model Output",
                                                    verbatimTextOutput("win_streak_output")),
                                           tabPanel("Diagnostic Plot - Cross Validation",
                                                    plotOutput("diag_plot_cv_win_streak")),
                                           tabPanel("Confusion Matrix",
                                                    htmlOutput("win_streak_cm"))
                                    )
                                )
                                
                            )
                        )
                )
            )
            
            
            # tabItem(tabName = "")
            
            
        )
    ), 
    
    #============================================ Server ============================================#
    
    server = server <- function(input, output) {
        
        print_tbl_txt <- function(prediction, data) {
            cm = table(prediction, data$win_loss)
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
                print_tbl_txt(prediction, testing_app)
            } else if (input$model_type == "Random Forest"){
                prediction = predict(rf_baseline, testing_app)
                print_tbl_txt(prediction, testing_app)
            } else {
                prediction = predict(svm_baseline, testing_app)
                print_tbl_txt(prediction, testing_app)
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
                print_tbl_txt(prediction, testing_app)
            } else if (input$model_type == "Random Forest"){
                prediction = predict(rf_sums, testing_app)
                print_tbl_txt(prediction, testing_app)
            } else {
                prediction = predict(svm_sums, testing_app)
                print_tbl_txt(prediction, testing_app)
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
                print_tbl_txt(prediction, testing_app)
            } else if (input$model_type == "Random Forest"){
                prediction = predict(rf_win_streak, testing_app)
                print_tbl_txt(prediction, testing_app)
            } else {
                prediction = predict(svm_win_streak, testing_app)
                print_tbl_txt(prediction, testing_app)
            }
        })
        
        
        
        
        
        
        
        # Intro Tab 
        
        output$introText <- renderText({
            text <- "Here is some intro text that I will "
            paste(text)
        })
        
        # Data Tab 
        
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
        
        # Plots Tab 
        
        # https://sites.temple.edu/psmgis/2017/07/26/r-shiny-task-create-an-input-select-box-that-is-dependent-on-a-previous-input-choice/
        
        # making choice of teams dependent on chosen season 
        
        output$goals_teamSelector <- renderUI({
            
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
        
        # Goals Plot 
        
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
                     y = "Total Points") +
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
                             y = "Total Points") +
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
        
        output$points_plot <- renderPlot({
            
            one_season <- team_points_app %>%
                filter(Season == input$points_season) 
            
            top6teams_points <- one_season %>%
                group_by(Team) %>%
                filter(cumpoints == max(cumpoints) & final_id == max(final_id)) %>%
                arrange(desc(cumpoints))
            
            score_cutoff_points <- top6teams_points$cumpoints[6]
            
            one_season$Team <- reorder(one_season$Team, -one_season$cumpoints)
            teams = unique(as.character(one_season$Team))
            
            one_season <- one_season %>%
                ungroup() %>%
                mutate(Team=fct_relevel(Team, levels = teams),
                       label = if_else(final_id == max(final_id) & cumpoints >= score_cutoff_all, 
                                       paste0(as.character(Team),": ", cumpoints), 
                                       "")) 
            g <- one_season %>%
                ggplot(aes(x = Date, y = cumpoints, color = Team, group = Team)) +
                geom_line() +
                theme_minimal() + 
                labs(title = paste("Premier League Standings from", input$points_season, "Season"),
                     x="Date",
                     y = "Total Points") +
                theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                      plot.subtitle = element_text(hjust = 0.5),
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
                             y = "Total Points") +
                        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                              plot.subtitle = element_text(hjust = 0.5),
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
        
        
        
        
        
    }
)


