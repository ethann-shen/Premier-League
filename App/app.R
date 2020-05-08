library(tidyverse)
library(lubridate)
library(ggrepel)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(DT)

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
    select(Date, Team, Outcome, "Cumulative Points", "Location", Season) %>%


# Plot Colors 

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col.pal = c(col_vector[5:25])

# App 

ui <- dashboardPage(
    
    dashboardHeader(title = "Premier League Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "intro",  icon = icon("home")),
            menuItem("Data", tabName = "data", icon = icon("table")) ,
            menuItem("Visualizations", tabName = "vis", icon = icon("chart-line"))
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
            )
        )
        
        
        # tabItem(tabName = "")
        
        
    )
)


server <- function(input, output) {
    
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

shinyApp(ui = ui, server = server)


