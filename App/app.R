library(tidyverse)
library(lubridate)
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
    arrange(desc(Season), Team, Date) 

top6teams_points_app <- team_points_app %>%
    group_by(Team, Season) %>%
    filter(cumpoints == max(cumpoints) & final_id == max(final_id)) %>%
    arrange(desc(Season), desc(cumpoints))

ui <- dashboardPage(
    dashboardHeader(title = "Premier League Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "intro",  icon = icon("home")),
            menuItem("Data", tabName = "data", icon = icon("table")) #,
            # menuItem("Plots", tabName = "plots", icon = icon("chart-line"))
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
                            box(title = "Goals", status = "primary", solidHeader = TRUE, width = "100%",
                                DTOutput("team_points"))
                        )
                    )
            )
        )
        
        
        # tabItem(tabName = "")
        
        
    )
)


server <- function(input, output) {
    output$introText <- renderText({
        text <- "Here is some intro text that I will "
        paste(text)
        
    })
    
    output$raw_data <- renderDT({
        PL_data_app
    },
    selection = "single",
    rownames = FALSE,
    options = list(scrollX = TRUE))
    
    output$team_points <- renderDT({
        team_points_app %>%
            rename("Cumulative Points" = cumpoints) %>%
            mutate(Outcome = case_when(
                FT_Points == 3 ~ "Win", 
                FT_Points == 1 ~ "Draw", 
                FT_Points == 0 ~ "Loss"
            )) %>%
            select(-FT_Points, -final_id) 
    },
    selection = "single",
    rownames = FALSE,
    options = list(scrollX = TRUE))
}

shinyApp(ui = ui, server = server)

