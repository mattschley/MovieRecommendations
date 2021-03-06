## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

genre_list <- c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(
            sidebarMenu(
              menuItem("System 2", tabName = "dashboard", icon = icon("dashboard")),
              menuItem("System 1", tabName = "System1", icon = icon("th"))
            )
          ),

          dashboardBody(
            tabItems(
              tabItem(
                tabName = "dashboard", includeCSS("css/movies.css"),
              fluidRow(
                  box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      div(class = "rateitems",
                          uiOutput('ratings')
                      )
                  )
                ),
              fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "Step 2: Discover movies you might like",
                    br(),
                    withBusyIndicatorUI(
                      actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results")
                  )
               )
          ),
          tabItem(
              tabName = "System1", includeCSS("css/movies.css"),
            fluidRow(
                  box(width = 12, title = "Step 1: Select a Genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      selectInput("genre", "Genre:", choices=genre_list),
                  ),
                ),
              fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "Step 2: Discover movies you might like",
                    br(),
                    withBusyIndicatorUI(
                      actionButton("btn1", "Click here to get your recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results2")
                  )
               )
          ) 
          )
          )
    )
) 