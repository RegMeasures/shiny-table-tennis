library(dplyr)
library(shiny)
library(plotly)
library(DT)
library(zoo)

packageVersion('plotly')
options(shiny.sanitize.errors = TRUE)

ui <- fluidPage(
  titlePanel("NIWA Christchurch table tennis ranking system"),    
  
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "Singles ranking", 
      DTOutput("SinglesTable"),
      plotlyOutput("SinglesScorePlot", height = 500)
    ),
    tabPanel(
      "Record singles match", 
      fluidRow(
        column(
          width = 6, align="center",
          selectInput("S_Player1", "Player 1", choices="")
        ),
        column(
          width = 6, align="center",
          selectInput("S_Player2", "Player 2", choices="")
        )
      ),
      fluidRow(
        align="center",
        h4(textOutput("PredictedSinglesOdds"))
      ),
      fluidRow(
        column(
          width = 6, align="center",
          numericInput("S_Wins1", "Number of games won by Player 1", value = 0, min = 0, max = 10)
        ),
        column(
          width = 6, align="center",
          numericInput("S_Wins2", "Number of games won by Player 2", value = 0, min = 0, max = 10)
        )
      ),
      fluidRow(
        align="center",
        actionButton("EnterSinglesGame", "Submit result")
      ),
      fluidRow(
        h3("Recently recorded games"),
        DTOutput("RecentSinglesGames")
      )
    ),
    tabPanel(
      "Doubles ranking", 
      DTOutput("DoublesTable"),
      plotlyOutput("DoublesScorePlot", height = 500)
    ),
    tabPanel(
      "Record Doubles match" , 
      fluidRow(
        column(
          width = 3, align="center",
          selectInput("D_Player1", "Team 1 Player 1", choices="")
        ),
        column(
          width = 3, align="center",
          selectInput("D_Player2", "Team 1 Player 2", choices="")
        ),
        column(
          width = 3, align="center",
          selectInput("D_Player3", "Team 2 Player 1", choices="")
        ),
        column(
          width = 3, align="center",
          selectInput("D_Player4", "Team 2 Player 2", choices="")
        )
      ),
      fluidRow(
        align="center",
        h4(textOutput("PredictedDoublesOdds"))
      ),
      fluidRow(
        column(
          width = 6, align="center",
          numericInput("D_Wins1", "Number of games won by team 1", value = 0, min = 0, max = 10)
        ),
        column(
          width = 6, align="center",
          numericInput("D_Wins2", "Number of games won by team 2", value = 0, min = 0, max = 10)
        )
      ),
      fluidRow(
        align="center",
        actionButton("EnterDoublesGame", "Save match")
      ),
      fluidRow(
        h3("Previous Games"),
        DTOutput("RecentDoublesGames")
      )
    ),
    tabPanel(
      "About this app" ,
      h4("The ELO scoring system"),
      "Both singles and doubles ratings are calculated using the ",
      tags$a(href="https://en.wikipedia.org/wiki/Elo_rating_system", "Elo rating system"),
      ". This system gives each player a rating. The difference in rating between two players",
      "is used to calculate the expected probability of each player winning.",
      "The difference between the expected outcome, and the actual result,",
      "is then used to move points from one player to the other.",
      "The bigger the difference between the actual result and the expected result",
      "the greater the movement of points."
    )
  )
)
