library(dplyr)
library(shiny)
library(plotly)
library(DT)
library(zoo)

packageVersion('plotly')
options(shiny.sanitize.errors = TRUE)

ui <- fluidPage(
  titlePanel(HTML("NIWA Christchurch table tennis <s>ranking</s> contact tracing system")),    
  
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
        actionButton("RecordDoublesMatchButton", "Save match")
      ),
      fluidRow(
        h3("Previous Games"),
        DTOutput("RecentDoublesGames")
      )
    ),
    tabPanel(
      "Player details",
      fluidRow(
        textInput("newPlayerName", "New player name"),
        actionButton("AddPlayerButton", "Add new player")
      )
    ),
    tabPanel(
      "About" ,
      h4("The ELO scoring system"),
      "Player ratings are calculated using the ",
      tags$a(href="https://en.wikipedia.org/wiki/Elo_rating_system", "Elo rating system"),
      ". This system gives each player a rating. The difference in rating between two players",
      "is used to calculate the expected probability of each player winning.",
      "The difference between the expected outcome, and the actual result,",
      "is then used to move points from one player to the other.",
      "The bigger the difference between the actual result and the expected result",
      "the greater the movement of points.",
      h4("Doubles"),
      "Players are still assigned an individual rating for doubles, but their ",
      "doubles rating is completely seperate from their singles rating. ",
      "In doubles games 'team ratings' are calculated by summing the doubles ",
      "ratings of each pair. These team ratings are used in the ELO scoring ",
      "system to calculate the expected outcome.",
      "Win/loss points are then assigned to both players on a team.",
      "For example, if a high rating player and low rating player team up ",
      "against two medium ranked players the total rating of each team will be",
      "close, so the win probability is close to 50% for both teams. ",
      "The players on the winning team will both have their ratings increased ",
      "by the same amount of points, and this same amount of points will also ",
      "be deducted from both of the players on the loosing team.",
      h4("Rankings"),
      "To be assigned a ranking players must have played a minimum of four ",
      "games in the last four weeks. This threshold is applied seperately for ",
      "doubles and singles ranking (i.e. to have a singles ranking you must ",
      "have played four or more singles games in the last four weeks).",
      "The purpose of this threshold is twofold, firstly it ensures the rating",
      "of new players gets a chance to adjust prior to them recieving a ",
      "ranking, and secondly it ensures that rankings cannot be maintained ",
      "without regular play (i.e. if you are no.1 you can't just retire!)"
    )
  )
)
