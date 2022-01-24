library(dplyr)
library(shiny)
library(plotly)
library(DT)
library(zoo)

packageVersion('plotly')
options(shiny.sanitize.errors = TRUE)

source("rankingFunctions.R")

SinglesFName <- "SinglesResults.csv"
DoublesFName <- "DoublesResults.csv"
PlayersFName <- "Players.csv"

server = function(input,output,session){
  
  # Create some reactive variables to hold dynamic data
  InitialPlayerList <- readPlayerList(PlayersFName)
  Players <- reactiveVal(InitialPlayerList)
  SinglesGames <- reactiveVal(readSinglesData(SinglesFName, InitialPlayerList))
  DoublesGames <- reactiveVal(readDoublesData(DoublesFName, InitialPlayerList))
  
  observe({updateSelectInput(session, "S_Player1", choices = as.list(c("",Players())))})
  observe({updateSelectInput(session, "S_Player2", choices = as.list(c("",Players())))})
  observe({updateSelectInput(session, "D_Player1", choices = as.list(c("",Players())))})
  observe({updateSelectInput(session, "D_Player2", choices = as.list(c("",Players())))})
  observe({updateSelectInput(session, "D_Player3", choices = as.list(c("",Players())))})
  observe({updateSelectInput(session, "D_Player4", choices = as.list(c("",Players())))})
  
  # Derive some further useful parameters using reactive functions
  # SinglesScores <- calculateSinglesScores(SinglesData, Players)
  SinglesScores <- reactive({
    calculateSinglesScores(SinglesGames(), Players())
  })
  # DoublesScores <- calculateDoublesScores(DoublesData, Players)
  DoublesScores <- reactive({
    calculateDoublesScores(DoublesGames(), Players())
  })
  
  SinglesRankTable <- reactive({
    SinglesSummary(SinglesScores(), SinglesGames())
  })
  
  DoublesRankTable <- reactive({
    DoublesSummary(DoublesScores(), DoublesGames())
  })
  
  # Singles ranking tab
  output$SinglesTable <- renderDT(datatable(SinglesRankTable(), rownames=FALSE) %>%
                             formatRound(3, digits=0)
                           )
  output$SinglesScorePlot <- renderPlotly({
    NGames <- nrow(SinglesScores())
    fig <- plot_ly(x = 1:NGames)
    for (Player in SinglesRankTable()$Person) {
      # only display lines of ranked players
      if (!is.na(SinglesRankTable()[SinglesRankTable()$Person==Player, "Position"])) {
        Visibility = TRUE
      } else {
        Visibility = "legendonly"
      }
      
      fig <- add_lines(fig, y = SinglesScores()[, Player], 
                       name = Player,
                       hoverinfo="text",
                       text = sprintf(paste('<b>%s</b><br>',
                                            'Date: %s<br>',
                                            'Rating: %.0f', sep=''), 
                                      Player, SinglesScores()[, 'Date'], SinglesScores()[, Player]),
                       visible = Visibility
                       )
      # fig <- add_annotations(fig, text = Player, 
      #                        x = nrow(Scores()), y = tail(Scores()[, Player],1), 
      #                        ax = 50, ay = 0)
    }
    
    layout(fig,
           yaxis = list(title = 'Singles rating'),
           xaxis = list(title = 'Match number',
                        range = c(max(0,NGames-200), NGames)))
  })
  
  # Doubles ranking tab
  output$DoublesTable <- renderDT(datatable(DoublesRankTable(), rownames=FALSE) %>%
                                    formatRound(3, digits=0)
  )
  
  output$DoublesScorePlot <- renderPlotly({
    NGames <- nrow(DoublesScores())
    fig <- plot_ly(x = 1:NGames)
    for (Player in DoublesRankTable()$Person) {
      # only display lines of ranked players
      if (!is.na(DoublesRankTable()[DoublesRankTable()$Person==Player, "Position"])) {
        Visibility = TRUE
      } else {
        Visibility = "legendonly"
      }
      
      fig <- add_lines(fig, y = DoublesScores()[, Player], 
                       name = Player,
                       hoverinfo="text",
                       text = sprintf(paste('<b>%s</b><br>',
                                            'Date: %s<br>',
                                            'Rating: %.0f', sep=''), 
                                      Player, DoublesScores()[, 'Date'], DoublesScores()[, Player]),
                       visible = Visibility
      )
      # fig <- add_annotations(fig, text = Player, 
      #                        x = nrow(Scores()), y = tail(Scores()[, Player],1), 
      #                        ax = 50, ay = 0)
    }
    
    layout(fig,
           yaxis = list(title = 'Doubles rating'),
           xaxis = list(title = 'Match number',
                        range = c(max(0,NGames-200), NGames)))
  })
  
  # Add singles game tab
  S_ProbOf1Winning <- reactive({
    if (input$S_Player1=="") {
      Player1Score <- InitialScore
    }
    else {
      Player1Score <- tail(SinglesScores()[input$S_Player1],1)
    }
    
    if (input$S_Player2=="") {
      Player2Score <- InitialScore
    }
    else {
      Player2Score <- tail(SinglesScores()[input$S_Player2],1)
    }
    
    return(calculateOdds(Player1Score, Player2Score))
  })
  
  output$PredictedSinglesOdds <- renderText({ 
    paste("The predicted chance of player 1 winning is ", round(S_ProbOf1Winning()*100), "%")
  })
  
  observeEvent(input$EnterSinglesGame, {
    # NEED TO ADD SOME VALIDATION AND HANDLE NEW PLAYERS
    NewSinglesGame <- list(Date = Sys.Date(),
                           Player_1 = input$S_Player1,
                           Player_2 = input$S_Player2,
                           Wins_1 = input$S_Wins1,
                           Wins_2 = input$S_Wins2)
    SinglesGames(rbind(SinglesGames(), NewSinglesGame))
    write.csv(SinglesGames(), SinglesFName, row.names = FALSE)
    
    updateSelectInput(session, "S_Player1", selected="")
    updateSelectInput(session, "S_Player2", selected="")
    updateNumericInput(session, "S_Wins1", value=0)
    updateNumericInput(session, "S_Wins2", value=0)
  })
  
  output$RecentSinglesGames <- renderDT(datatable(SinglesGames()[nrow(SinglesGames()):1,], rownames=TRUE) %>% 
                                   formatDate(
                                     columns = 1, 
                                     method =  "toLocaleDateString"
                                   )
  )
  
  # Add doubles game tab
  D_ProbOf1Winning <- reactive({
    if (input$D_Player1=="") {
      Player1Score <- InitialScore
    }
    else {
      Player1Score <- tail(DoublesScores()[input$D_Player1],1)
    }
    
    if (input$D_Player2=="") {
      Player2Score <- InitialScore
    }
    else {
      Player2Score <- tail(DoublesScores()[input$D_Player2],1)
    }
    
    if (input$D_Player3=="") {
      Player3Score <- InitialScore
    }
    else {
      Player3Score <- tail(DoublesScores()[input$D_Player3],1)
    }
    
    if (input$D_Player4=="") {
      Player4Score <- InitialScore
    }
    else {
      Player4Score <- tail(DoublesScores()[input$D_Player4],1)
    }
    
    Team1Score <- Player1Score + Player2Score
    Team2Score <- Player3Score + Player4Score
    
    return(calculateOdds(Team1Score, Team2Score))
  })
  
  output$PredictedDoublesOdds <- renderText({ 
    paste("The predicted chance of team 1 winning is ", round(D_ProbOf1Winning()*100), "%")
  })
  
  observeEvent(input$EnterDoublesGame, {
    # NEED TO ADD SOME VALIDATION AND HANDLE NEW PLAYERS
    NewDoublesGame <- list(Date = Sys.Date(),
                    Player_1 = input$D_Player1,
                    Player_2 = input$D_Player2,
                    Player_3 = input$D_Player3,
                    Player_4 = input$D_Player4,
                    Wins_1_2 = input$D_Wins1,
                    Wins_3_4 = input$D_Wins2)
    DoublesGames(rbind(DoublesGames(), NewDoublesGame))
    write.csv(DoublesGames(), DoublesFName, row.names = FALSE)
    
    updateSelectInput(session, "D_Player1", selected="")
    updateSelectInput(session, "D_Player2", selected="")
    updateSelectInput(session, "D_Player3", selected="")
    updateSelectInput(session, "D_Player4", selected="")
    updateNumericInput(session, "D_Wins1", value=0)
    updateNumericInput(session, "D_Wins2", value=0)
  })
  
  output$RecentDoublesGames <- renderDT(datatable(DoublesGames()[nrow(DoublesGames()):1,], rownames=TRUE) %>% 
                                   formatDate(
                                     columns = 1, 
                                     method =  "toLocaleDateString"
                                   )
  )
} 
