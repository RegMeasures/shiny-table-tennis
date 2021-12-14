library(dplyr)
library(ggplot2)
library(shinyjs)
library(plotly)
library(DT)
library(htmltools)
library(zoo)

packageVersion('plotly')
options(shiny.sanitize.errors = TRUE)

source("rankingFunctions.R")

SinglesFName <- "SinglesResults.csv"
DoublesFName <- "DoublesResults.csv"
PlayersFName <- "Players.csv"

Players <- readPlayerList(PlayersFName)
SinglesData <- readSinglesData(SinglesFName, Players)
DoublesData <- readDoublesData(DoublesFName, Players)

shinyApp(
  ui = fluidPage(
    titlePanel("NIWA Christchurch table tennis ranking system"),    
    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Singles ranking", 
        DTOutput("SinglesTable"),
        plotlyOutput("SinglesScorePlot", height = 300)
      ),
      tabPanel(
        "Record singles game", 
        fluidRow(
          column(
            width = 6, align="center",
            selectInput("Player1", "Player 1", choices=as.list(c("",Players)))
          ),
          column(
            width = 6, align="center",
            selectInput("Player2", "Player 2", choices=as.list(c("",Players)))
          )
        ),
        fluidRow(
          align="center",
          h4(textOutput("PredictedOdds"))
        ),
        fluidRow(
          column(
            width = 6, align="center",
            numericInput("Score1", "Player 1 Score", value = 11, min = 0, max = 50)
          ),
          column(
            width = 6, align="center",
            numericInput("Score2", "Player 2 Score", value = 11, min = 0, max = 50)
          )
        ),
        fluidRow(
          align="center",
          actionButton("EnterSinglesGame", "Submit result")
        ),
        fluidRow(
          h3("Recently recorded games"),
          DTOutput("RecentGames")
        )
      ),
      tabPanel(
        "Doubles ranking"#, 
        # DTOutput("DoublesTable"),
        # plotlyOutput("DoublesScorePlot", height = 300)
      ),
      tabPanel(
        "Record Doubles game" #, 
        # fluidRow(
        #   column(
        #     width = 6, align="center",
        #     selectInput("Player1", "Player 1", choices=as.list(c("",SinglesData$Players)))
        #   ),
        #   column(
        #     width = 6, align="center",
        #     selectInput("Player2", "Player 2", choices=as.list(c("",SinglesData$Players)))
        #   )
        # ),
        # fluidRow(
        #   align="center",
        #   h4(textOutput("PredictedOdds"))
        # ),
        # fluidRow(
        #   column(
        #     width = 6, align="center",
        #     numericInput("Score1", "Player 1 Score", value = 11, min = 0, max = 50)
        #   ),
        #   column(
        #     width = 6, align="center",
        #     numericInput("Score2", "Player 2 Score", value = 11, min = 0, max = 50)
        #   )
        # ),
        # fluidRow(
        #   align="center",
        #   actionButton("EnterSinglesGame", "Submit result")
        # ),
        # fluidRow(
        #   h3("Previous Games"),
        #   DTOutput("RecentGames")
        # )
      )
    )
  ),

  server = function(input,output,session){
    
    # Create some reactive variables to hold dynamic data
    Players <- reactiveVal(Players)
    SinglesGames <- reactiveVal(SinglesData)
    DoublesGames <- reactiveVal(DoublesData)
    
    # Derive some further useful parameters using reactive functions
    # SinglesScores <- calculateSinglesScores(SinglesData, Players)
    SinglesScores <- reactive({
      calculateSinglesScores(SinglesGames(), Players())
    })
    # DoublesScores <- calculateDoublesScores(DoublesData, Players)
    DoublesScores <- reactive({
      calculateDoublesScores(DoublesGames(), Players())
    })
    
    RankTable <- reactive({
      SummaryTable(SinglesScores(), SinglesGames())
    })
    
    ## Create the ranking summary table
    output$SinglesTable <- renderDT(datatable(RankTable(), rownames=FALSE) %>%
                               formatRound(3, digits=0)
                             )
    
    output$RecentGames <- renderDT(datatable(SinglesGames()[nrow(SinglesGames()):1,], rownames=TRUE) %>% 
      formatDate(
        columns = 1, 
        method =  "toLocaleDateString"
      )
    )
    
    output$SinglesScorePlot <- renderPlotly({
      NGames <- nrow(SinglesScores())
      fig <- plot_ly(x = 1:NGames)
      for (Player in Players()) {
        fig <- add_lines(fig, y = SinglesScores()[, Player], 
                         name = Player,
                         hoverinfo="text",
                         text = SinglesScores()[, 'Date'],
                         hovertemplate = paste('<b>%{fullData.name}</b><br>',
                                               'Date: %{text}<br>',
                                               'Rating: %{y:.0f}<extra></extra>', sep='')
                         )
        # fig <- add_annotations(fig, text = Player, 
        #                        x = nrow(Scores()), y = tail(Scores()[, Player],1), 
        #                        ax = 50, ay = 0)
      }
      
      layout(fig,
             yaxis = list(title = 'Doubles rating'),
             xaxis = list(title = 'Game number',
                          range = c(max(0,NGames-50), NGames)))
    })
    
    ProbOf1Winning <- reactive({
      if (input$Player1=="") {
        Player1Score <- InitialScore
      }
      else {
        Player1Score <- tail(SinglesScores()[input$Player1],1)
      }
      
      if (input$Player2=="") {
        Player2Score <- InitialScore
      }
      else {
        Player2Score <- tail(SinglesScores()[input$Player2],1)
      }
      
      return(calculateOdds(Player1Score, Player2Score))
    })
    
    output$PredictedOdds <- renderText({ 
      paste("The predicted chance of player 1 winning is ", round(ProbOf1Winning()*100), "%")
    })
    
    observeEvent(input$EnterSinglesGame, {
      # NEED TO ADD SOME VALIDATION AND HANDLE NEW PLAYERS
      NewGame <- list(Date = Sys.Date(),
                      Player_1 = input$Player1,
                      Player_2 = input$Player2,
                      Score_1 = input$Score1,
                      Score_2 = input$Score2)
      SinglesGames(rbind(SinglesGames(), NewGame))
      write.csv(SinglesGames(), DataFile, row.names = FALSE)
    })
    
  }
)
