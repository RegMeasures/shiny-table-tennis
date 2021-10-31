
InitialScore <- 1500.0
Kvalue <- 20.0

readSinglesData <- function(FileName) {
  # Read in the list of games from the CSV file
  Games <- read.csv(FileName, header=TRUE, colClasses=c("Date", "character", "character", "integer", "integer"))
  
  # Convert the player columns to factors using the same key across all player columns
  Players <- sort(unique(c(Games$Player_1, Games$Player_2)))
  Games$Player_1 <- factor(Games$Player_1, levels=Players)
  Games$Player_2 <- factor(Games$Player_2, levels=Players)
  
  return(list(Games=Games, Players=Players))
}

# Predict the likelihood that player 1 will win
# 
# ProbOf1Winning <- calculateOdds(Player1Score, Player2Score)
# 
calculateOdds <- function(Player1Score, Player2Score) {
  
  EloDiff <- Player1Score - Player2Score
  ProbOf1Winning <- 1 / (1 + 10^(-EloDiff/400))
  return(ProbOf1Winning)
}

calculateSinglesScores <- function(SinglesGames, SinglesPlayers) {
  
  # Initialise the scoring dataframe
  Scores <- data.frame(matrix(NA,    
                              nrow = nrow(SinglesGames) + 1,
                              ncol = length(SinglesPlayers)))
  colnames(Scores) <- SinglesPlayers
  
  # Loop over the list of games and update the scoring
  for (GameNo in 1:nrow(SinglesGames)) {
    Scores[GameNo+1,] <- Scores[GameNo,]
    
    Player1 <- as.integer(SinglesGames[GameNo, "Player_1"])
    Player2 <- as.integer(SinglesGames[GameNo, "Player_2"])
    
    Player1Score <- Scores[GameNo, Player1]
    Player2Score <- Scores[GameNo, Player2]
    
    if (is.na(Player1Score)) {Player1Score <- InitialScore}
    if (is.na(Player2Score)) {Player2Score <- InitialScore}
    
    ProbOf1Winning <- calculateOdds(Player1Score, Player2Score)
    
    if(SinglesGames[GameNo, "Score_1"] > SinglesGames[GameNo, "Score_2"]) {
      Player1Delta <- Kvalue * (1 - ProbOf1Winning)
    } else {
      Player1Delta <- - Kvalue * ProbOf1Winning
    }
    
    Scores[GameNo+1, Player1] <- Player1Score + Player1Delta
    Scores[GameNo+1, Player2] <- Player2Score - Player1Delta
  }
  
  # Add date column and make it the first column
  Scores$Date <- c(SinglesGames$Date[1], SinglesGames$Date)
  Scores <- Scores[, c(ncol(Scores), 1:ncol(Scores)-1)]
  
  # Delete the first (dummy) row
  Scores <- Scores[-1, ]
  
  return(Scores)
}

# Ranking <- scores2Rank(Scores)
scores2Rank <- function(Scores) {
  t(apply(-Scores[,-1], 1, rank, ties.method="min"))
}

SummaryTable <- function(Scores, Games) {
  LatestRanking <- t(tail(scores2Rank(Scores),1))
  Person <- rownames(LatestRanking)
  Position <- LatestRanking[,1]
  Score <- t(tail(Scores,1))[-1,]
  
  SummaryTable <- data.frame(Position, Person, Score)
                             
  Played <- table(Games$Player_1) + table(Games$Player_2)
  Won <- table(Games$Player_1[Games$Score_1>Games$Score_2]) + table(Games$Player_2[Games$Score_2>Games$Score_1])
  GameCount <- t(rbind(Played, Won))
  
  SummaryTable <- cbind(SummaryTable, GameCount)
  SummaryTable <- SummaryTable[order(SummaryTable$Position),]
  return(SummaryTable)
}


  