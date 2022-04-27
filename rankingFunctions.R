
InitialScore <- 1500.0
Kvalue <- 30.0

readPlayerList <- function(PlayersFName) {
  # Read in list of players
  Players <- read.csv(PlayersFName, header=FALSE, colClasses = "character")
  Players <- sort(unique(Players[,1]))
}

readSinglesData <- function(SinglesFName, Players) {
  # Read in the list of games from the CSV file
  SinglesGames <- read.csv(SinglesFName, header=TRUE, colClasses=c("Date", "character", "character", "integer", "integer"))
  
  # Convert the player columns to factors using the same key across all player columns
  SinglesGames$Player_1 <- factor(SinglesGames$Player_1, levels=Players)
  SinglesGames$Player_2 <- factor(SinglesGames$Player_2, levels=Players)
  
  return(SinglesGames)
}

readDoublesData <- function(DoublesFName, Players) {
  # Read in the list of games from the CSV file
  DoublesGames <- read.csv(DoublesFName, header=TRUE, 
                           colClasses=c("Date", "character", "character", "character", "character", "integer", "integer"))
  
  # Convert the player columns to factors using the same key across all player columns
  DoublesGames$Player_1 <- factor(DoublesGames$Player_1, levels=Players)
  DoublesGames$Player_2 <- factor(DoublesGames$Player_2, levels=Players)
  DoublesGames$Player_3 <- factor(DoublesGames$Player_3, levels=Players)
  DoublesGames$Player_4 <- factor(DoublesGames$Player_4, levels=Players)
  
  return(DoublesGames)
}

# Predict the likelihood that player/team 1 will win
# 
# ProbOf1Winning <- calculateOdds(Player1Score, Player2Score)
# 
calculateOdds <- function(Score1, Score2) {
  
  EloDiff <- Score1 - Score2
  ProbOf1Winning <- 1 / (1 + 10^(-EloDiff/400))
  return(ProbOf1Winning)
}

calculateSinglesScores <- function(SinglesGames, Players) {
  
  # Initialise the scoring dataframe
  SinglesScores <- data.frame(matrix(NA,    
                                    nrow = nrow(SinglesGames) + 1,
                                    ncol = length(Players)))
  colnames(SinglesScores) <- Players
  
  # Loop over the list of games and update the scoring
  for (GameNo in 1:nrow(SinglesGames)) {
    SinglesScores[GameNo+1,] <- SinglesScores[GameNo,]
    
    Player1 <- as.integer(SinglesGames[GameNo, "Player_1"])
    Player2 <- as.integer(SinglesGames[GameNo, "Player_2"])
    
    if (is.na(SinglesScores[GameNo, Player1])) {SinglesScores[GameNo, Player1] <- InitialScore}
    if (is.na(SinglesScores[GameNo, Player2])) {SinglesScores[GameNo, Player2] <- InitialScore}
    
    Player1Score <- SinglesScores[GameNo, Player1]
    Player2Score <- SinglesScores[GameNo, Player2]
    
    ProbOf1Winning <- calculateOdds(Player1Score, Player2Score)
    
    Player1Delta <- Kvalue * ((1 - ProbOf1Winning) * SinglesGames[GameNo, "Wins_1"] 
                              - ProbOf1Winning * SinglesGames[GameNo, "Wins_2"])
    
    SinglesScores[GameNo+1, Player1] <- Player1Score + Player1Delta
    SinglesScores[GameNo+1, Player2] <- Player2Score - Player1Delta
  }
  
  # Add date column and make it the first column
  SinglesScores$Date <- c(SinglesGames$Date[1], SinglesGames$Date)
  SinglesScores <- SinglesScores[, c(ncol(SinglesScores), 1:ncol(SinglesScores)-1)]
  
  # Delete the first (dummy) row
  SinglesScores <- SinglesScores[-1, ]
  
  return(SinglesScores)
}


calculateDoublesScores <- function(DoublesGames, Players) {
  
  # Initialise the scoring dataframe
  DoublesScores <- data.frame(matrix(NA,    
                              nrow = nrow(DoublesGames) + 1,
                              ncol = length(Players)))
  colnames(DoublesScores) <- Players
  
  # Loop over the list of games and update the scoring
  for (GameNo in 1:nrow(DoublesGames)) {
    DoublesScores[GameNo+1,] <- DoublesScores[GameNo,]
    
    Player1 <- as.integer(DoublesGames[GameNo, "Player_1"])
    Player2 <- as.integer(DoublesGames[GameNo, "Player_2"])
    Player3 <- as.integer(DoublesGames[GameNo, "Player_3"])
    Player4 <- as.integer(DoublesGames[GameNo, "Player_4"])
    
    if (is.na(DoublesScores[GameNo, Player1])) {DoublesScores[GameNo, Player1] <- InitialScore}
    if (is.na(DoublesScores[GameNo, Player2])) {DoublesScores[GameNo, Player2] <- InitialScore}
    if (is.na(DoublesScores[GameNo, Player3])) {DoublesScores[GameNo, Player3] <- InitialScore}
    if (is.na(DoublesScores[GameNo, Player4])) {DoublesScores[GameNo, Player4] <- InitialScore}
    
    Player1Score <- DoublesScores[GameNo, Player1]
    Player2Score <- DoublesScores[GameNo, Player2]
    Player3Score <- DoublesScores[GameNo, Player3]
    Player4Score <- DoublesScores[GameNo, Player4]
    
    Team1Score <- Player1Score + Player2Score
    Team2Score <- Player3Score + Player4Score
    
    ProbOf1Winning <- calculateOdds(Team1Score, Team2Score)
    
    Team1Delta <- Kvalue * ((1 - ProbOf1Winning) * DoublesGames[GameNo, "Wins_1_2"] 
                            - ProbOf1Winning * DoublesGames[GameNo, "Wins_3_4"])
    
    DoublesScores[GameNo+1, Player1] <- Player1Score + Team1Delta
    DoublesScores[GameNo+1, Player2] <- Player2Score + Team1Delta
    DoublesScores[GameNo+1, Player3] <- Player3Score - Team1Delta
    DoublesScores[GameNo+1, Player4] <- Player4Score - Team1Delta
  }
  
  # Add date column and make it the first column
  DoublesScores$Date <- c(DoublesGames$Date[1], DoublesGames$Date)
  DoublesScores <- DoublesScores[, c(ncol(DoublesScores), 1:ncol(DoublesScores)-1)]
  
  # Delete the first (dummy) row
  DoublesScores <- DoublesScores[-1, ]
  
  return(DoublesScores)
}

# Ranking <- scores2Rank(Scores)
scores2Rank <- function(Scores) {
  t(apply(-Scores[,-1], 1, rank, ties.method="min"))
}

SinglesSummary <- function(SinglesScores, SinglesGames) {
  Player <- colnames(SinglesScores)[-1]
  Score <- as.numeric(SinglesScores[nrow(SinglesScores),-1])
  
  NPlayers <- length(Player)
  Won <- integer(NPlayers)
  Lost <- integer(NPlayers)
  RecentPlayed <- integer(NPlayers)
  for (PlayerNo in 1:NPlayers) {
    Won[PlayerNo] <- sum(SinglesGames[SinglesGames$Player_1 == Player[PlayerNo], "Wins_1"]) +
                     sum(SinglesGames[SinglesGames$Player_2 == Player[PlayerNo], "Wins_2"])
    Lost[PlayerNo] <- sum(SinglesGames[SinglesGames$Player_1 == Player[PlayerNo], "Wins_2"]) +
                      sum(SinglesGames[SinglesGames$Player_2 == Player[PlayerNo], "Wins_1"])
    RecentPlayed[PlayerNo] <- sum(SinglesGames[SinglesGames$Player_1 == Player[PlayerNo] & SinglesGames$Date > Sys.Date()-28, "Wins_1"]) +
                              sum(SinglesGames[SinglesGames$Player_2 == Player[PlayerNo] & SinglesGames$Date > Sys.Date()-28, "Wins_1"]) +
                              sum(SinglesGames[SinglesGames$Player_1 == Player[PlayerNo] & SinglesGames$Date > Sys.Date()-28, "Wins_2"]) +
                              sum(SinglesGames[SinglesGames$Player_2 == Player[PlayerNo] & SinglesGames$Date > Sys.Date()-28, "Wins_2"])
  }                           
  Played <- Won + Lost
  
  Position <- integer(NPlayers)
  Position[RecentPlayed>3] <- rank(-Score[RecentPlayed>3], ties.method="min")
  Position[RecentPlayed<=3] <- NA
  
  SummaryTable <- data.frame(Player, Position, Score, Played, Won, Lost, RecentPlayed)
  
  SummaryTable <- SummaryTable[order(SummaryTable$Position),]
  return(SummaryTable)
}

DoublesSummary <- function(DoublesScores, DoublesGames) {
  Player <- colnames(DoublesScores)[-1]
  Score <- as.numeric(DoublesScores[nrow(DoublesScores),-1])
  
  NPlayers <- length(Player)
  Won <- integer(NPlayers)
  Lost <- integer(NPlayers)
  RecentPlayed <- integer(NPlayers)
  for (PlayerNo in 1:NPlayers) {
    Won[PlayerNo] <- sum(DoublesGames[DoublesGames$Player_1 == Player[PlayerNo], "Wins_1_2"]) +
                     sum(DoublesGames[DoublesGames$Player_2 == Player[PlayerNo], "Wins_1_2"]) +
                     sum(DoublesGames[DoublesGames$Player_3 == Player[PlayerNo], "Wins_3_4"]) +
                     sum(DoublesGames[DoublesGames$Player_4 == Player[PlayerNo], "Wins_3_4"])
    Lost[PlayerNo] <- sum(DoublesGames[DoublesGames$Player_1 == Player[PlayerNo], "Wins_3_4"]) +
                      sum(DoublesGames[DoublesGames$Player_2 == Player[PlayerNo], "Wins_3_4"]) +
                      sum(DoublesGames[DoublesGames$Player_3 == Player[PlayerNo], "Wins_1_2"]) +
                      sum(DoublesGames[DoublesGames$Player_4 == Player[PlayerNo], "Wins_1_2"])
    RecentPlayed[PlayerNo] <- sum(DoublesGames[DoublesGames$Player_1 == Player[PlayerNo] & DoublesGames$Date > Sys.Date()-28, "Wins_1_2"]) +
                              sum(DoublesGames[DoublesGames$Player_2 == Player[PlayerNo] & DoublesGames$Date > Sys.Date()-28, "Wins_1_2"]) +
                              sum(DoublesGames[DoublesGames$Player_3 == Player[PlayerNo] & DoublesGames$Date > Sys.Date()-28, "Wins_1_2"]) +
                              sum(DoublesGames[DoublesGames$Player_4 == Player[PlayerNo] & DoublesGames$Date > Sys.Date()-28, "Wins_1_2"]) +
                              sum(DoublesGames[DoublesGames$Player_1 == Player[PlayerNo] & DoublesGames$Date > Sys.Date()-28, "Wins_3_4"]) +
                              sum(DoublesGames[DoublesGames$Player_2 == Player[PlayerNo] & DoublesGames$Date > Sys.Date()-28, "Wins_3_4"]) +
                              sum(DoublesGames[DoublesGames$Player_3 == Player[PlayerNo] & DoublesGames$Date > Sys.Date()-28, "Wins_3_4"]) +
                              sum(DoublesGames[DoublesGames$Player_4 == Player[PlayerNo] & DoublesGames$Date > Sys.Date()-28, "Wins_3_4"])
  }                           
  Played <- Won + Lost
  
  Position <- integer(NPlayers)
  Position[RecentPlayed>3] <- rank(-Score[RecentPlayed>3], ties.method="min")
  Position[RecentPlayed<=3] <- NA
  
  SummaryTable <- data.frame(Player, Position, Score, Played, Won, Lost, RecentPlayed)
  
  SummaryTable <- SummaryTable[order(SummaryTable$Position),]
  return(SummaryTable)
}
  