game_data <- function(data, columnName, game_type) {
	games = data.frame(data)
	games = games %>%
		rename(TeamName = columnName) %>%
		select(Date, TeamName, FTHG, FTAG, FTR)
	if(dim(games)[1]>0){
		games = games %>%
			mutate(Result = ifelse(FTR=="D", "T",ifelse (FTR==game_type, "W" , "L" )))
	}
	games
}

away_game_data = function(data) game_data(data,"AwayTeam",'A')
home_game_data = function(data) game_data(data,"HomeTeam",'H')