argumentDateFormat = "%m/%d/%Y"
formatArgumentDate = function(givenDate) as.Date(givenDate, format=argumentDateFormat)

dataDateFormat = "%d/%m/%Y"
formatDataDate = function(givenDate) {
	formattedDate = as.Date(givenDate, format = dataDateFormat)
	formattedDate = four_digit_year(formattedDate)
	formattedDate
}

substrRight = function(x, n) substr(x, nchar(x)-n+1, nchar(x))
last2 = function (x) substrRight(x,2)
join = function (x,y) paste(x,y,sep="")

# computes the string needed to create the URL to load data
season_string = function(season) {
	seasons = unlist(strsplit(season,"/"))
	season_url = Reduce(join, Map(last2, seasons))
}

epl_data_for_season <- function (season){
	url = paste("http://www.football-data.co.uk/mmz4281/",season_string(season),"/E0.csv", sep="")
	read.csv(url)
}

# converts the year to a 4 digit year threshold is 1990 since the oldest data available is from 1993/94
four_digit_year <- function(given_date, threshold=1990){
	years_after_cent <- year(given_date) %% 100
	year(given_date) <- ifelse(years_after_cent > (threshold %% 100), 1900+years_after_cent, 2000+years_after_cent)
	given_date
}

# keeps only relevant columns from the raw data
keep_relevant_columns_and_rows =
	function(epl_data, given_date) subset(epl_data, Date <= given_date, select = c("Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR"))

#takes game data, and summary(matches played, goals scored, goals allowed) and compiles records and points
compile_record <- function(played_gs_ga, games) {
	wins = games %>% filter(Result=="W") %>% group_by(TeamName) %>% summarise(wins = n())
	losses = games %>% filter(Result=="L") %>% group_by(TeamName) %>% summarise(losses = n())
	ties = games %>% filter(Result=="T") %>% group_by(TeamName) %>% summarise(ties = n())
	records = full_join(wins, losses, by="TeamName") %>%
		full_join(ties, by="TeamName") %>%
		full_join(played_gs_ga, by="TeamName")
	records[is.na(records)] = 0
	records$points = records$wins*3+records$ties
	records$record = paste(records$wins, records$losses, records$ties, sep = "-")
	records
}


# Takes the record string (ex. WWWL) as input and returns streak (W3)
streak = function(record){
	chars <- unlist(strsplit(record, ""))
	streak_length = 1
	record_without_first_character <- chars[-1]
	first_character <- chars[1]
	for (c in record_without_first_character){
		if(c==first_character)
			streak_length = streak_length+1
		else
			break
	}
	Streak = paste(chars[1], streak_length, sep = "", collapse = "")
	Streak
}

#loads epl data for and season, checks that the given date is after the season start date and returns
# data from the season only upto the given date
get_relevant_data <- function(given_date, season) {
	given_date = formatArgumentDate(given_date)
	epl_data = epl_data_for_season(season)
	epl_data$Date = formatDataDate(epl_data$Date)
	if(given_date < min(epl_data$Date))
		stop("Please enter a date later than the season start date")
	epl_data = keep_relevant_columns_and_rows(epl_data, given_date)
	epl_data
}

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

#creates a copy of the game_data and computes wins-loss-tie Result for Away Team
away_game_data = function(data) game_data(data,"AwayTeam",'A')

#creates a copy of the game_data and computes wins-loss-tie Result for Home Team
home_game_data = function(data) game_data(data,"HomeTeam",'H')

clean_up <- function(all_records) {
	all_records[is.na(all_records)] = 0
	all_records[all_records$record.home == 0, "record.home"]= "0-0-0"
	all_records[all_records$record.away == 0, "record.away"]= "0-0-0"
	all_records
}



