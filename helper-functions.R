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

season_string = function(season) {
	seasons = unlist(strsplit(season,"/"))
	season_url = Reduce(join, Map(last2, seasons))
}

epl_data_for_season <- function (season){
	url = paste("http://www.football-data.co.uk/mmz4281/",season_string(season),"/E0.csv", sep="")
	read.csv(url)
}

four_digit_year <- function(given_date, threshold=1990){
	years_after_cent <- year(given_date) %% 100
	year(given_date) <- ifelse(years_after_cent > (threshold %% 100), 1900+years_after_cent, 2000+years_after_cent)
	given_date
}





keep_relevant_columns_and_rows =
	function(epl_data, given_date) subset(epl_data, Date <= given_date, select = c("Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR"))

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

streak = function(record){
	chars <- unlist(strsplit(record, ""))
	streak_length = 1
	for (c in chars[-1]){
		if(c==chars[1])
			streak_length = streak_length+1
		else
			break
	}
	Streak = paste(chars[1], streak_length, sep = "", collapse = "")
	Streak
}


get_relevant_data <- function(given_date, season) {
	given_date = formatArgumentDate(given_date)
	epl_data = epl_data_for_season(season)
	epl_data$Date = formatDataDate(epl_data$Date)
	if(given_date < min(epl_data$Date))
		stop("Please enter a date later than the season start date")
	epl_data = keep_relevant_columns_and_rows(epl_data, given_date)
	epl_data
}







