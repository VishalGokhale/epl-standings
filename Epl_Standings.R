library(dplyr)

source("./helper-functions.R")
source("./home_away_functions.R")

EPL_Standings = function(standings_as_on, season){
	epl_data = get_relevant_data(standings_as_on, season)

	home_games = home_game_data(epl_data)
	away_games = away_game_data(epl_data)

	home_records = home_games %>%
		group_by(TeamName) %>%
		summarise(matches = n(), GS = sum(FTHG), GA = sum(FTAG)) %>%
		compile_record(home_games)

	away_records = away_games %>%
		group_by(TeamName) %>%
		summarise(matches = n(), GS = sum(FTAG), GA = sum(FTHG)) %>%
		compile_record(away_games)

	all_records = full_join(home_records, away_records, by = "TeamName", suffix=c(".home",".away"))
	all_records[is.na(all_records)] = 0
	all_records[all_records$record.home == 0, "record.home"]= "0-0-0"
	all_records[all_records$record.away == 0, "record.away"]= "0-0-0"

	all_games_by_team <- rbind(home_games, away_games) %>%
		group_by(TeamName)

	win_loss_ties_streak = all_games_by_team	%>%
		arrange(desc(Date), TeamName) %>%
		summarise(record = paste(Result, sep="", collapse = "")) %>%
		mutate(Streak = sapply(record, streak)) %>%
		select(TeamName, Streak)

	last_10_record = all_games_by_team %>%
		top_n(10, Date) %>%
		#arrange(TeamName, Date) %>%
		group_by(TeamName) %>%
		summarise(wins = sum(Result=="W") , losses = sum(Result=="L"), ties = sum(Result=="T")) %>%
		mutate(Last10 = paste(wins,losses,ties, sep="-")) %>%
		select(TeamName, Last10)

	standings = all_records %>%
		mutate(MatchesPlayed = (matches.home + matches.away),
			   Points = points.home + points.away,
			   PPM = Points/MatchesPlayed,
			   PtPct = Points/(3*MatchesPlayed),
			   wins = wins.home + wins.away,
			   losses = losses.home + losses.away,
			   ties = ties.home + ties.away,
			   HomeRec = record.home,
			   AwayRec = record.away,
			   Record = paste(wins, losses, ties, sep="-"),
			   GS = GS.home + GS.away,
			   GA = GA.home + GA.away,
			   GSM = GS/MatchesPlayed,
			   GAM = GA/MatchesPlayed
		) %>%
		arrange(desc(PPM), desc(wins), desc(GS) , GAM) %>%
		select(TeamName, Record, HomeRec, AwayRec, MatchesPlayed, Points, PPM, PtPct, GS, GSM, GA, GAM) %>%
		full_join(last_10_record, by = "TeamName") %>%
		full_join(win_loss_ties_streak, by = "TeamName")


	as.data.frame(standings)
}


EPL_Standings("10/30/2019","2018/19")
