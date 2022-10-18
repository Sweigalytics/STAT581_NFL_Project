drop table if exists public.team_wins_point_differential;
create table if not exists public.team_wins_point_differential as
with h as -- Find the number of games with Hurts as primary passer to calculate team's win % prior to QB's start over those number of games.
(
	select COUNT(*) as hurts_games
	from public.primary_passer_games
	where passer_player_id = '00-0036389'
),
wins as 
(
	select game_id
			,case 
				when away_score > home_score then away_team 
				when home_score > away_score then home_team
				when home_score = away_score then 'TIE'
				end as winning_team
			,ABS(away_score - home_score) as point_differential
	from public.schedules
),
team_games as
(
	select distinct away_team as team
		,game_id
		,season
		,week
	from public.schedules
	where date(gameday) <= (current_date - 1)
	union
	select distinct home_team as team 
		,game_id
		,season
		,week
	from public.schedules
	where date(gameday) <= (current_date - 1)
),
team_wins_games as
(
	select l.current_team as team
		,Z.game_id
		,Z.season
		,Z.week
		,Z.win
		,Z.team_point_differential
		,row_number() over (partition by l.current_team order by z.game_id) as team_game_number
	from
	(
		select team
			,game_id
			,season
			,week
			,win
			,team_point_differential
		from public.schedules_1997_1998
		union ALL
		select t.team
			,t.game_id
			,t.season
			,t.week
			,case when t.team = w.winning_team then 1 else 0 end as win
			,case when t.team = winning_team then w.point_differential else -w.point_differential end team_point_differential
		from team_games t
		left join wins w 
			on t.game_id = w.game_id
	) Z
	left join public.historical_team_mapping l
		on Z.team = l.previous_team 
)
select T.*
	,(SUM(T.win) over (partition by T.team order by T.team_game_number rows (select hurts_games from h) preceding) - T.win) as prior_hurts_games_wins
	,((SUM(T.win) over (partition by T.team order by T.team_game_number rows (select hurts_games from h) preceding)) - T.win)::float / (select hurts_games from h) as prior_hurts_games_win_perc
	,((SUM(T.team_point_differential) over (partition by T.team order by T.team_game_number rows (select hurts_games from h) preceding)) - T.team_point_differential) as prior_hurts_point_differential
from team_wins_games T
order by T.team
	,T.team_game_number
;