drop table if exists public.team_wins_point_differential;
create table if not exists public.team_wins_point_differential AS
with wins as 
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
)
select t.team
	,t.game_id
	,t.season
	,t.week
	,case when t.team = w.winning_team then 1 else 0 end as win
	,case when t.team = winning_team then w.point_differential else -w.point_differential end team_point_differential
	,row_number() over (partition by t.team order by t.game_id) as team_game_number
from team_games t
left join wins w 
	on t.game_id = w.game_id
;