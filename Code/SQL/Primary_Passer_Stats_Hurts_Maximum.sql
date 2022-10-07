--Normalizing all QB stats to only include each QB's first number of primary games that Hurts has played so far.
--For example, if Hurts played 24 games so far, then we only want to include Tom Brady's first 24 games in his career as a primary passer.
drop table if exists public.primary_passer_stats_hurts_maximum;
create table if not exists public.primary_passer_stats_hurts_maximum AS
with h as
(
	select COUNT(*) as hurts_games
	from public.primary_passer_games
	where passer_player_id = '00-0036389'
)
select G.passer_player_id
	,G.passer_player_name
	,G.player_first_season
	,G.player_last_season
	,G.primary_passing_games
	,P.pass_attempts
	,P.pass_successes
	,P.passing_yards
	,P.passing_fumbles_lost
	,(P.pass_successes)::float / P.pass_attempts  as passing_completion_percentage
	,(P.passing_yards / P.pass_attempts)::float as passing_yards_per_attempt
	,P.primary_passing_touchdowns
	,(P.primary_passing_touchdowns)::float / (select hurts_games from h)::float as primary_passing_tds_per_game
	,R.rush_attempts
	,R.rushing_yards
	,(R.rushing_yards / R.rush_attempts)::float as rushing_yards_per_attempt
	,R.primary_rushing_touchdowns
	,(R.primary_rushing_touchdowns)::float / (select hurts_games from h)::float as primary_rushing_tds_per_game
	,P.sacks
	,(P.sacks / (P.pass_attempts + R.rush_attempts))::float as sacks_per_play
	,P.passing_interceptions
	,(P.passing_interceptions / P.pass_attempts)::float as interceptions_per_attempt
	,P.passing_fumbles_lost + R.rushing_fumbles_lost as total_fumbles_lost
	,((P.passing_fumbles_lost + R.rushing_fumbles_lost) / (P.pass_attempts + R.rush_attempts))::float as fumbles_per_attempt
	,((P.passing_interceptions + P.passing_fumbles_lost + R.rushing_fumbles_lost) / (P.pass_attempts + R.rush_attempts))::float as turnovers_per_attempt
from
(
	select distinct passer_player_id 
		,passer_player_name 
		,player_first_season 
		,player_last_season 
		,primary_passing_games 
	from public.primary_passer_games
) G
left join -- Joining the passing stats
(
	select A.passer_player_id 
		,COUNT(case when A.sack = 0 then A.passer_player_id END) as pass_attempts
		,COUNT(A.passing_yards) as pass_successes
		,SUM(A.passing_yards) as passing_yards
		,SUM(A.sack) as sacks
		,SUM(case when (A.passer_player_id = A.fumbled_1_player_id) or (A.passer_player_id = A.fumbled_2_player_id) then A.fumble_lost else 0 END) as passing_fumbles_lost
		,SUM(A.interception) as passing_interceptions
		,COUNT(case when A.posteam = A.td_team and R.game_id is not null then A.td_player_id END) as primary_passing_touchdowns -- Need to include A.posteam = A.td_team, otherwise Pick 6's would be counted as TDs.
		,SUM(case when R.game_id is not null then A.passing_yards END) as primary_passing_yards
	from public."nflfastR_pbp" A
	inner join public.primary_passer_games_hurts_minimum r
		on A.game_id = r.game_id
		and A.passer_player_id  = r.passer_player_id
	where r.primary_game_number <= (select hurts_games from h)
	group by A.passer_player_id
) P
	on G.passer_player_id = P.passer_player_id
left join -- Joining the rushing stats
(
	select rusher_player_id
		,COUNT(rusher_player_id) as rush_attempts
		,SUM(rushing_yards) as rushing_yards
		,SUM(fumble_lost) as rushing_fumbles_lost
		,COUNT(case when A.posteam = A.td_team and R.game_id is not null then A.td_player_id END) as primary_rushing_touchdowns
		,SUM(case when R.game_id is not null then A.rushing_yards END) as primary_rushing_yards
	from public."nflfastR_pbp" A
	inner join public.primary_passer_games_hurts_minimum r
		on A.game_id = r.game_id
		and A.rusher_player_id  = r.passer_player_id
	where r.primary_game_number <= (select hurts_games from h)
	group by rusher_player_id
) R
	on G.passer_player_id = R.rusher_player_id
where (P.passer_player_id is not null or R.rusher_player_id is not NULL) -- Ensures we only include players in `public.primary_passer_games_hurts_minimum`
order by G.primary_passing_games DESC
;