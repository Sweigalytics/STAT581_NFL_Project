drop table if exists public.primary_passer_games;
create table if not exists public.primary_passer_games AS
with q as
(
	select p.game_id
		,p.season
		,p.week
		,MAX(p.posteam) as posteam -- to accommodate some missing values.
		,p.passer_player_id 
		,p.passer_player_name 
		,min(p.season) over (partition by p.passer_player_id) as player_first_season
		,max(p.season) over (partition by p.passer_player_id) as player_last_season
		,COUNT(1) as pass_plays
	from public."nflfastR_pbp" p
	where p.passer_player_id is not null
	group by p.game_id
		,p.season
		,p.week
		,p.passer_player_id 
		,p.passer_player_name
),
-- Ranking the passers by number of passes within their given team per each game.
r as
(
	select *
		,RANK() over (partition by game_id, posteam order by pass_plays DESC) as passer_rank
	from q
),
p as
(
	select *
		,RANK() over (partition by passer_player_id order by game_id) as primary_game_number
	from r
	where passer_rank = 1
	order by passer_player_id
		,game_id
)
select P.game_id 
	,P.season
	,P.week
	,l.current_team as posteam
	,P.passer_player_id 
	,MAX(P.passer_player_name) over (partition by P.passer_player_id) as passer_player_name
	,P.player_first_season
	,P.player_last_season
	,P.passer_rank
	,P.primary_game_number
	,MAX(P.primary_game_number) over (partition by P.passer_player_id) as primary_passing_games
	,min(P.game_id) over (partition by P.passer_player_id) as first_primary_passing_game
	,FIRST_VALUE(P.posteam) over (partition by P.passer_player_id order by P.primary_game_number) as first_primary_passing_team
from p
left join public.historical_team_mapping l
	on P.posteam = l.previous_team 
;