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
select game_id 
	,season
	,week
	,posteam
	,passer_player_id 
	,MAX(passer_player_name) over (partition by passer_player_id) as passer_player_name
	,player_first_season
	,player_last_season
	,passer_rank
	,primary_game_number
	,MAX(primary_game_number) over (partition by passer_player_id) as primary_passing_games
from p
;