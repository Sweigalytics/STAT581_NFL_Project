drop table if exists public.games_by_primary_passer;
create table if not exists public.games_by_primary_passer AS
-- Counting the pass plays by each player in a game.
with q as
(
	select p.game_id
		,p.season
		,p.posteam 
		,p.passer_player_id 
		,p.passer_player_name 
		,min(p.season) over (partition by p.passer_player_id) as player_first_season
		,max(p.season) over (partition by p.passer_player_id) as player_last_season
		,COUNT(1) as pass_plays
	from public."nflfastR_pbp" p
	where p.passer_player_id is not null
	group by p.game_id
		,p.season
		,p.posteam 
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
-- Counting the number of games where Jalen Hurts was the primary passer.
h as
(
	select COUNT(*) as hurts_games
	from r
	where r.passer_rank = 1
		and r.passer_player_id = '00-0036389'
)
select p.*
from 
(
	select r.passer_player_id
		,r.passer_player_name
		,r.player_first_season
		,r.player_last_season
		,COUNT(*) as primary_passing_games
	from r
	where r.passer_rank = 1
	group by r.passer_player_id
		,r.passer_player_name
		,r.player_first_season
) p
-- Use the INNER JOIN to only include primary passers who have at least as many games as Jalen Hurts.
inner join h
	on p.primary_passing_games >= h.hurts_games
order by p.primary_passing_Games DESC
;