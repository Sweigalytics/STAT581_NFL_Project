-- Creating a table of Passer IDs and Game IDs for QBs that had at least as many primary games as Hurts.
drop table if exists public.primary_passer_games_hurts_minimum;
create table if not exists public.primary_passer_games_hurts_minimum AS
-- Counting the number of games where Jalen Hurts was the primary passer.
with h as
(
	select COUNT(*) as hurts_games
	from public.primary_passer_games
	where passer_player_id = '00-0036389'
),
--Finding players with their first season in the dataset in 1999, but were drafted prior.
--We want to only include QBs who started their NFL career in 1999 or after.
d as
(
	select distinct A.passer_player_id 
	from public.primary_passer_games A
	left join
	(
		select left(pfr_player_name,1) || '.' || split_part(pfr_player_name, ' ', 2) as draft_name
			,*
		from public.draft
		where position = 'QB'
	) D
		on replace(A.passer_player_name, ' ','') = D.draft_name
	where A.player_first_season = 1999
		and (D.season < 1999 or D.season is null)
)
select p.*
from public.primary_passer_games p
WHERE p.primary_passing_games >= (select hurts_games from h) -- Filter to only include primary passers who have at least as many games as Jalen Hurts.
	and p.passer_player_id not in (select passer_player_id from d) -- Remove players who started their career prior to 1999
order by p.passer_player_id, primary_game_number
;