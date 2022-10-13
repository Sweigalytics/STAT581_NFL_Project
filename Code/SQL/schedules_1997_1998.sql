drop table if exists public.schedules_1997_1998;
create table if not exists public.schedules_1997_1998 AS
with u as 
(
	select 1997 as season
		,*
	from public.nfl_schedule_1997
	union ALL
	select 1998 as season
		,*
	from public.nfl_schedule_1998
),
A as ( 
	select w.current_team as team
		,w.current_team || '_' || l.current_team as teams_key
		,season
		,case 
			when week = 'WildCard' then 18
			when week = 'Division' then 19
			when week = 'ConfChamp' then 20
			when week = 'SuperBowl' then 21
			else week::integer
			end as week
		,1 as win -- This is the winning team
		,ptsw - ptsl as team_point_differential
	from u
	left join public.historical_team_mapping w
		on u."Winner/tie" = w.previous_team 
	left join public.historical_team_mapping l 
		on u."Loser/tie" = l.previous_team 
	union all 
	select l.current_team as team
		,w.current_team || '_' || l.current_team as teams_key
		,season
		,case 
			when week = 'WildCard' then 18
			when week = 'Division' then 19
			when week = 'ConfChamp' then 20
			when week = 'SuperBowl' then 21
			else week::integer
			end as week
		,0 as win -- This is the losing team
		,ptsl - ptsw  as team_point_differential
	from u
	left join public.historical_team_mapping w
		on u."Winner/tie" = w.previous_team 
	left join public.historical_team_mapping l 
		on u."Loser/tie" = l.previous_team
)
select team
	,season::varchar(4) || '_' || right('00' || week::varchar(2),2) || '_' || a.teams_key as game_id
	,season
	,week
	,win
	,team_point_differential
from A
;