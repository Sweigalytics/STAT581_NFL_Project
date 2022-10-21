--Select the first contract the player signed after playing the current number of games Hurts played as a primary passer.
drop table if exists public.primary_passer_stats_hurts_maximum_contracts;
create table if not exists public.Primary_Passer_Stats_Hurts_Maximum_Contracts AS
select *
from 
(
	select P.*
		--The number of days between the QB's first primary start to when they reached Hurts' number of games.
		,(P.hurts_game_date::date - P.first_primary_passing_game_date::date) as days_to_hurts_game 
		,C.*
		,RANK() over (partition by P.passer_player_id order by C.year_signed) as contract_rank
	from public.primary_passer_stats_hurts_maximum P
	LEFT JOIN
	(
		select 
			case
				when player = 'Josh Allen' then 'Jos.Allen'
				when player = 'Josh Freeman' then 'Jo.Freeman'
				when player = 'Tyrod Taylor' then 'Ty.Taylor'
				when player = 'Robert Griffin III' then 'R.Griffin III'
				else left(player,1) || '.' || split_part(player, ' ', 2)
				end as player_name
			,year_signed
			,years
			--There are some players who signed multiple contracts within a year, so we will average the numeric values.
			,AVG(inflated_value) as inflated_value
			,AVG(inflated_apy) as inflated_apy
			,AVG(inflated_guaranteed) as inflated_guaranteed
		from public.contracts
		where position = 'QB'
		group by case
				when player = 'Josh Allen' then 'Jos.Allen'
				when player = 'Josh Freeman' then 'Jo.Freeman'
				when player = 'Tyrod Taylor' then 'Ty.Taylor'
				when player = 'Robert Griffin III' then 'R.Griffin III'
				else left(player,1) || '.' || split_part(player, ' ', 2)
				end
			,year_signed
			,years
	) C
		on P.passer_player_name = C.player_name
		and P.hurts_game_year <= C.year_signed 
	where (C.player_name is not null or P.passer_player_id = '00-0036389')
) Z
where Z.contract_rank = 1
;


