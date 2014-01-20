-module(start).
-export([install/1,init_tables/1]).
-export([add_character/2, add_party/1, add_to_party/2, find_by_spec/1]).

%% 

-record(character, {name,
				    specialisation,
				    money,
				    left_hand,
				    right_hand,
				    armor}).
				   
-record(join_party, {party_name,
					 charr_name}).
				   
-record(party, {party_name}).

-record(enemy, {boss_name,
				prize,
				event_description}).
				
-record(join_event, {party_name,
					 boss_name}).
				
-record(item, {item_name,
			   item_type,
			   attribute,
			   special_ability}).

%%

install(Nodes) ->
	ok = mnesia:create_schema(Nodes),
	rpc:multicall(Nodes, application, start, [mnesia]),
	init_tables(Nodes).

	
init_tables(Nodes) ->
    mnesia:create_table(character,
                        [{attributes, record_info(fields, character)},
						{index, [#character.specialisation]},
						{disc_copies, Nodes}]),
						
    mnesia:create_table(party,
                        [{attributes, record_info(fields, party)},
						{disc_copies, Nodes}]),
						
    mnesia:create_table(join_party,
                        [{attributes, record_info(fields, join_party)},
						{disc_copies, Nodes}]),
						
    mnesia:create_table(enemy,
                        [{attributes, record_info(fields, enemy)},
						{disc_copies, Nodes},
						{local_content, true}]),
						
	mnesia:create_table(join_event,
                        [{type, bag}, 
						{attributes, record_info(fields, join_event)},
						{disc_copies, Nodes},
						{local_content, true}]).
	
add_character(Name, Specialisation) ->
	F = fun() ->
		mnesia:write(#character{name = Name,
							    specialisation = Specialisation,
							    money = 0,
							    left_hand = "Empty",
							    right_hand = "Empty",
							    armor = "None"})
	end,
	mnesia:activity(transaction, F).
	
add_party(Name) ->
	F = fun() ->
		mnesia:write(#party{party_name = Name})
	end,
	mnesia:activity(transaction, F).

	
add_to_party(Name,Party) ->
	F = fun() ->
		case mnesia:read({character, Name}) =:= [] orelse
		     mnesia:read({party, Party}) =:= [] of
		true ->
			{error, unknown_argument};
		false ->
			mnesia:write(#join_party{party_name = Party,
									 charr_name = Name})
		end
	end,
	mnesia:activity(transaction,F).	
	
add_enemy(BossName, Prize, Desc) ->
	F = fun() ->
		mnesia:write(#enemy{boss_name = BossName,
							prize = Prize,
							event_description = Desc})
	end,
	mnesia:activity(transaction, F).


add_party_to_event(Name,BossNaame) ->
	F = fun() ->
		case mnesia:read({party, Name}) =:= [] orelse
		     mnesia:read({enemy, BossName}) =:= [] of
		true ->
			{error, unknown_argument};
		false ->
			mnesia:write(#join_event{party_name = Name,
									 boss_name = BossName})
		end
	end,
	mnesia:activity(transaction,F).	

	
find_by_spec(Specialisation) ->
	Pattern = #character{ _ = '_', specialisation = Specialisation},
	F = fun() ->
		Res = mnesia:match_object(Pattern),
		[{Name, LeftHand, RightHand, Armor} || #character{name=Name, left_hand=LeftHand, right_hand=RightHand, armor=Armor} <- Res]
	end,
	mnesia:activity(transaction, F).
	
	
	
	
	
	
	