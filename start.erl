-module(start).
-export([install/1,init_tables/1]).
-export([add_character/2, add_party/1, add_to_party/2, find_by_spec/1,
		 kill_enemy/2, pay/2, add_enemy/3, find_prize/1]).

%% 

-record(character, {name,
				    specialisation,
				    left_hand,
				    right_hand,
				    armor}).
				   
-record(join_party, {party_name,
					 charr_name}).
				   
-record(party, {party_name,
				money}).

-record(enemy, {boss_name,
				prize,
				event_description}).
				
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
						{type,bag},
						{disc_copies, Nodes}]),
						
    mnesia:create_table(enemy,
                        [{attributes, record_info(fields, enemy)},
						{disc_copies, Nodes},
						{local_content, true}]).
	
	
add_character(Name, Specialisation) ->
	F = fun() ->
		mnesia:write(#character{name = Name,
							    specialisation = Specialisation,
							    left_hand = "Empty",
							    right_hand = "Empty",
							    armor = "None"})
	end,
	mnesia:activity(transaction, F).
	
	
add_party(Name) ->
	F = fun() ->
		mnesia:write(#party{party_name = Name,
							money = 0})
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

	
find_by_spec(Specialisation) ->
	Pattern = #character{ _ = '_', specialisation = Specialisation},
	F = fun() ->
		Res = mnesia:match_object(Pattern),
		[{Name, LeftHand, RightHand, Armor} || #character{name=Name, left_hand=LeftHand, right_hand=RightHand, armor=Armor} <- Res]
	end,
	mnesia:activity(transaction, F).
	
	
kill_enemy(Party, Name) ->
	pay(Party, find_prize(Name)),
	F = fun() -> mnesia:delete({enemy, Name}) end,
	mnesia:activity(transaction, F).
	
	
find_prize(Name) ->
	F = fun() -> mnesia:read({enemy, Name}) end,
	case mnesia:activity(transaction, F) of
		[] -> 0;
		[#enemy{prize = Money}] -> Money
	end.
	
	
pay(Party, Prize) ->
    F = fun() ->
        [P] = mnesia:read({party, Party}),
        Money = P#party.money,
        New = P#party{money = Money + Prize},
        mnesia:write(New)
    end,
    mnesia:transaction(F).
	
	
	
	
	