-module(input).
-export([add_chars/0,add_party/0]).

add_chars() ->
	start:add_character("Minsc","Ranger"),
	start:add_character("Dyanheir","Invoker"),
	start:add_character("Alora","Thief"),
	start:add_character("Imoen","Thief"),
	start:add_character("Khalid","Fighter"),
	start:add_character("Kivan","Ranger"),
	start:add_character("Kagain","Fighter"),
	start:add_character("Coran","Thief"),
	start:add_character("Edwin","Conjurer"),
	start:add_character("Xzar","Necromancer").

add_party() ->
	start:add_party("Fast and Furious"),
	start:add_party("DragonBall Team").
	
add_enemies() ->
	start:add_enemy("Greater Wyvern", 100, "Scares small children in nearby village."),
	start:add_enemy("Doppleganger", 250, "Leads pack of ghouls and ettercaps."),
	start:add_enemy("Basilisk", 500, "What an ugly monster!"),
	start:add_enemy("Smaug", 1000, "Destroys cities in his free time.").
	