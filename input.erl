-module(input).
-export([add_chars/0,add_enemies1/0,add_enemies2/0]).

add_chars() ->
	start:add_character("Minsc","Ranger","Human"),
	start:add_character("Dyanheir","Invoker","Human"),
	start:add_character("Alora","Thief","Halfling"),
	start:add_character("Imoen","Thief","Human"),
	start:add_character("Khalid","Fighter","Half-Elf"),
	start:add_character("Kivan","Ranger","Elf"),
	start:add_character("Kagain","Fighter","Dwarf"),
	start:add_character("Coran","Thief","Elf"),
	start:add_character("Edwin","Conjurer","Human"),
	start:add_character("Xzar","Necromancer","Human"),

	start:add_party("Fast and Furious"),
	start:add_party("Freelancer"),

	start:add_to_party("Alora","Freelancer"),
	start:add_to_party("Minsc","Fast and Furious"),
	start:add_to_party("Edwin","Fast and Furious"),
	start:add_to_party("Imoen","Fast and Furious"),
	start:add_to_party("Dyanheir","Fast and Furious").
	
add_enemies1() ->
	start:add_enemy("Greater Wyvern", 100, "Scares small children in nearby village."),
	start:add_enemy("Doppleganger", 250, "Leads pack of ghouls and ettercaps."),
	start:add_enemy("Basilisk", 500, "Eats naughty children."),
	start:add_enemy("Smaug", 1000, "Destroys cities in his free time.").
	
add_enemies2() ->
	start:add_enemy("Small Wyvern", 50, "Steals food from fridges."),
	start:add_enemy("Ghoul", 150, "Nobody likes him since he plays peekaboo on city's graveyard."),
	start:add_enemy("Kitty", 3700, "Fearsome beast.").
