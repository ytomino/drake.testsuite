-- { dg-do run }
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
procedure maps_character_mapping is
	subtype C is Character;
	X : Ada.Strings.Maps.Character_Mapping;
	function M (Item : Wide_Wide_Character) return Wide_Wide_Character is
	begin
		case Item is
			when 'A' => return 'B';
			when 'B' => return 'A';
			when others => return Item;
		end case;
	end M;
	AIU : constant String := (
		C'Val (16#e3#), C'Val (16#81#), C'Val (16#82#),
		C'Val (16#e3#), C'Val (16#81#), C'Val (16#84#),
		C'Val (16#e3#), C'Val (16#81#), C'Val (16#86#));
begin
	X := Ada.Strings.Maps.To_Mapping ("ABC", "ABC");
	pragma Assert (Ada.Strings.Maps.Is_Identity (X));
	X := Ada.Strings.Maps.To_Mapping ("ABC", AIU);
	pragma Assert (Ada.Strings.Fixed.Translate ("ABCDE", X) = AIU & "DE");
	pragma Assert (Ada.Strings.Fixed.Translate ("ABC", M'Access) = "BAC");
	pragma Debug (Ada.Debug.Put ("OK"));
end maps_character_mapping;
