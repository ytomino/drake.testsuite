-- { dg-do run }
with Ada.Characters.Latin_1;
with Ada.Wide_Characters.Latin_1;
with Ada.Wide_Wide_Characters.Latin_1;
with Ada.Containers;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Wide_Hash_Case_Insensitive;
with Ada.Strings.Wide_Wide_Hash_Case_Insensitive;
procedure hash_case_insensitive_utf is
	use type Ada.Containers.Hash_Type;
	subtype UTF_8 is String;
	subtype C_8 is Character;
	subtype UTF_16 is Wide_String;
	subtype C_16 is Wide_Character;
	subtype UTF_32 is Wide_Wide_String;
	subtype C_32 is Wide_Wide_Character;
	type Encoded is record
		UTF_8 : not null access constant hash_case_insensitive_utf.UTF_8;
		UTF_16 : not null access constant hash_case_insensitive_utf.UTF_16;
		UTF_32 : not null access constant hash_case_insensitive_utf.UTF_32;
	end record;
	type Encoded_Array is array (Positive range <>) of Encoded;
	Table : constant array (Positive range <>) of access Encoded_Array := (
		new Encoded_Array'(
			1 => (new UTF_8'(""), new UTF_16'(""), new UTF_32'(""))),
		new Encoded_Array'(
			(new UTF_8'("A"), new UTF_16'("A"), new UTF_32'("A")),
			(new UTF_8'("a"), new UTF_16'("a"), new UTF_32'("a"))),
		new Encoded_Array'(
			(new UTF_8'("S"), new UTF_16'("S"), new UTF_32'("S")),
			(new UTF_8'("s"), new UTF_16'("s"), new UTF_32'("s")),
			(new UTF_8'(C_8'Val (16#C5#) & C_8'Val (16#BF#)),
				new UTF_16'(1 => C_16'Val (16#017F#)),
				new UTF_32'(1 => C_32'Val (16#017F#)))),
		new Encoded_Array'(
			(new UTF_8'(Ada.Characters.Latin_1.UC_A_Grave),
				new UTF_16'(1 => Ada.Wide_Characters.Latin_1.UC_A_Grave),
				new UTF_32'(1 => Ada.Wide_Wide_Characters.Latin_1.UC_A_Grave)),
			(new UTF_8'(Ada.Characters.Latin_1.LC_A_Grave),
				new UTF_16'(1 => Ada.Wide_Characters.Latin_1.LC_A_Grave),
				new UTF_32'(1 => Ada.Wide_Wide_Characters.Latin_1.LC_A_Grave))),
		new Encoded_Array'(
			(new UTF_8'(Ada.Characters.Latin_1.LC_Y_Diaeresis),
				new UTF_16'(1 => Ada.Wide_Characters.Latin_1.LC_Y_Diaeresis),
				new UTF_32'(
					1 => Ada.Wide_Wide_Characters.Latin_1.LC_Y_Diaeresis)),
			(new UTF_8'(C_8'Val (16#C5#) & C_8'Val (16#B8#)),
				new UTF_16'(1 => C_16'Val (16#0178#)),
				new UTF_32'(1 => C_32'Val (16#0178#)))),
		new Encoded_Array'(
			1 => (
				new UTF_8'(
					C_8'Val (16#F0#) & C_8'Val (16#90#) & C_8'Val (16#8C#)
					& C_8'Val (16#80#)),
				new UTF_16'(C_16'Val (16#D800#) & C_16'Val (16#DF00#)),
				new UTF_32'(1 => C_32'Val (16#10300#)))),
		new Encoded_Array'(
			1 => (new UTF_8'("123"), new UTF_16'("123"), new UTF_32'("123"))));
	Values : array (Table'Range) of Ada.Containers.Hash_Type;
begin
	-- make a mapping
	for I in Table'Range loop
		declare
			E : Encoded_Array renames Table (I).all;
			Value : Ada.Containers.Hash_Type renames Values (I);
		begin
			Value := Ada.Strings.Hash_Case_Insensitive (E (E'First).UTF_8.all);
			for J in Table (I)'Range loop
				declare
					F : Encoded renames E (J);
				begin
					pragma Assert (
						Ada.Strings.Hash_Case_Insensitive (F.UTF_8.all) = Value);
					pragma Assert (
						Ada.Strings.Wide_Hash_Case_Insensitive (F.UTF_16.all) =
						Value);
					pragma Assert (
						Ada.Strings.Wide_Wide_Hash_Case_Insensitive (F.UTF_32.all) =
						Value);
				end;
			end loop;
		end;
	end loop;
	-- check no-conflicts
	for I in Table'First .. Table'Last - 1 loop
		for J in I + 1 .. Table'Last loop
			pragma Assert (Values (I) /= Values (J));
			null;
		end loop;
	end loop;
	pragma Debug (Ada.Debug.Put ("OK"));
end hash_case_insensitive_utf;
