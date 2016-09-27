-- { dg-do run }
with Ada.Characters.Latin_1;
with Ada.Wide_Characters.Latin_1;
with Ada.Wide_Wide_Characters.Latin_1;
with Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Wide_Hash;
with Ada.Strings.Wide_Wide_Hash;
with Ada.Strings.Fixed.Hash;
with Ada.Strings.Wide_Fixed.Wide_Hash;
with Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash;
with Ada.Strings.Bounded.Hash;
with Ada.Strings.Wide_Bounded.Wide_Hash;
with Ada.Strings.Wide_Wide_Bounded.Wide_Wide_Hash;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Wide_Unbounded.Wide_Hash;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
procedure hash_utf is
	use type Ada.Containers.Hash_Type;
	subtype UTF_8 is String;
	subtype C_8 is Character;
	subtype UTF_16 is Wide_String;
	subtype C_16 is Wide_Character;
	subtype UTF_32 is Wide_Wide_String;
	subtype C_32 is Wide_Wide_Character;
	type Encoded is record
		UTF_8 : not null access constant hash_utf.UTF_8;
		UTF_16 : not null access constant hash_utf.UTF_16;
		UTF_32 : not null access constant hash_utf.UTF_32;
	end record;
	Table : constant array (Positive range <>) of Encoded := (
		(new UTF_8'(""), new UTF_16'(""), new UTF_32'("")),
		(new UTF_8'("a"), new UTF_16'("a"), new UTF_32'("a")),
		(new UTF_8'("A"), new UTF_16'("A"), new UTF_32'("A")),
		(new UTF_8'("S"), new UTF_16'("S"), new UTF_32'("S")),
		(new UTF_8'("s"), new UTF_16'("s"), new UTF_32'("s")),
		(new UTF_8'(C_8'Val (16#C5#) & C_8'Val (16#BF#)),
			new UTF_16'(1 => C_16'Val (16#017F#)),
			new UTF_32'(1 => C_32'Val (16#017F#))),
		(new UTF_8'(Ada.Characters.Latin_1.UC_A_Grave),
			new UTF_16'(1 => Ada.Wide_Characters.Latin_1.UC_A_Grave),
			new UTF_32'(1 => Ada.Wide_Wide_Characters.Latin_1.UC_A_Grave)),
		(new UTF_8'(Ada.Characters.Latin_1.LC_A_Grave),
			new UTF_16'(1 => Ada.Wide_Characters.Latin_1.LC_A_Grave),
			new UTF_32'(1 => Ada.Wide_Wide_Characters.Latin_1.LC_A_Grave)),
		(new UTF_8'(Ada.Characters.Latin_1.LC_Y_Diaeresis),
			new UTF_16'(1 => Ada.Wide_Characters.Latin_1.LC_Y_Diaeresis),
			new UTF_32'(1 => Ada.Wide_Wide_Characters.Latin_1.LC_Y_Diaeresis)),
		(new UTF_8'(C_8'Val (16#C5#) & C_8'Val (16#B8#)),
			new UTF_16'(1 => C_16'Val (16#0178#)),
			new UTF_32'(1 => C_32'Val (16#0178#))),
		(new UTF_8'(
				C_8'Val (16#F0#) & C_8'Val (16#90#) & C_8'Val (16#8C#)
				& C_8'Val (16#80#)),
			new UTF_16'(C_16'Val (16#D800#) & C_16'Val (16#DF00#)),
			new UTF_32'(1 => C_32'Val (16#10300#))),
		(new UTF_8'("123"), new UTF_16'("123"), new UTF_32'("123")));
	Values : array (Table'Range) of Ada.Containers.Hash_Type;
	generic
		type UTF_8_Type (<>) is private;
		with function To (X : UTF_8) return UTF_8_Type;
		with function Hash (X : UTF_8_Type) return Ada.Containers.Hash_Type;
		type UTF_16_Type (<>) is private;
		with function To (X : UTF_16) return UTF_16_Type;
		with function Hash (X : UTF_16_Type) return Ada.Containers.Hash_Type;
		type UTF_32_Type (<>) is private;
		with function To (X : UTF_32) return UTF_32_Type;
		with function Hash (X : UTF_32_Type) return Ada.Containers.Hash_Type;
	procedure Generic_Try;
	procedure Generic_Try is
	begin
		for I in Table'Range loop
			declare
				E : Encoded renames Table (I);
				Value : Ada.Containers.Hash_Type renames Values (I);
			begin
				pragma Assert (Hash (To (E.UTF_8.all)) = Value);
				pragma Assert (Hash (To (E.UTF_16.all)) = Value);
				pragma Assert (Hash (To (E.UTF_32.all)) = Value);
				null;
			end;
		end loop;
	end Generic_Try;
begin
	-- make a mapping
	for I in Table'Range loop
		declare
			E : Encoded renames Table (I);
			Value : Ada.Containers.Hash_Type renames Values (I);
		begin
			Value := Ada.Strings.Hash (E.UTF_8.all);
			pragma Assert (Ada.Strings.Wide_Hash (E.UTF_16.all) = Value);
			pragma Assert (Ada.Strings.Wide_Wide_Hash (E.UTF_32.all) = Value);
		end;
	end loop;
	-- check no-conflicts
	for I in Table'First .. Table'Last - 1 loop
		for J in I + 1 .. Table'Last loop
			pragma Assert (Values (I) /= Values (J));
			null;
		end loop;
	end loop;
	-- fixed
	declare
		function Identity (S : String) return String is
		begin
			return S;
		end Identity;
		function Identity (S : Wide_String) return Wide_String is
		begin
			return S;
		end Identity;
		function Identity (S : Wide_Wide_String) return Wide_Wide_String is
		begin
			return S;
		end Identity;
		procedure Try_Fixed is
			new Generic_Try (
				String,
				Identity,
				Ada.Strings.Fixed.Hash,
				Wide_String,
				Identity,
				Ada.Strings.Wide_Fixed.Wide_Hash,
				Wide_Wide_String,
				Identity,
				Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash);
	begin
		Try_Fixed;
	end;
	-- bounded
	declare
		package Bounded_Length is
			new Ada.Strings.Bounded.Generic_Bounded_Length (10);
		function Hash is new Ada.Strings.Bounded.Hash (Bounded_Length);
		package Wide_Bounded_Length is
			new Ada.Strings.Wide_Bounded.Generic_Bounded_Length (10);
		function Hash is
			new Ada.Strings.Wide_Bounded.Wide_Hash (Wide_Bounded_Length);
		package Wide_Wide_Bounded_Length is
			new Ada.Strings.Wide_Wide_Bounded.Generic_Bounded_Length (10);
		function Hash is
			new Ada.Strings.Wide_Wide_Bounded.Wide_Wide_Hash (
				Wide_Wide_Bounded_Length);
		procedure Try_Unbounded is
			new Generic_Try (
				Bounded_Length.Bounded_String,
				Bounded_Length.Bounded_Strings."+",
				Hash,
				Wide_Bounded_Length.Bounded_Wide_String,
				Wide_Bounded_Length.Bounded_Wide_Strings."+",
				Hash,
				Wide_Wide_Bounded_Length.Bounded_Wide_Wide_String,
				Wide_Wide_Bounded_Length.Bounded_Wide_Wide_Strings."+",
				Hash);
	begin
		Try_Unbounded;
	end;
	-- unbounded
	declare
		procedure Try_Unbounded is
			new Generic_Try (
				Ada.Strings.Unbounded.Unbounded_String,
				Ada.Strings.Unbounded.To_Unbounded_String,
				Ada.Strings.Unbounded.Hash,
				Ada.Strings.Wide_Unbounded.Unbounded_Wide_String,
				Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String,
				Ada.Strings.Wide_Unbounded.Wide_Hash,
				Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String,
				Ada.Strings.Wide_Wide_Unbounded.To_Unbounded_Wide_Wide_String,
				Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash);
	begin
		Try_Unbounded;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end hash_utf;
