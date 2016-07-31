-- { dg-do run }
with Ada.Strings.Generic_Unbounded;
with Ada.Strings.Unbounded_Strings;
with Ada.Strings.Unbounded_Wide_Strings;
with Ada.Strings.Unbounded_Wide_Wide_Strings;
procedure unbounded is
	generic
		with package Unbounded is new Ada.Strings.Generic_Unbounded (<>);
	procedure Generic_Try;
	procedure Generic_Try is
		A : constant Unbounded.Character_Type :=
			Unbounded.Character_Type'Val (Character'Pos ('A'));
		B : constant Unbounded.Character_Type :=
			Unbounded.Character_Type'Val (Character'Pos ('B'));
		C : constant Unbounded.Character_Type :=
			Unbounded.Character_Type'Val (Character'Pos ('C'));
		use type Unbounded.Unbounded_String;
	begin
		declare -- Append (S, S)
			S : Unbounded.Unbounded_String;
		begin
			-- AB
			Unbounded.Set_Unbounded_String (S, Unbounded.String_Type'(A, B));
			Unbounded.Append (S, S);
			pragma Assert (S = Unbounded.String_Type'(A, B, A, B));
			Unbounded.Append (S, S);
			pragma Assert (S = Unbounded.String_Type'(A, B, A, B, A, B, A, B));
			-- ABC
			Unbounded.Set_Unbounded_String (S, Unbounded.String_Type'(A, B, C));
			Unbounded.Append (S, S);
			pragma Assert (S = Unbounded.String_Type'(A, B, C, A, B, C));
		end;
	end Generic_Try;
	procedure Try_Strings is new Generic_Try (Ada.Strings.Unbounded_Strings);
	procedure Try_Wide_Strings is
		new Generic_Try (Ada.Strings.Unbounded_Wide_Strings);
	procedure Try_Wide_Wide_Strings is
		new Generic_Try (Ada.Strings.Unbounded_Wide_Wide_Strings);
begin
	Try_Strings;
	Try_Wide_Strings;
	Try_Wide_Wide_Strings;
	pragma Debug (Ada.Debug.Put ("OK"));
end unbounded;
