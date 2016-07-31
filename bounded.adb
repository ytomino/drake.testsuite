-- { dg-do run }
with Ada.Strings.Bounded_Strings;
with Ada.Strings.Bounded_Wide_Strings;
with Ada.Strings.Bounded_Wide_Wide_Strings;
with Ada.Strings.Generic_Bounded;
procedure bounded is
	generic
		with package Bounded is new Ada.Strings.Generic_Bounded (<>);
	procedure Generic_Try;
	procedure Generic_Try is
		A : constant Bounded.Character_Type :=
			Bounded.Character_Type'Val (Character'Pos ('A'));
		B : constant Bounded.Character_Type :=
			Bounded.Character_Type'Val (Character'Pos ('B'));
		C : constant Bounded.Character_Type :=
			Bounded.Character_Type'Val (Character'Pos ('C'));
		D : constant Bounded.Character_Type :=
			Bounded.Character_Type'Val (Character'Pos ('D'));
		E : constant Bounded.Character_Type :=
			Bounded.Character_Type'Val (Character'Pos ('E'));
		F : constant Bounded.Character_Type :=
			Bounded.Character_Type'Val (Character'Pos ('F'));
		package B5 is new Bounded.Generic_Bounded_Length (5);
		use type B5.Bounded_String;
	begin
		declare -- Append (S, "LONGER STRING", Drop => Right)
			S : B5.Bounded_String;
		begin
			B5.Set_Bounded_String (S, Bounded.String_Type'(1 => A));
			B5.Append (S, Bounded.String_Type'(B, C, D, E, F),
				Drop => Ada.Strings.Right);
			pragma Assert (S = Bounded.String_Type'(A, B, C, D, E));
			B5.Append (S, Bounded.String_Type'(F, E, D, C, B, A),
				Drop => Ada.Strings.Right);
			pragma Assert (S = Bounded.String_Type'(A, B, C, D, E));
		end;
		declare -- Append (S, "LONGER STRING", Drop => Left)
			S : B5.Bounded_String;
		begin
			B5.Set_Bounded_String (S, Bounded.String_Type'(1 => A));
			B5.Append (S, Bounded.String_Type'(B, C, D, E, F),
				Drop => Ada.Strings.Left);
			pragma Assert (S = Bounded.String_Type'(B, C, D, E, F));
			B5.Append (S, Bounded.String_Type'(F, E, D, C, B, A),
				Drop => Ada.Strings.Left);
			pragma Assert (S = Bounded.String_Type'(E, D, C, B, A));
		end;
		declare -- Append (S, S, Drop => Right)
			S : B5.Bounded_String;
		begin
			-- AB
			B5.Set_Bounded_String (S, Bounded.String_Type'(A, B));
			B5.Append (S, S, Drop => Ada.Strings.Right);
			pragma Assert (S = Bounded.String_Type'(A, B, A, B));
			B5.Append (S, S, Drop => Ada.Strings.Right);
			pragma Assert (S = Bounded.String_Type'(A, B, A, B, A));
			-- ABC
			B5.Set_Bounded_String (S, Bounded.String_Type'(A, B, C));
			B5.Append (S, S, Drop => Ada.Strings.Right);
			pragma Assert (S = Bounded.String_Type'(A, B, C, A, B));
		end;
		declare -- Append (S, S, Drop => Left)
			S : B5.Bounded_String;
		begin
			-- AB
			B5.Set_Bounded_String (S, Bounded.String_Type'(A, B));
			B5.Append (S, S, Drop => Ada.Strings.Left);
			pragma Assert (S = Bounded.String_Type'(A, B, A, B));
			B5.Append (S, S, Drop => Ada.Strings.Left);
			pragma Assert (S = Bounded.String_Type'(B, A, B, A, B));
			-- ABC
			B5.Set_Bounded_String (S, Bounded.String_Type'(A, B, C));
			B5.Append (S, S, Drop => Ada.Strings.Left);
			pragma Assert (S = Bounded.String_Type'(B, C, A, B, C));
		end;
	end Generic_Try;
	procedure Try_Strings is new Generic_Try (Ada.Strings.Bounded_Strings);
	procedure Try_Wide_Strings is
		new Generic_Try (Ada.Strings.Bounded_Wide_Strings);
	procedure Try_Wide_Wide_Strings is
		new Generic_Try (Ada.Strings.Bounded_Wide_Wide_Strings);
begin
	Try_Strings;
	Try_Wide_Strings;
	Try_Wide_Wide_Strings;
	pragma Debug (Ada.Debug.Put ("OK"));
end bounded;
