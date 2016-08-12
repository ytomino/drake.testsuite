-- { dg-do run }
with Ada.Strings.Generic_Functions;
with Ada.Strings.Functions.Maps;
with Ada.Strings.Maps;
with Ada.Strings.Wide_Functions.Maps;
with Ada.Strings.Wide_Wide_Functions.Maps;
procedure strings_fixed_thresholds is
begin
	declare
		A_Set : constant Ada.Strings.Maps.Character_Set :=
			Ada.Strings.Maps.To_Set ('A');
		function Wide_Wide_Id (X : Wide_Wide_Character)
			return Wide_Wide_Character is
		begin
			return X;
		end Wide_Wide_Id;
		generic
			with package F is new Ada.Strings.Generic_Functions (<>);
			with package M is
				new F.Generic_Maps (
					Character_Set => Ada.Strings.Maps.Character_Set,
					Character_Mapping => Ada.Strings.Maps.Character_Mapping,
					others => <>);
		procedure Generic_Test_Functions;
		procedure Generic_Test_Functions is
			use type F.String_Type;
			function Id (X : F.Character_Type) return F.Character_Type is
			begin
				return X;
			end Id;
			A : constant F.Character_Type :=
				F.Character_Type'Val (Character'Pos ('A'));
			B : constant F.Character_Type :=
				F.Character_Type'Val (Character'Pos ('B'));
			In_L0 : constant F.String_Type (Integer'Last .. Integer'Last - 1) :=
				(others => <>);
			In_A_L1 : constant F.String_Type (Integer'Last .. Integer'Last) :=
				(Integer'Last => A);
			In_B_L1 : constant F.String_Type (Integer'Last .. Integer'Last) :=
				(Integer'Last => B);
			In_Blank_L1 : constant F.String_Type (Integer'Last .. Integer'Last) :=
				(Integer'Last => F.Space);
			Out_L0 : F.String_Type (Integer'Last .. Integer'Last - 1);
			Out_L1 : F.String_Type (Integer'Last .. Integer'Last);
			P : Natural;
			Out_First : Positive;
			Out_Last : Natural;
		begin
			-- Move
			Out_L1 := (1 .. 1 => F.Space);
			F.Move (In_A_L1, Out_L1);
			pragma Assert (Out_L1 = In_A_L1);
			-- Index_Element
			P := F.Index_Element (In_A_L1, A, In_A_L1'First, Ada.Strings.Forward);
			pragma Assert (P = In_A_L1'First);
			P :=
				F.Index_Element (In_A_L1, A, In_A_L1'First, Ada.Strings.Backward);
			pragma Assert (P = In_A_L1'First);
			-- Index
			P := F.Index (In_A_L1, In_A_L1, In_A_L1'First, Ada.Strings.Forward);
			pragma Assert (P = In_A_L1'First);
			P := F.Index (In_A_L1, In_B_L1, In_A_L1'First, Ada.Strings.Forward);
			pragma Assert (P = 0);
			P := F.Index (In_A_L1, In_A_L1, In_A_L1'First, Ada.Strings.Backward);
			pragma Assert (P = In_A_L1'First);
			P := F.Index (In_A_L1, In_B_L1, In_A_L1'First, Ada.Strings.Backward);
			pragma Assert (P = 0);
			-- Index_Non_Blank
			P := F.Index_Non_Blank (In_A_L1, In_A_L1'First, Ada.Strings.Forward);
			pragma Assert (P = In_A_L1'First);
			P :=
				F.Index_Non_Blank (
					In_Blank_L1,
					In_Blank_L1'First,
					Ada.Strings.Forward);
			pragma Assert (P = 0);
			P := F.Index_Non_Blank (In_A_L1, In_A_L1'First, Ada.Strings.Backward);
			pragma Assert (P = In_A_L1'First);
			P :=
				F.Index_Non_Blank (
					In_Blank_L1,
					In_Blank_L1'First,
					Ada.Strings.Backward);
			pragma Assert (P = 0);
			-- Count
			P := F.Count (In_A_L1, In_A_L1);
			pragma Assert (P = 1);
			P := F.Count (In_Blank_L1, In_A_L1);
			pragma Assert (P = 0);
			-- Replace_Slice
			Out_L1 :=
				F.Replace_Slice (In_A_L1, In_A_L1'First, In_A_L1'Last, In_B_L1);
			pragma Assert (Out_L1 = In_B_L1);
			Out_L1 :=
				F.Replace_Slice (In_L0, In_L0'First, In_L0'Last, In_A_L1);
			pragma Assert (Out_L1 = In_A_L1);
			Out_L0 :=
				F.Replace_Slice (In_A_L1, In_A_L1'First, In_A_L1'Last, In_L0);
			Out_L1 := In_A_L1;
			F.Replace_Slice (Out_L1, Out_L1'First, Out_L1'Last, In_B_L1);
			pragma Assert (Out_L1 = In_B_L1);
			Out_L1 := In_Blank_L1;
			F.Replace_Slice (Out_L1, Out_L1'First, Out_L1'Last - 1, In_A_L1);
			pragma Assert (Out_L1 = In_A_L1);
			F.Replace_Slice (Out_L1, Out_L1'First, Out_L1'Last, In_L0);
			pragma Assert (Out_L1 = In_Blank_L1);
			-- Insert
			Out_L1 := F.Insert (In_L0, In_L0'First, In_A_L1);
			pragma Assert (Out_L1 = In_A_L1);
			Out_L1 := F.Insert (In_A_L1, In_A_L1'First, In_L0);
			pragma Assert (Out_L1 = In_A_L1);
			Out_L1 := In_Blank_L1;
			F.Insert (Out_L1, Out_L1'First, In_A_L1);
			pragma Assert (Out_L1 = In_A_L1);
			F.Insert (Out_L1, Out_L1'First, In_L0);
			pragma Assert (Out_L1 = In_A_L1);
			-- Overwrite
			Out_L1 := F.Overwrite (In_L0, In_L0'First, In_A_L1);
			pragma Assert (Out_L1 = In_A_L1);
			Out_L1 := F.Overwrite (In_A_L1, In_A_L1'First, In_L0);
			pragma Assert (Out_L1 = In_A_L1);
			Out_L1 := In_Blank_L1;
			F.Overwrite (Out_L1, Out_L1'First, In_A_L1);
			pragma Assert (Out_L1 = In_A_L1);
			F.Overwrite (Out_L1, Out_L1'First, In_L0);
			pragma Assert (Out_L1 = In_A_L1);
			-- Delete
			Out_L0 := F.Delete (In_A_L1, In_A_L1'First, In_A_L1'Last);
			Out_L1 := F.Delete (In_A_L1, In_A_L1'First, In_A_L1'First - 1);
			pragma Assert (Out_L1 = In_A_L1);
			Out_L1 := In_A_L1;
			F.Delete (Out_L1, Out_L1'First, Out_L1'Last - 1);
			pragma Assert (Out_L1 = In_A_L1);
			F.Delete (Out_L1, Out_L1'First, Out_L1'Last);
			pragma Assert (Out_L1 = In_Blank_L1);
			-- Trim
			for Side in Ada.Strings.Trim_End loop
				Out_L0 := F.Trim (In_Blank_L1, Side => Side);
				Out_L1 := F.Trim (In_A_L1, Side => Side);
				pragma Assert (Out_L1 = In_A_L1);
				Out_L1 := In_Blank_L1;
				F.Trim (Out_L1, Side => Side);
				pragma Assert (Out_L1 = In_Blank_L1);
				Out_L1 := In_A_L1;
				F.Trim (Out_L1, Side => Side);
				pragma Assert (Out_L1 = In_A_L1);
			end loop;
			-- Head
			Out_L1 := F.Head (In_A_L1, 1);
			pragma Assert (Out_L1 = In_A_L1);
			Out_L0 := F.Head (In_A_L1, 0);
			Out_L1 := In_A_L1;
			F.Head (Out_L1, 1);
			pragma Assert (Out_L1 = In_A_L1);
			F.Head (Out_L1, 0);
			pragma Assert (Out_L1 = In_Blank_L1);
			-- Tail
			Out_L1 := F.Tail (In_A_L1, 1);
			pragma Assert (Out_L1 = In_A_L1);
			Out_L0 := F.Tail (In_A_L1, 0);
			Out_L1 := In_A_L1;
			F.Tail (Out_L1, 1);
			pragma Assert (Out_L1 = In_A_L1);
			F.Tail (Out_L1, 0);
			pragma Assert (Out_L1 = In_Blank_L1);
			-- "*"
			Out_L1 := F."*" (1, In_A_L1);
			pragma Assert (Out_L1 = In_A_L1);
			-- Index (Character_Mapping)
			P :=
				M.Index (
					In_A_L1,
					In_A_L1,
					In_A_L1'First,
					Ada.Strings.Forward,
					Ada.Strings.Maps.Identity);
			pragma Assert (P = In_A_L1'First);
			P :=
				M.Index (
					In_A_L1,
					In_B_L1,
					In_A_L1'First,
					Ada.Strings.Forward,
					Ada.Strings.Maps.Identity);
			pragma Assert (P = 0);
			P :=
				M.Index (
					In_A_L1,
					In_A_L1,
					In_A_L1'First,
					Ada.Strings.Backward,
					Ada.Strings.Maps.Identity);
			pragma Assert (P = In_A_L1'First);
			P :=
				M.Index (
					In_A_L1,
					In_B_L1,
					In_A_L1'First,
					Ada.Strings.Backward,
					Ada.Strings.Maps.Identity);
			pragma Assert (P = 0);
			-- Index (function)
			P :=
				M.Index (
					In_A_L1,
					In_A_L1,
					In_A_L1'First,
					Ada.Strings.Forward,
					Wide_Wide_Id'Access);
			pragma Assert (P = In_A_L1'First);
			P :=
				M.Index (
					In_A_L1,
					In_B_L1,
					In_A_L1'First,
					Ada.Strings.Forward,
					Wide_Wide_Id'Access);
			pragma Assert (P = 0);
			P :=
				M.Index (
					In_A_L1,
					In_A_L1,
					In_A_L1'First,
					Ada.Strings.Backward,
					Wide_Wide_Id'Access);
			pragma Assert (P = In_A_L1'First);
			P :=
				M.Index (
					In_A_L1,
					In_B_L1,
					In_A_L1'First,
					Ada.Strings.Backward,
					Wide_Wide_Id'Access);
			pragma Assert (P = 0);
			-- Index_Element (function)
			P :=
				M.Index_Element (
					In_A_L1,
					In_A_L1,
					In_A_L1'First,
					Ada.Strings.Forward,
					Id'Access);
			pragma Assert (P = In_A_L1'First);
			P :=
				M.Index_Element (
					In_A_L1,
					In_B_L1,
					In_A_L1'First,
					Ada.Strings.Forward,
					Id'Access);
			pragma Assert (P = 0);
			P :=
				M.Index_Element (
					In_A_L1,
					In_A_L1,
					In_A_L1'First,
					Ada.Strings.Backward,
					Id'Access);
			pragma Assert (P = In_A_L1'First);
			P :=
				M.Index_Element (
					In_A_L1,
					In_B_L1,
					In_A_L1'First,
					Ada.Strings.Backward,
					Id'Access);
			pragma Assert (P = 0);
			-- Index (Character_Set)
			P :=
				M.Index (
					In_A_L1,
					Ada.Strings.Maps.Null_Set,
					In_A_L1'First,
					Ada.Strings.Outside,
					Ada.Strings.Forward);
			pragma Assert (P = In_A_L1'First);
			P :=
				M.Index (
					In_A_L1,
					Ada.Strings.Maps.Null_Set,
					In_A_L1'First,
					Ada.Strings.Inside,
					Ada.Strings.Forward);
			pragma Assert (P = 0);
			P :=
				M.Index (
					In_A_L1,
					Ada.Strings.Maps.Null_Set,
					In_A_L1'First,
					Ada.Strings.Outside,
					Ada.Strings.Backward);
			pragma Assert (P = In_A_L1'First);
			P :=
				M.Index (
					In_A_L1,
					Ada.Strings.Maps.Null_Set,
					In_A_L1'First,
					Ada.Strings.Inside,
					Ada.Strings.Backward);
			pragma Assert (P = 0);
			-- Count (Character_Mapping)
			P := M.Count (In_A_L1, In_A_L1, Ada.Strings.Maps.Identity);
			pragma Assert (P = 1);
			P := M.Count (In_Blank_L1, In_A_L1, Ada.Strings.Maps.Identity);
			pragma Assert (P = 0);
			-- Count (function)
			P := M.Count (In_A_L1, In_A_L1, Wide_Wide_Id'Access);
			pragma Assert (P = 1);
			P := M.Count (In_Blank_L1, In_A_L1, Wide_Wide_Id'Access);
			pragma Assert (P = 0);
			-- Count_Element (function)
			P := M.Count_Element (In_A_L1, In_A_L1, Id'Access);
			pragma Assert (P = 1);
			P := M.Count_Element (In_Blank_L1, In_A_L1, Id'Access);
			pragma Assert (P = 0);
			-- Count (Character_Set)
			P := M.Count (In_A_L1, A_Set);
			pragma Assert (P = 1);
			P := M.Count (In_Blank_L1, A_Set);
			pragma Assert (P = 0);
			-- Find_Token (Character_Set)
			M.Find_Token (
				In_A_L1,
				Ada.Strings.Maps.Null_Set,
				In_A_L1'First,
				Ada.Strings.Outside,
				Out_First,
				Out_Last);
			pragma Assert (Out_First = In_A_L1'First);
			pragma Assert (Out_Last = In_A_L1'First);
			M.Find_Token (
				In_A_L1,
				Ada.Strings.Maps.Null_Set,
				In_A_L1'First,
				Ada.Strings.Inside,
				Out_First,
				Out_Last);
			pragma Assert (Out_First = In_A_L1'First);
			pragma Assert (Out_Last = In_A_L1'First - 1);
			-- Find_Token_First (Character_Set)
			P :=
				M.Find_Token_First (
					In_A_L1,
					Ada.Strings.Maps.Null_Set,
					Ada.Strings.Outside);
			pragma Assert (P = In_A_L1'First);
			begin
				P :=
					M.Find_Token_First (
						In_A_L1,
						Ada.Strings.Maps.Null_Set,
						Ada.Strings.Inside);
				raise Program_Error; -- unreachable
			exception
				when Constraint_Error => null; -- by Integer'Last + 1
			end;
			P :=
				M.Find_Token_First (
					In_L0,
					Ada.Strings.Maps.Null_Set,
					Ada.Strings.Inside);
			pragma Assert (P = In_L0'Last + 1);
			-- Translate (Character_Mapping)
			Out_L1 := M.Translate (In_A_L1, Ada.Strings.Maps.Identity);
			pragma Assert (Out_L1 = In_A_L1);
			Out_L1 := In_A_L1;
			M.Translate (Out_L1, Ada.Strings.Maps.Identity);
			pragma Assert (Out_L1 = In_A_L1);
			-- Translate (function)
			Out_L1 := M.Translate (In_A_L1, Wide_Wide_Id'Access);
			pragma Assert (Out_L1 = In_A_L1);
			Out_L1 := In_A_L1;
			M.Translate (Out_L1, Wide_Wide_Id'Access);
			pragma Assert (Out_L1 = In_A_L1);
			-- Translate_Element (function)
			Out_L1 := M.Translate_Element (In_A_L1, Id'Access);
			pragma Assert (Out_L1 = In_A_L1);
			Out_L1 := In_A_L1;
			M.Translate_Element (Out_L1, Id'Access);
			pragma Assert (Out_L1 = In_A_L1);
			-- Trim (Character_Set)
			Out_L0 := M.Trim (In_A_L1, A_Set, Ada.Strings.Maps.Null_Set);
			Out_L0 := M.Trim (In_A_L1, Ada.Strings.Maps.Null_Set, A_Set);
			Out_L1 :=
				M.Trim (
					In_A_L1,
					Ada.Strings.Maps.Null_Set,
					Ada.Strings.Maps.Null_Set);
		end Generic_Test_Functions;
		procedure Test_Functions is
			new Generic_Test_Functions (
				Ada.Strings.Functions,
				Ada.Strings.Functions.Maps);
		procedure Test_Wide_Functions is
			new Generic_Test_Functions (
				Ada.Strings.Wide_Functions,
				Ada.Strings.Wide_Functions.Maps);
		procedure Test_Wide_Wide_Functions is
			new Generic_Test_Functions (
				Ada.Strings.Wide_Wide_Functions,
				Ada.Strings.Wide_Wide_Functions.Maps);
	begin
		Test_Functions;
		Test_Wide_Functions;
		Test_Wide_Wide_Functions;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end strings_fixed_thresholds;
