-- { dg-do run }
with Ada.Characters.ASCII.Handling;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Limited_Vectors;
with Ada.Containers.Vectors;
with Ada.Iterator_Interfaces;
with Ada.Streams.Unbounded_Storage_IO;
procedure vectors is
	use type Ada.Containers.Count_Type;
	function Custom_Eq (Left, Right : Character) return Boolean is
	begin
		return Ada.Characters.ASCII.Handling.To_Upper (Left) =
			Ada.Characters.ASCII.Handling.To_Upper (Right);
	end Custom_Eq;
	function Custom_Le (Left, Right : Character) return Boolean is
	begin
		return Ada.Characters.ASCII.Handling.To_Upper (Left) <
			Ada.Characters.ASCII.Handling.To_Upper (Right);
	end Custom_Le;
	package Vectors is new Ada.Containers.Vectors (
		Positive,
		Character,
		"=" => Custom_Eq);
	package Vectors_Sorting is new Vectors.Generic_Sorting ("<" => Custom_Le);
	package IVectors is new Ada.Containers.Indefinite_Vectors (
		Positive,
		Character,
		"=" => Custom_Eq);
	package IVectors_Sorting is new IVectors.Generic_Sorting ("<" => Custom_Le);
	pragma Unreferenced (IVectors_Sorting);
	package LVectors is new Ada.Containers.Limited_Vectors (
		Positive,
		Character);
begin
	declare -- Sorting
		function To_Vector is new Vectors.Generic_Array_To_Vector (String);
		use type Vectors.Vector;
		X : aliased Vectors.Vector := To_Vector ("ABC");
		Y : aliased Vectors.Vector := To_Vector ("abc");
	begin
		pragma Assert (X = Y);
		pragma Assert (X.Length = 3);
		pragma Assert (X.Constant_Reference (X.First_Index).Element.all = 'A');
		X.Swap (1, 3);
		pragma Assert (String (X.Constant_Reference.Element.all) = "CBA");
		X := To_Vector ("zxcvbnm");
		Y := To_Vector ("ASDFGHJKL");
		Vectors_Sorting.Sort (X);
		Vectors_Sorting.Sort (Y);
		Vectors_Sorting.Merge (X, Y);
		pragma Assert (String (X.Constant_Reference.Element.all) = "AbcDFGHJKLmnSvxz");
	end;
	declare -- copy-on-write
		use type Vectors.Vector;
		X : aliased Vectors.Vector := 'A' & 'B' & 'C';
		Y : aliased Vectors.Vector;
		Z : aliased Vectors.Vector;
	begin
		pragma Assert (X.Length = 3);
		pragma Assert (X.Element (X.Last) = 'C');
		pragma Assert (X.Constant_Reference (X.Last_Index).Element.all = 'C');
		Y := X & 'D';
		Z := X & 'E';
		pragma Assert (Y.Length = 4);
		pragma Assert (Y.Element (Y.Last) = 'D');
		pragma Assert (Z.Length = 4);
		pragma Assert (Z.Element (Z.Last) = 'E');
		pragma Assert (X.Constant_Reference (1).Element /=
			Y.Constant_Reference (1).Element);
		pragma Assert (X.Constant_Reference (1).Element /=
			Z.Constant_Reference (1).Element);
	end;
	declare -- copy-on-write for different lengths
		X, Y : Vectors.Vector;
	begin
		-- copy by the other (original) object
		Vectors.Clear (X);
		Vectors.Append (X, 'a');
		Vectors.Append (X, 'b');
		Vectors.Append (X, 'c');
		Y := X; -- share
		Vectors.Append (Y, 'd');
		declare
			Ref : Vectors.Reference_Type renames X.Reference (1); -- copy
		begin
			pragma Assert (Ref.Element.all = 'a');
			null;
		end;
		pragma Assert (Y.Element (4) = 'd');
	end;
	declare -- copy-on-write for different lengths
		X, Y : Vectors.Vector;
	begin
		-- copy by the other (copied) object
		Vectors.Clear (X);
		Vectors.Append (X, 'a');
		Vectors.Append (X, 'b');
		Vectors.Append (X, 'c');
		Y := X; -- share
		Vectors.Append (X, 'd');
		declare
			Ref : Vectors.Reference_Type renames Y.Reference (1); -- copy
		begin
			pragma Assert (Ref.Element.all = 'a');
			null;
		end;
		pragma Assert (X.Element (4) = 'd');
	end;
	declare -- Set_Length
		X : Vectors.Vector;
	begin
		Vectors.Set_Length (X, 10);
	end;
	declare -- insert
		use type Vectors.Vector;
		X : aliased Vectors.Vector := 'A' & 'B' & 'C' & 'D';
		Position : Vectors.Cursor;
	begin
		Position := Vectors.To_Cursor (X, 2);
		Vectors.Delete (X, Position, 2);
		pragma Assert (Position = Vectors.No_Element);
		pragma Assert (X.Length = 2);
		pragma Assert (X.Element (X.First) = 'A');
		pragma Assert (X.Element (X.Last) = 'D');
		Vectors.Insert (X, 2, 'Z');
		pragma Assert (X.Length = 3);
		pragma Assert (X.Element (1) = 'A');
		pragma Assert (X.Element (2) = 'Z');
		pragma Assert (X.Element (3) = 'D');
	end;
	declare -- "&"
		use type IVectors.Vector;
		X : aliased IVectors.Vector := 'A' & 'B' & 'C';
		Y : aliased IVectors.Vector;
		Z : aliased IVectors.Vector;
		Position : IVectors.Cursor;
	begin
		pragma Assert (X.Length = 3);
		pragma Assert (X.Element (X.Last) = 'C');
		Y := X & 'D';
		Z := X & 'E';
		pragma Assert (Y.Length = 4);
		pragma Assert (Y.Element (Y.Last) = 'D');
		pragma Assert (Z.Length = 4);
		pragma Assert (Z.Element (Z.Last) = 'E');
		pragma Assert (X.Constant_Reference (1).Element /=
			Y.Constant_Reference (1).Element);
		pragma Assert (X.Constant_Reference (1).Element /=
			Z.Constant_Reference (1).Element);
		X := 'A' & 'B' & 'C' & 'D';
		Position := IVectors.To_Cursor (X, 2);
		IVectors.Delete (X, Position, 2);
		pragma Assert (Position = IVectors.No_Element);
		pragma Assert (X.Length = 2);
		pragma Assert (X.Element (X.First) = 'A');
		pragma Assert (X.Element (X.Last) = 'D');
		IVectors.Insert (X, 2, 'Z');
		pragma Assert (X.Length = 3);
		pragma Assert (X.Element (1) = 'A');
		pragma Assert (X.Element (2) = 'Z');
		pragma Assert (X.Element (3) = 'D');
	end;
	declare -- iterate
		use Vectors;
		X : aliased Vectors.Vector;
	begin
		pragma Assert (X.Constant_Reference.Element.all = "");
		for I in Character'('A') .. 'Z' loop
			Append (X, I);
		end loop;
		-- accessor
		pragma Assert (X.Reference.Element.all = "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
		pragma Assert (X.Reference.Element (1 .. 26) = "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
		pragma Assert (X.Reference.Element (2 .. 3) = "BC");
		-- forward iteration
		declare
			Ite : Vector_Iterator_Interfaces.Reversible_Iterator'Class := X.Iterate;
			Pos : Cursor := Vector_Iterator_Interfaces.First (Ite);
			C : Character := 'A';
		begin
			while Pos /= No_Element loop
				pragma Assert (X.Reference (Pos).Element.all = C);
				C := Character'Succ (C);
				Pos := Vector_Iterator_Interfaces.Next (Ite, Pos);
			end loop;
		end;
		-- forward iteration (Ada 2012)
		declare
			C : Character := 'A';
		begin
			for E of X loop
				pragma Assert (E = C);
				C := Character'Succ (C);
			end loop;
		end;
		-- reverse iteration
		declare
			Ite : Vector_Iterator_Interfaces.Reversible_Iterator'Class := X.Iterate;
			Pos : Cursor := Vector_Iterator_Interfaces.Last (Ite);
			C : Character := 'Z';
		begin
			while Pos /= No_Element loop
				pragma Assert (X.Reference (Pos).Element.all = C);
				C := Character'Pred (C);
				Pos := Vector_Iterator_Interfaces.Previous (Ite, Pos);
			end loop;
		end;
		-- reverse iteration (Ada 2012)
		declare
			C : Character := 'Z';
		begin
			for E of reverse X loop
				pragma Assert (E = C);
				C := Character'Pred (C);
			end loop;
		end;
	end;
	declare -- Append (X, X)
		use type Vectors.Element_Array;
		X : Vectors.Vector;
		IX : IVectors.Vector;
	begin
		-- no effect
		Vectors.Append (X, X);
		IVectors.Append (IX, IX);
		Vectors.Insert (X, 1, X);
		IVectors.Insert (IX, 1, IX);
		-- construction
		Vectors.Append (X, 'a');
		Vectors.Append (X, 'b');
		IVectors.Append (IX, 'a');
		IVectors.Append (IX, 'b');
		-- append self
		Vectors.Append (X, X);
		IVectors.Append (IX, IX);
		pragma Assert (X.Length = 4);
		pragma Assert (X.Constant_Reference.Element.all = "abab");
		pragma Assert (IX.Length = 4);
		for I in 1 .. 4 loop
			pragma Assert (IX.Element (I) = X.Element (I));
			null;
		end loop;
	end;
	declare -- Find
		generic
			type T is tagged limited private;
			type C is private;
			with procedure Append (
				X : in out T;
				E : in Character;
				L : in Ada.Containers.Count_Type := 1) is <>;
			with function Find (X : T; E : Character) return C is <>;
			with function Find (X : T; E : Character; P : C) return C is <>;
			with function Find_Index (X : T'Class; E : Character; P : Positive)
				return Natural is <>;
			with function Reverse_Find (X : T; E : Character) return C is <>;
			with function Reverse_Find (X : T; E : Character; P : C)
				return C is <>;
			with function Reverse_Find_Index (
				X : T'Class;
				E : Character;
				P : Positive)
				return Natural is <>;
			No_Element : in C;
		procedure Generic_Try_Find;
		procedure Generic_Try_Find is
			X : T;
		begin
			-- empty
			pragma Assert (Find (X, 'A') = No_Element);
			pragma Assert (Find (X, 'A', No_Element) = No_Element);
			pragma Assert (Find_Index (X, 'A', 1) = 0);
			pragma Assert (Reverse_Find (X, 'A') = No_Element);
			pragma Assert (Reverse_Find (X, 'A', No_Element) = No_Element);
			pragma Assert (Reverse_Find_Index (X, 'A', Integer'Last) = 0);
			-- not empty
			Append (X, 'b');
			pragma Assert (Find (X, 'A') = No_Element);
			begin
				pragma Assert (Find (X, 'A', No_Element) = No_Element);
				raise Program_Error; -- correct behavior
			exception
				when Constraint_Error => null; -- intentional violation
			end;
			pragma Assert (Find_Index (X, 'A', 1) = 0);
			pragma Assert (Reverse_Find (X, 'A') = No_Element);
			begin
				pragma Assert (Reverse_Find (X, 'A', No_Element) = No_Element);
				raise Program_Error; -- correct behavior
			exception
				when Constraint_Error => null; -- intentional violation
			end;
			pragma Assert (Reverse_Find_Index (X, 'A', Integer'Last) = 0);
		end Generic_Try_Find;
		procedure Try_D is
			new Generic_Try_Find (
				Vectors.Vector,
				Vectors.Cursor,
				Vectors.Append,
				Vectors.Find,
				Vectors.Find,
				Vectors.Find_Index,
				Vectors.Reverse_Find,
				Vectors.Reverse_Find,
				Vectors.Reverse_Find_Index,
				Vectors.No_Element);
		procedure Try_I is
			new Generic_Try_Find (
				IVectors.Vector,
				IVectors.Cursor,
				IVectors.Append,
				IVectors.Find,
				IVectors.Find,
				IVectors.Find_Index,
				IVectors.Reverse_Find,
				IVectors.Reverse_Find,
				IVectors.Reverse_Find_Index,
				IVectors.No_Element);
	begin
		Try_D;
		Try_I;
	end;
	declare -- Iterate (invalid ranges)
		generic
			type T is tagged limited private;
			with function Last (C : T) return Natural;
			with package II is
				new Ada.Iterator_Interfaces (Natural, Has_Element => <>);
			with function Iterate (C : T'Class; First, Last : Natural)
				return II.Reversible_Iterator'Class;
		procedure Iterate_Invalid_Ranges (C : T);
		procedure Iterate_Invalid_Ranges (C : T) is
		begin
			for I in C.Iterate (1, 0) loop
				raise Program_Error;
			end loop;
			for I in reverse C.Iterate (1, 0) loop
				raise Program_Error;
			end loop;
			for I in C.Iterate (Last (C) + 1, Last (C)) loop
				raise Program_Error;
			end loop;
			for I in reverse C.Iterate (Last (C) + 1, Last (C)) loop
				raise Program_Error;
			end loop;
			for I in C.Iterate (0, Last (C)) loop
				raise Program_Error;
			end loop;
			for I in reverse C.Iterate (0, Last (C)) loop
				raise Program_Error;
			end loop;
		end Iterate_Invalid_Ranges;
		procedure IIR is
			new Iterate_Invalid_Ranges (
				Vectors.Vector,
				Vectors.Last,
				Vectors.Vector_Iterator_Interfaces,
				Vectors.Iterate);
		procedure IIR is
			new Iterate_Invalid_Ranges (
				IVectors.Vector,
				IVectors.Last,
				IVectors.Vector_Iterator_Interfaces,
				IVectors.Iterate);
		procedure IIR is
			new Iterate_Invalid_Ranges (
				LVectors.Vector,
				LVectors.Last,
				LVectors.Vector_Iterator_Interfaces,
				LVectors.Iterate);
		C : Vectors.Vector;
		IC : IVectors.Vector;
		LC : LVectors.Vector;
	begin
		for Count in 0 .. 1 loop
			IIR (C);
			IIR (IC);
			IIR (LC);
			Vectors.Append (C, 'x');
			IVectors.Append (IC, 'x');
			declare
				function New_X return Character renames 'x';
				P : LVectors.Cursor;
			begin
				LVectors.Insert (LC, LVectors.No_Element, New_X'Access,
					Position => P);
			end;
		end loop;
	end;
	declare -- streaming
		package USIO renames Ada.Streams.Unbounded_Storage_IO;
		X : Vectors.Vector;
		IX : IVectors.Vector;
		Buffer : USIO.Buffer_Type;
	begin
		-- Definite -> Inefinite (0)
		Vectors.Vector'Write (USIO.Stream (Buffer), X); -- write empty
		IVectors.Append (IX, 'a');
		pragma Assert (IX.Length = 1);
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		IVectors.Vector'Read (USIO.Stream (Buffer), IX);
		pragma Assert (IX.Length = 0);
		-- Indefinite -> Definite (1)
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		IVectors.Append (IX, 'b');
		pragma Assert (IX.Length = 1);
		IVectors.Vector'Write (USIO.Stream (Buffer), IX); -- write 'b'
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		Vectors.Vector'Read (USIO.Stream (Buffer), X);
		pragma Assert (X.Length = 1);
		pragma Assert (X.Element (1) = 'b');
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end vectors;
