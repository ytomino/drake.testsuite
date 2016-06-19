-- { dg-do run }
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Limited_Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;
with Ada.Streams.Unbounded_Storage_IO;
procedure doubly_linked_lists is
	use type Ada.Containers.Count_Type;
	package Lists is new Ada.Containers.Doubly_Linked_Lists (Character);
	package ILists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Character);
	package LLists is new Ada.Containers.Limited_Doubly_Linked_Lists (Character);
begin
	declare -- Append
		X : Lists.List;
	begin
		Lists.Append (X, 'A');
		pragma Assert (X.Length = 1);
		pragma Assert (Lists.Element (X.First) = 'A');
		pragma Assert (Lists.Element (X.Last) = 'A');
		Lists.Append (X, 'B');
		pragma Assert (X.Length = 2);
		pragma Assert (Lists.Element (X.First) = 'A');
		pragma Assert (Lists.Element (X.Last) = 'B');
		Lists.Append (X, 'C');
		pragma Assert (X.Length = 3);
		pragma Assert (Lists.Element (X.First) = 'A');
		pragma Assert (Lists.Element (X.Last) = 'C');
	end;
	declare -- Iterate
		use type Lists.Cursor;
		X : aliased Lists.List;
		C : Character;
	begin
		for I in Character'('A') .. 'F' loop
			Lists.Append (X, I);
		end loop;
		C := 'A';
		declare
			Ite : Lists.List_Iterator_Interfaces.Reversible_Iterator'Class := X.Iterate;
			Pos : Lists.Cursor := Lists.List_Iterator_Interfaces.First (Ite);
		begin
			while Pos /= Lists.No_Element loop
				pragma Assert (Lists.Element (Pos) = C);
				C := Character'Succ (C);
				Pos := Lists.List_Iterator_Interfaces.Next (Ite, Pos);
			end loop;
		end;
		declare
			Ite : Lists.List_Iterator_Interfaces.Reversible_Iterator'Class := X.Iterate;
			Pos : Lists.Cursor := Lists.List_Iterator_Interfaces.Last (Ite);
		begin
			while Pos /= Lists.No_Element loop
				C := Character'Pred (C);
				pragma Assert (Lists.Element (Pos) = C);
				Pos := Lists.List_Iterator_Interfaces.Previous (Ite, Pos);
			end loop;
		end;
		declare
			I : Lists.Cursor := X.First;
			Last : Lists.Cursor := X.Last;
		begin
			if Lists.Has_Element (I) then
				loop
					pragma Assert (Lists.Element (I) = C);
					C := Character'Succ (C);
					exit when I = Last;
					Lists.Next (I);
				end loop;
			end if;
		end;
		declare
			I : Lists.Cursor := X.Last;
			First : Lists.Cursor := X.First;
		begin
			if Lists.Has_Element (I) then
				loop
					C := Character'Pred (C);
					pragma Assert (Lists.Element (I) = C);
					exit when I = First;
					Lists.Previous (I);
				end loop;
			end if;
		end;
		declare
			I : Lists.Cursor := X.First;
			Last : Lists.Cursor := Lists.Previous (X.Last);
		begin
			if Lists.Has_Element (I) then
				loop
					pragma Assert (Lists.Element (I) = C);
					exit when I = Last;
					C := Character'Succ (C);
					Lists.Next (I);
				end loop;
			end if;
		end;
		pragma Assert (C = 'E');
		Lists.Clear (X);
		declare
			I : Lists.Cursor := X.First;
		begin
			while Lists.Has_Element (I) loop
				pragma Assert (False);
				Lists.Next (I);
			end loop;
		end;
		declare
			I : Lists.Cursor := X.Last;
		begin
			while Lists.Has_Element (I) loop
				pragma Assert (False);
				Lists.Previous (I);
			end loop;
		end;
	end;
	declare -- Reverse_Elements, Swap_Links
		X : Lists.List;
		I : Lists.Cursor;
	begin
		Lists.Append (X, 'A');
		Lists.Append (X, 'B');
		Lists.Append (X, 'C');
		Lists.Reverse_Elements (X);
		pragma Assert (X.Length = 3);
		pragma Assert (Lists.Element (X.First) = 'C');
		pragma Assert (Lists.Element (Lists.Next (X.First)) = 'B');
		pragma Assert (Lists.Element (X.Last) = 'A');
		Lists.Swap_Links (X, X.First, X.Last);
		declare
			Data : String := "ABC";
			Index : Integer := 1;
		begin
			I := X.First;
			while Lists.Has_Element (I) loop
				pragma Assert (Lists.Element (I) = Data (Index));
				Index := Integer'Succ (Index);
				Lists.Next (I);
			end loop;
			I := X.Last;
			while Lists.Has_Element (I) loop
				Index := Integer'Pred (Index);
				pragma Assert (Lists.Element (I) = Data (Index));
				Lists.Previous (I);
			end loop;
		end;
		Lists.Swap_Links (X, X.First, Lists.Next (X.First));
		declare
			Data : String := "BAC";
			Index : Integer := 1;
		begin
			I := X.First;
			while Lists.Has_Element (I) loop
				pragma Assert (Lists.Element (I) = Data (Index));
				Index := Integer'Succ (Index);
				Lists.Next (I);
			end loop;
			I := X.Last;
			while Lists.Has_Element (I) loop
				Index := Integer'Pred (Index);
				pragma Assert (Lists.Element (I) = Data (Index));
				Lists.Previous (I);
			end loop;
		end;
	end;
	declare -- Splice
		use type Lists.Cursor;
		X : Lists.List;
		Y : Lists.List;
	begin
		Lists.Append (X, 'A');
		Lists.Append (X, 'B');
		Lists.Append (Y, '1');
		Lists.Append (Y, '2');
		Lists.Splice (X, Lists.No_Element, Y);
		pragma Assert (X.Length = 4);
		pragma Assert (Lists.Element (X.First) = 'A');
		pragma Assert (Lists.Element (X.Last) = '2');
		pragma Assert (Y.Length = 0);
		pragma Assert (Y.First = Lists.No_Element);
		pragma Assert (Y.Last = Lists.No_Element);
	end;
	declare -- Sort
		use type Lists.List;
		package Sorting is new Lists.Generic_Sorting;
		X : Lists.List;
		Y : Lists.List;
	begin
		Lists.Append (X, '1');
		Lists.Append (X, '3');
		Lists.Append (X, '5');
		Lists.Append (Y, '2');
		Lists.Append (Y, '4');
		Lists.Append (Y, '6');
		Sorting.Merge (X, Y);
		pragma Assert (X.Length = 6);
		pragma Assert (Y.Length = 0);
		pragma Assert (Sorting.Is_Sorted (X));
		pragma Assert (Lists.Is_Empty (Y));
		Lists.Append (Y, '4');
		Lists.Append (Y, '2');
		Lists.Append (Y, '5');
		Lists.Append (Y, '3');
		Lists.Append (Y, '1');
		Lists.Append (Y, '6');
		Sorting.Sort (Y);
		pragma Assert (Sorting.Is_Sorted (Y));
		pragma Assert (X = Y);
	end;
	declare -- copy-on-write
		use type Lists.Cursor;
		X : Lists.List;
		Y : Lists.List;
		X_F : Lists.Cursor;
		Y_F : Lists.Cursor;
	begin
		Lists.Append (X, 'a');
		X_F := X.First;
		Y := X; -- shared
		Y_F := Y.First; -- read operation for Y
		pragma Assert (X.First = X_F, "should keep X");
		pragma Assert (Y.First = Y_F, "should keep Y");
		pragma Assert (X_F /= Y_F, "should be copied");
		Y := X; -- shared
		Y_F := X.First; -- read operation for X, result is garbage
		Y_F := Y.First;
		pragma Assert (X.First = X_F, "should keep X");
		pragma Assert (Y.First = Y_F, "should keep Y");
		pragma Assert (X_F /= Y_F, "should be copied");
		Y := X; -- shared
		Lists.Append (Y, 'b'); -- write operation for Y
		Y_F := Y.First;
		pragma Assert (X.First = X_F, "should keep X");
		pragma Assert (Y.First = Y_F, "should keep Y");
		pragma Assert (X_F /= Y_F, "should be copied");
		Y := X; -- shared
		Lists.Append (X, 'b'); -- write operation for X
		Y_F := Y.First;
		pragma Assert (X.First = X_F, "should keep X");
		pragma Assert (Y.First = Y_F, "should keep Y");
		pragma Assert (X_F /= Y_F, "should be copied");
	end;
	declare -- Find
		use type Lists.Cursor;
		X : Lists.List;
		Fst_A, Snd_A : Lists.Cursor;
	begin
		Lists.Insert (X, Lists.No_Element, 'b');
		Lists.Insert (X, Lists.No_Element, 'a', Fst_A);
		Lists.Insert (X, Lists.No_Element, 'b');
		Lists.Insert (X, Lists.No_Element, 'a', Snd_A);
		Lists.Insert (X, Lists.No_Element, 'b');
		pragma Assert (Lists.Find (X, 'a') = Fst_A);
		pragma Assert (Lists.Find (X, 'a', Lists.Next (Fst_A)) = Snd_A);
		pragma Assert (Lists.Reverse_Find (X, 'a') = Snd_A);
		pragma Assert (Lists.Reverse_Find (X, 'a', Lists.Previous (Snd_A)) = Fst_A);
	end;
	declare -- Iterate (invalid ranges)
		generic
			type Cursor is private;
			No_Element : Cursor;
			type T is tagged limited private;
			with function First (C : T) return Cursor;
			with function Last (C : T) return Cursor;
			with package II is
				new Ada.Iterator_Interfaces (Cursor, Has_Element => <>);
			with function Iterate (C : T'Class; First, Last : Cursor)
				return II.Reversible_Iterator'Class;
		procedure Iterate_Invalid_Ranges (C : T);
		procedure Iterate_Invalid_Ranges (C : T) is
		begin
			for I in C.Iterate (First (C), No_Element) loop
				raise Program_Error;
			end loop;
			for I in reverse C.Iterate (First (C), No_Element) loop
				raise Program_Error;
			end loop;
			for I in C.Iterate (No_Element, Last (C)) loop
				raise Program_Error;
			end loop;
			for I in reverse C.Iterate (No_Element, Last (C)) loop
				raise Program_Error;
			end loop;
		end Iterate_Invalid_Ranges;
		procedure IIR is
			new Iterate_Invalid_Ranges (
				Lists.Cursor,
				Lists.No_Element,
				Lists.List,
				Lists.First,
				Lists.Last,
				Lists.List_Iterator_Interfaces,
				Lists.Iterate);
		procedure IIR is
			new Iterate_Invalid_Ranges (
				ILists.Cursor,
				ILists.No_Element,
				ILists.List,
				ILists.First,
				ILists.Last,
				ILists.List_Iterator_Interfaces,
				ILists.Iterate);
		procedure IIR is
			new Iterate_Invalid_Ranges (
				LLists.Cursor,
				LLists.No_Element,
				LLists.List,
				LLists.First,
				LLists.Last,
				LLists.List_Iterator_Interfaces,
				LLists.Iterate);
		C : Lists.List;
		IC : ILists.List;
		LC : LLists.List;
	begin
		for Count in 0 .. 1 loop
			IIR (C);
			IIR (IC);
			IIR (LC);
			Lists.Append (C, 'x');
			ILists.Append (IC, 'x');
			declare
				function New_X return Character renames 'x';
				P : LLists.Cursor;
			begin
				LLists.Insert (LC, LLists.No_Element, New_X'Access, P);
			end;
		end loop;
	end;
	declare -- streaming
		package USIO renames Ada.Streams.Unbounded_Storage_IO;
		X : Lists.List;
		IX : ILists.List;
		Buffer : USIO.Buffer_Type;
	begin
		-- Definite -> Inefinite (0)
		Lists.List'Write (USIO.Stream (Buffer), X); -- write empty
		ILists.Append (IX, 'a');
		pragma Assert (IX.Length = 1);
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		ILists.List'Read (USIO.Stream (Buffer), IX);
		pragma Assert (IX.Length = 0);
		-- Indefinite -> Definite (1)
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		ILists.Append (IX, 'b');
		pragma Assert (IX.Length = 1);
		ILists.List'Write (USIO.Stream (Buffer), IX); -- write 'b'
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		Lists.List'Read (USIO.Stream (Buffer), X);
		pragma Assert (X.Length = 1);
		pragma Assert (Lists.Element (X.First) = 'b');
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end doubly_linked_lists;
