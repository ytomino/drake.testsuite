-- { dg-do run }
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Limited_Ordered_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Iterator_Interfaces;
with Ada.Streams.Unbounded_Storage_IO;
procedure ordered_maps is
	use type Ada.Containers.Count_Type;
	package Maps is new Ada.Containers.Ordered_Maps (Character, Integer);
	package IMaps is new Ada.Containers.Indefinite_Ordered_Maps (Character, Integer);
	package LMaps is new Ada.Containers.Limited_Ordered_Maps (Character, Integer);
begin
	declare -- Insert
		X : Maps.Map;
	begin
		Maps.Insert (X, 'B', 15);
		pragma Assert (X.Length = 1);
		pragma Assert (Maps.Key (X.First) = 'B');
		pragma Assert (Maps.Key (X.Last) = 'B');
		pragma Assert (Maps.Element (X.First) = 15);
		pragma Assert (Maps.Element (X.Last) = 15);
		Maps.Insert (X, 'A', 10);
		pragma Assert (X.Length = 2);
		pragma Assert (Maps.Key (X.First) = 'A');
		pragma Assert (Maps.Key (X.Last) = 'B');
		pragma Assert (Maps.Element (X.First) = 10);
		pragma Assert (Maps.Element (X.Last) = 15);
		Maps.Insert (X, 'C', 20);
		pragma Assert (X.Length = 3);
		pragma Assert (Maps.Key (X.First) = 'A');
		pragma Assert (Maps.Key (X.Last) = 'C');
		pragma Assert (Maps.Element (X.First) = 10);
		pragma Assert (Maps.Element (X.Last) = 20);
	end;
	declare -- Iterate
		use type Maps.Cursor;
		X : aliased Maps.Map;
		C : Character;
	begin
		for I in Character'('A') .. 'F' loop
			Maps.Insert (X, I, Character'Pos (I));
		end loop;
		C := 'A';
		declare
			I : Maps.Cursor := X.First;
		begin
			while Maps.Has_Element (I) loop
				pragma Assert (Maps.Key (I) = C);
				C := Character'Succ (C);
				Maps.Next (I);
			end loop;
		end;
		declare
			I : Maps.Cursor := X.Last;
		begin
			while Maps.Has_Element (I) loop
				C := Character'Pred (C);
				pragma Assert (Maps.Key (I) = C);
				Maps.Previous (I);
			end loop;
		end;
		declare
			I : Maps.Cursor := X.First;
			Last : Maps.Cursor := X.Last;
		begin
			if Maps.Has_Element (I) then
				loop
					pragma Assert (Maps.Key (I) = C);
					C := Character'Succ (C);
					exit when I = Last;
					Maps.Next (I);
				end loop;
			end if;
		end;
		declare
			I : Maps.Cursor := X.Last;
			First : Maps.Cursor := X.First;
		begin
			if Maps.Has_Element (I) then
				loop
					C := Character'Pred (C);
					pragma Assert (Maps.Key (I) = C);
					exit when I = First;
					Maps.Previous (I);
				end loop;
			end if;
		end;
		Maps.Clear (X);
		-- forward iteration
		declare
			Ite : Maps.Map_Iterator_Interfaces.Reversible_Iterator'Class := X.Iterate;
			Pos : Maps.Cursor := Maps.Map_Iterator_Interfaces.First (Ite);
		begin
			while Pos /= Maps.No_Element loop
				pragma Assert (False);
				Pos := Maps.Map_Iterator_Interfaces.Next (Ite, Pos);
			end loop;
		end;
		-- reverse iteration
		declare
			Ite : Maps.Map_Iterator_Interfaces.Reversible_Iterator'Class := X.Iterate;
			Pos : Maps.Cursor := Maps.Map_Iterator_Interfaces.Last (Ite);
		begin
			while Pos /= Maps.No_Element loop
				pragma Assert (False);
				Pos := Maps.Map_Iterator_Interfaces.Previous (Ite, Pos);
			end loop;
		end;
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
				Maps.Cursor,
				Maps.No_Element,
				Maps.Map,
				Maps.First,
				Maps.Last,
				Maps.Map_Iterator_Interfaces,
				Maps.Iterate);
		procedure IIR is
			new Iterate_Invalid_Ranges (
				IMaps.Cursor,
				IMaps.No_Element,
				IMaps.Map,
				IMaps.First,
				IMaps.Last,
				IMaps.Map_Iterator_Interfaces,
				IMaps.Iterate);
		procedure IIR is
			new Iterate_Invalid_Ranges (
				LMaps.Cursor,
				LMaps.No_Element,
				LMaps.Map,
				LMaps.First,
				LMaps.Last,
				LMaps.Map_Iterator_Interfaces,
				LMaps.Iterate);
		C : Maps.Map;
		IC : IMaps.Map;
		LC : LMaps.Map;
	begin
		for Count in 0 .. 1 loop
			IIR (C);
			IIR (IC);
			IIR (LC);
			declare
				function New_Key return Character is
				begin
					return Character'Val (Character'Pos ('x') + Count);
				end New_Key;
				function New_Element return Integer is
				begin
					return 123 + Count;
				end New_Element;
			begin
				Maps.Insert (C, New_Key, New_Element);
				IMaps.Insert (IC, New_Key, New_Element);
				LMaps.Insert (LC, New_Key'Access, New_Element'Access);
			end;
		end loop;
	end;
	declare -- streaming
		package USIO renames Ada.Streams.Unbounded_Storage_IO;
		X : Maps.Map;
		IX : IMaps.Map;
		Buffer : USIO.Buffer_Type;
	begin
		-- Definite -> Inefinite (0)
		Maps.Map'Write (USIO.Stream (Buffer), X); -- write empty
		IMaps.Insert (IX, '#', 9);
		pragma Assert (IX.Length = 1);
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		IMaps.Map'Read (USIO.Stream (Buffer), IX);
		pragma Assert (IX.Length = 0);
		-- Indefinite -> Definite (1)
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		IMaps.Insert (IX, '$', 10);
		pragma Assert (IX.Length = 1);
		IMaps.Map'Write (USIO.Stream (Buffer), IX); -- write 'b'
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (USIO.Stream (Buffer).all), 1);
		Maps.Map'Read (USIO.Stream (Buffer), X);
		pragma Assert (X.Length = 1);
		pragma Assert (Maps.Element (X.First) = 10);
		pragma Assert (X.Element ('$') = 10);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end ordered_maps;
