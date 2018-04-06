-- { dg-do run }
with Ada.Numerics.Complex_Types;
with Ada.Streams.Unbounded_Storage_IO;
with Ada.Text_IO.Complex_IO;
with Ada.Text_IO.Text_Streams;
procedure text_io_real_types is
	use Ada.Text_IO;
	use type Ada.Numerics.Complex_Types.Complex;
	type Fixed is delta 0.01 range -99.99 .. 99.99;
	type Decimal is delta 0.01 digits 4;
	package Float_Text_IO is new Ada.Text_IO.Float_IO (Float);
	package Fixed_Text_IO is new Ada.Text_IO.Fixed_IO (Fixed);
	package Decimal_Text_IO is new Ada.Text_IO.Decimal_IO (Decimal);
	package Complex_Text_IO is
		new Ada.Text_IO.Complex_IO (Ada.Numerics.Complex_Types);
begin
	-- for File_Type
	declare
		Buffer : Ada.Streams.Unbounded_Storage_IO.Buffer_Type;
		File : File_Type;
	begin
		-- put
		Text_Streams.Open (
			File,
			Out_File,
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
		declare
			Item : constant Float := 0.25;
		begin
			Float_Text_IO.Put (File, Item, Fore => 3, Aft => 2, Exp => 0);
			Float_Text_IO.Put (File, Item, Fore => 3); -- Default_Exp = 3
			New_Line (File);
		end;
		declare
			Item : constant Fixed := 0.25;
		begin
			pragma Assert (Fixed_Text_IO.Default_Fore = 3);
			Fixed_Text_IO.Put (File, Item); -- Default_Exp = 0
			Fixed_Text_IO.Put (File, Item, Aft => 5, Exp => 3);
			New_Line (File);
		end;
		declare
			Item : constant Decimal := 0.1;
		begin
			pragma Assert (Decimal_Text_IO.Default_Fore = 3);
			Decimal_Text_IO.Put (File, Item); -- Default_Exp = 0
			Decimal_Text_IO.Put (File, Item, Aft => 5, Exp => 3);
			New_Line (File);
		end;
		declare
			Item : constant Ada.Numerics.Complex_Types.Complex := (0.25, 0.5);
		begin
			Complex_Text_IO.Put (File, Item, Fore => 3, Aft => 2, Exp => 0);
			Complex_Text_IO.Put (File, Item, Fore => 3); -- Default_Exp = 3
			New_Line (File);
		end;
		Close (File);
		-- get as values
		Ada.Streams.Unbounded_Storage_IO.Reset (Buffer);
		Text_Streams.Open (
			File,
			In_File,
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
		declare
			Item : Float;
		begin
			Float_Text_IO.Get (File, Item);
			pragma Assert (Item = 0.25);
			Float_Text_IO.Get (File, Item);
			pragma Assert (Item = 0.25);
		end;
		declare
			Item : Fixed;
		begin
			Fixed_Text_IO.Get (File, Item);
			pragma Assert (Item = 0.25);
			Fixed_Text_IO.Get (File, Item);
			pragma Assert (Item = 0.25);
		end;
		declare
			Item : Decimal;
		begin
			Decimal_Text_IO.Get (File, Item);
			pragma Assert (Item = 0.1);
			Decimal_Text_IO.Get (File, Item);
			pragma Assert (Item = 0.1);
		end;
		declare
			Item : Ada.Numerics.Complex_Types.Complex;
		begin
			Complex_Text_IO.Get (File, Item);
			pragma Assert (Item = (0.25, 0.5));
			Complex_Text_IO.Get (File, Item);
			pragma Assert (Item = (0.25, 0.5));
		end;
		pragma Assert (End_Of_Line (File));
		Skip_Line (File);
		pragma Assert (End_Of_File (File));
		Close (File);
		-- get as strings
		Ada.Streams.Unbounded_Storage_IO.Reset (Buffer);
		Text_Streams.Open (
			File,
			In_File,
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
		declare
			Item : constant String := Get_Line (File); -- float
		begin
			pragma Assert (Item = "  0.25  2.50000E-01");
			null;
		end;
		declare
			Item : constant String := Get_Line (File); -- fixed
		begin
			pragma Assert (Item = "  0.25  2.50000E-01");
			null;
		end;
		declare
			Item : constant String := Get_Line (File); -- decimal
		begin
			pragma Assert (Item = "  0.10  1.00000E-01");
			null;
		end;
		declare
			Item : constant String := Get_Line (File); -- complex
		begin
			pragma Assert (Item = "(  0.25,  0.50)(  2.50000E-01,  5.00000E-01)");
			null;
		end;
		pragma Assert (End_Of_File (File));
		Close (File);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end text_io_real_types;
