-- { dg-do run }
with Ada.IO_Modes;
with Ada.Streams.Unbounded_Storage_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Wide_Text_IO;
with Ada.Wide_Wide_Text_IO;
procedure text_io_characters is
	Buffer : Ada.Streams.Unbounded_Storage_IO.Buffer_Type;
begin
	-- put
	declare
		File : Ada.Text_IO.File_Type;
	begin
		Ada.Text_IO.Text_Streams.Open (
			File,
			Ada.Text_IO.Out_File,
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			External => Ada.IO_Modes.UTF_8);
		declare
			Item : constant Wide_Wide_Character :=
				Wide_Wide_Character'Val (16#7fff_ffff#);
		begin
			Ada.Wide_Wide_Text_IO.Put (File, Item);
		end;
		Ada.Text_IO.Close (File);
	end;
	-- get as UTF-8
	Ada.Streams.Unbounded_Storage_IO.Reset (Buffer);
	declare
		File : Ada.Text_IO.File_Type;
	begin
		Ada.Text_IO.Text_Streams.Open (
			File,
			Ada.Text_IO.In_File,
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			External => Ada.IO_Modes.UTF_8);
		declare
			Item : Character;
		begin
			Ada.Text_IO.Get (File, Item);
			pragma Assert (Item = Character'Val (16#fd#));
			Ada.Text_IO.Get (File, Item);
			pragma Assert (Item = Character'Val (16#bf#));
			Ada.Text_IO.Get (File, Item);
			pragma Assert (Item = Character'Val (16#bf#));
			Ada.Text_IO.Get (File, Item);
			pragma Assert (Item = Character'Val (16#bf#));
			Ada.Text_IO.Get (File, Item);
			pragma Assert (Item = Character'Val (16#bf#));
			Ada.Text_IO.Get (File, Item);
			pragma Assert (Item = Character'Val (16#bf#));
		end;
		pragma Assert (Ada.Text_IO.End_Of_File (File));
		Ada.Text_IO.Close (File);
	end;
	-- get as UTF-16
	Ada.Streams.Unbounded_Storage_IO.Reset (Buffer);
	declare
		File : Ada.Text_IO.File_Type;
	begin
		Ada.Text_IO.Text_Streams.Open (
			File,
			Ada.Text_IO.In_File,
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			External => Ada.IO_Modes.UTF_8);
		declare
			Item : Wide_Character;
		begin
			Ada.Wide_Text_IO.Get (File, Item);
			pragma Assert (Item = Wide_Character'Val (16#dbff#));
			Ada.Wide_Text_IO.Get (File, Item);
			pragma Assert (Item = Wide_Character'Val (16#dfff#));
		end;
		pragma Assert (Ada.Text_IO.End_Of_File (File));
		Ada.Text_IO.Close (File);
	end;
	-- get as UTF-32
	Ada.Streams.Unbounded_Storage_IO.Reset (Buffer);
	declare
		File : Ada.Text_IO.File_Type;
	begin
		Ada.Text_IO.Text_Streams.Open (
			File,
			Ada.Text_IO.In_File,
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			External => Ada.IO_Modes.UTF_8);
		declare
			Item : Wide_Wide_Character;
		begin
			Ada.Wide_Wide_Text_IO.Get (File, Item);
			pragma Assert (Item = Wide_Wide_Character'Val (16#7fff_ffff#));
		end;
		pragma Assert (Ada.Text_IO.End_Of_File (File));
		Ada.Text_IO.Close (File);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end text_io_characters;
