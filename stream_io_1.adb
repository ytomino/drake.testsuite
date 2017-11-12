-- { dg-do run }
with Ada.Streams.Stream_IO;
procedure stream_io_1 is
	use Ada.Streams.Stream_IO;
begin
	declare
		File : File_Type;
	begin
		pragma Assert (not Is_Open (File));
		null;
	end;
	declare -- Mode
		File : File_Type;
	begin
		for I in File_Mode loop
			Create (File, I);
			pragma Assert (Mode (File) = I);
			Close (File);
		end loop;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end stream_io_1;
