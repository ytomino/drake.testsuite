-- { dg-do run }
with Ada.Sequential_IO;
procedure sequential_io_1 is
	package Character_IO is new Ada.Sequential_IO (Character);
begin
	declare
		use Character_IO;
		File : File_Type;
	begin
		pragma Assert (not Is_Open (File));
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end sequential_io_1;
