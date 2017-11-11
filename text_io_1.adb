-- { dg-do run }
with Ada.Text_IO;
procedure text_io_1 is
	use Ada.Text_IO;
begin
	declare
		File : File_Type;
	begin
		pragma Assert (not Is_Open (File));
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end text_io_1;
