-- { dg-do run }
with Ada.Storage_Mapped_IO;
procedure storage_mapped_io is
begin
	declare
		P : Ada.Storage_Mapped_IO.Storage_Type;
	begin
		pragma Assert (not Ada.Storage_Mapped_IO.Is_Mapped (P));
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end storage_mapped_io;
