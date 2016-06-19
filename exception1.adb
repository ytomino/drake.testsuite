-- { dg-do run }
with Ada;
procedure exception1 is
	my_exc : exception;
	pragma Unreferenced (my_exc);
begin
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end exception1;
