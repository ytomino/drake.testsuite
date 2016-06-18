-- { dg-do run }
-- { dg-output "handled!$" }
with Ada;
procedure exception2 is
begin
	raise Program_Error;
exception
	when Program_Error =>
		Ada.Debug.Put ("handled!");
		pragma Debug (Ada.Debug.Put ("OK"));
end exception2;
