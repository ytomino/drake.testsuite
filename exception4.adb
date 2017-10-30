-- { dg-do run }
-- { dg-output "handled!\n" }
with Ada;
procedure exception4 is
begin
	raise Program_Error;
exception
	when Program_Error =>
		begin
			raise Program_Error;
		exception
			when Program_Error =>
				Ada.Debug.Put ("handled!");
				pragma Debug (Ada.Debug.Put ("OK"));
		end;
end exception4;
