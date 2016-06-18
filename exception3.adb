-- { dg-do run }
-- { dg-shouldfail }
-- { dg-output "handled\.\.\. and reraising\.\.\.$" }
-- { dg-output "^raised PROGRAM_ERROR : exception3\.adb:[0-9]+ explicit raise$" }
with Ada;
procedure exception3 is
begin
	raise Program_Error;
exception
	when E : Program_Error =>
		Ada.Debug.Put ("handled... and reraising...");
		raise;
end exception3;
