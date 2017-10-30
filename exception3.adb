-- { dg-do run }
-- { dg-shouldfail "PROGRAM_ERROR" }
-- { dg-output "handled\\.\\.\\. and reraising\\.\\.\\.\n" }
-- { dg-output ".*\n" }
-- { dg-output "raised PROGRAM_ERROR : exception3\\.adb:\[0-9\]* explicit raise\n" }
with Ada;
procedure exception3 is
begin
	raise Program_Error;
exception
	when E : Program_Error =>
		pragma Unreferenced (E);
		Ada.Debug.Put ("handled... and reraising...");
		raise;
end exception3;
