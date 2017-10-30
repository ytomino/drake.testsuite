-- { dg-do run }
-- { dg-shouldfail "ADA.ASSERTIONS.ASSERTION_ERROR" }
-- { dg-output "handled\\.\n" }
-- { dg-output ".*\n" }
-- { dg-output "raised ADA\\.ASSERTIONS\\.ASSERTION_ERROR : assert\\.adb:\[0-9\]*\n" }
with Ada.Assertions;
procedure assert is
begin
	pragma Assert (False);
	null;
exception
	when Ada.Assertions.Assertion_Error =>
		Ada.Debug.Put ("handled.");
		Ada.Debug.Put ("ADA.ASSERTIONS.ASSERTION_ERROR is right.");
		Ada.Assertions.Assert (False);
end assert;
