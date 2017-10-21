-- { dg-do run }
-- { dg-shouldfail }
-- { dg-output "^STDOUT$" }
with Ada.Text_IO;
procedure text_io_autoflush is
	use Ada.Text_IO;
begin
	Put_Line ("STDOUT"); -- should be outputted even if redirected
	raise Program_Error;
end text_io_autoflush;
