-- { dg-do link }
-- Please try with sh: ../bin/interrupts & sleep 3 && kill -SIGINT $!
with Ada.Interrupts.Names;
with Ada.Text_IO; use Ada.Text_IO;
procedure interrupts is
	Break : Boolean := False;
	pragma Atomic (Break);
	protected Handlers is
		procedure Handle_Break;
--		pragma Interrupt_Handler (Handle_Break);
	end Handlers;
	protected body Handlers is
		procedure Handle_Break is
		begin
			Put_Line (Standard_Error, "SIGINT");
			Break := True;
		end Handle_Break;
	end Handlers;
begin
	Ada.Interrupts.Unchecked_Attach_Handler (Handlers.Handle_Break'Unrestricted_Access, Ada.Interrupts.Names.SIGINT);
	Put_Line ("press ^C");
	for I in 1 .. 10 loop
		delay 1.0;
		Put('*');
		exit when Break;
	end loop;
	pragma Debug (Ada.Debug.Put ("OK"));
end interrupts;
