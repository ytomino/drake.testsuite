-- { dg-do run }
with Ada.Processes;
procedure processes is
begin
	declare
		P : Ada.Processes.Process;
	begin
		pragma Assert (not Ada.Processes.Is_Open (P));
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end processes;
