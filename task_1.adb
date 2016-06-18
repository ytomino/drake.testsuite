-- { dg-do run }
with Ada;
with System.Tasks; -- primitives
procedure task_1 is
	Flags : array (1 .. 2) of Boolean := (others => False);
	procedure Process (Param : System.Address) is
		N : aliased Integer;
		for N'Address use Param;
	begin
		delay 0.1;
		Flags (N) := True;
	end Process;
	N1 : aliased constant Integer := 1;
	N2 : aliased constant Integer := 2;
	Id1 : System.Tasks.Task_Id;
	Id2 : System.Tasks.Task_Id;
	Aborted : Boolean;
begin
	System.Tasks.Create (Id1, N1'Address, Process'Access);
	System.Tasks.Create (Id2, N2'Address, Process'Access);
	System.Tasks.Wait (Id1, Aborted => Aborted);
	System.Tasks.Wait (Id2, Aborted => Aborted);
	pragma Assert (Flags (1) and then Flags (2));
	pragma Debug (Ada.Debug.Put ("OK"));
end task_1;
