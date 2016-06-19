-- { dg-do run }
with Ada.Asynchronous_Task_Control;
with Ada.Task_Attributes;
with Ada.Task_Identification;
with Ada.Unchecked_Conversion;
with System.Tasks;
procedure task_attributes is
	Count : constant := 3;
	Flags : array (1 .. Count) of Boolean := (others => False);
	function Cast is new Ada.Unchecked_Conversion (
		System.Tasks.Task_Id,
		Ada.Task_Identification.Task_Id);
	package Attr is new Ada.Task_Attributes (Integer, 0);
	procedure Process (Param : System.Address) is
		pragma Unreferenced (Param);
	begin
		delay 0.1;
		Flags (Attr.Value) := True;
	end Process;
	Ts : array (1 .. Count) of System.Tasks.Task_Id;
	Aborted : Boolean;
begin
	for I in Ts'Range loop
		System.Tasks.Create (Ts (I), System.Null_Address, Process'Access);
		Attr.Set_Value (I, T => Cast (Ts (I)));
	end loop;
	for I in Ts'Range loop
		System.Tasks.Wait (Ts (I), Aborted => Aborted);
	end loop;
	pragma Assert (for all I in Flags'Range => Flags (I));
	pragma Debug (Ada.Debug.Put ("OK"));
end task_attributes;
