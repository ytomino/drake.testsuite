-- { dg-do run }
with Ada.Real_Time;
with Ada.Synchronous_Task_Control;
with Ada.Synchronous_Task_Control.EDF;
with System.Synchronous_Control;
with System.Tasks;
procedure task_synchronous_control_1 is
	use type System.Synchronous_Control.Yield_Handler;
	
	procedure Check_Single_Task is
		Event : Ada.Synchronous_Task_Control.Suspension_Object;
		State : Boolean;
	begin
		Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline (
			Event,
			Ada.Real_Time.To_Time_Span (0.0),
			State);
		pragma Assert (not State); -- initial state should be false
		pragma Assert (not Ada.Synchronous_Task_Control.Current_State (Event));
		Ada.Synchronous_Task_Control.Set_True (Event);
		Ada.Synchronous_Task_Control.Suspend_Until_True (Event);
		Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline (
			Event,
			Ada.Real_Time.To_Time_Span (0.0),
			State);
		pragma Assert (State);
		pragma Assert (Ada.Synchronous_Task_Control.Current_State (Event));
		Ada.Synchronous_Task_Control.Set_False (Event);
		Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline (
			Event,
			Ada.Real_Time.To_Time_Span (0.0),
			State);
		pragma Assert (not State);
		pragma Assert (not Ada.Synchronous_Task_Control.Current_State (Event));
	end Check_Single_Task;
	
	procedure Check_Multi_Task is
		Event : Ada.Synchronous_Task_Control.Suspension_Object;
		procedure Process (Param : System.Address) is
			pragma Unreferenced (Param);
		begin
			delay 0.1;
			Ada.Synchronous_Task_Control.Set_True (Event);
		end Process;
		Id : System.Tasks.Task_Id;
		State : Boolean;
		Aborted : Boolean;
	begin
		System.Tasks.Create (Id, System.Null_Address, Process'Access);
		Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline (
			Event,
			Ada.Real_Time.To_Time_Span (0.0),
			State);
		pragma Assert (not State);
		Ada.Synchronous_Task_Control.Suspend_Until_True (Event);
		Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline (
			Event,
			Ada.Real_Time.To_Time_Span (0.0),
			State);
		pragma Assert (State);
		System.Tasks.Wait (Id, Aborted => Aborted);
		pragma Assert (not Aborted);
	end Check_Multi_Task;
	
begin
	-- on single-task runtime
	pragma Assert (
		System.Synchronous_Control.Yield_Hook =
		System.Synchronous_Control.Nop'Access);
	Check_Single_Task;
	pragma Assert (
		System.Synchronous_Control.Yield_Hook =
		System.Synchronous_Control.Nop'Access);
	-- on multi-task runtime
	Check_Multi_Task;
	pragma Assert (
		System.Synchronous_Control.Yield_Hook /=
		System.Synchronous_Control.Nop'Access);
	Check_Single_Task;
	pragma Assert (
		System.Synchronous_Control.Yield_Hook /=
		System.Synchronous_Control.Nop'Access);
	pragma Debug (Ada.Debug.Put ("OK"));
end task_synchronous_control_1;
