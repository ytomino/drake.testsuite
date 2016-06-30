-- { dg-do run }
with Ada.Real_Time;
with Ada.Synchronous_Task_Control;
with Ada.Synchronous_Task_Control.EDF;
procedure task_synchronous_control_1 is
begin
	-- Synchronous_Task_Control on single task
	declare
		Event : Ada.Synchronous_Task_Control.Suspension_Object;
		State : Boolean;
	begin
		Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline (
			Event,
			Ada.Real_Time.To_Time_Span (0.0),
			State);
		pragma Assert (not State); -- initial state should be false
		Ada.Synchronous_Task_Control.Set_True (Event);
		Ada.Synchronous_Task_Control.Suspend_Until_True (Event);
		Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline (
			Event,
			Ada.Real_Time.To_Time_Span (0.0),
			State);
		pragma Assert (State);
		Ada.Synchronous_Task_Control.Set_False (Event);
		Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline (
			Event,
			Ada.Real_Time.To_Time_Span (0.0),
			State);
		pragma Assert (not State);
	end;
	-- Synchronous_Task_Control
	declare
		ev : Ada.Synchronous_Task_Control.Suspension_Object;
		State : Boolean;
	begin
--		pragma Assert (not Ada.Synchronous_Task_Control.Current_State (ev));
		Ada.Synchronous_Task_Control.Set_True (ev);
		pragma Assert (Ada.Synchronous_Task_Control.Current_State (ev));
		Ada.Synchronous_Task_Control.Suspend_Until_True (ev);
		pragma Assert (Ada.Synchronous_Task_Control.Current_State (ev));
		Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline (
			ev,
			Ada.Real_Time.To_Time_Span (1.0),
			State);
		pragma Assert (State);
		pragma Assert (Ada.Synchronous_Task_Control.Current_State (ev));
		Ada.Synchronous_Task_Control.Set_False (ev);
		pragma Assert (not Ada.Synchronous_Task_Control.Current_State (ev));
		Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline (
			ev,
			Ada.Real_Time.To_Time_Span (1.0),
			State); -- it may be timeout
		pragma Assert (not State);
		pragma Assert (not Ada.Synchronous_Task_Control.Current_State (ev));
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end task_synchronous_control_1;
