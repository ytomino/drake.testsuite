-- { dg-do run }
with Ada.Real_Time;
with Ada.Synchronous_Task_Control.EDF;
procedure task_synchronous_control_abort is
begin
	--  abort Suspend_Until_True
	declare
		ev : Ada.Synchronous_Task_Control.Suspension_Object;
		T1_State : Positive := 1;
		task T1;
		task body T1 is
		begin
			T1_State := 2;
			Ada.Synchronous_Task_Control.Suspend_Until_True (ev);
			T1_State := 3;
			raise Program_Error; -- it does not come here
		end T1;
	begin
		delay 0.1;
		pragma Assert (T1_State = 2);
		abort T1;
		delay 0.1;
		pragma Assert (T1'Terminated);
		pragma Assert (T1_State = 2);
	end;
	--  abort Suspend_Until_True_And_Set_Deadline
	declare
		ev : Ada.Synchronous_Task_Control.Suspension_Object;
		T2_State : Positive := 1;
		task T2;
		task body T2 is
		begin
			T2_State := 2;
			Ada.Synchronous_Task_Control.EDF.Suspend_Until_True_And_Set_Deadline (
				ev,
				Ada.Real_Time.To_Time_Span (1.0));
			T2_State := 3;
			raise Program_Error; -- it does not come here
		end T2;
	begin
		delay 0.1;
		pragma Assert (T2_State = 2);
		abort T2;
		delay 0.1;
		pragma Assert (T2'Terminated);
		pragma Assert (T2_State = 2);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end task_synchronous_control_abort;
