-- { dg-do run }
with Ada.Synchronous_Task_Control;
with System.Native_Tasks.Yield;
procedure task_synchronous_control_abort is
begin
	Ada.Debug.Put ("**** break Suspend_Until_True ****");
	declare
		ev : Ada.Synchronous_Task_Control.Suspension_Object;
		task T1;
		task body T1 is
		begin
			-- force switching to environment task
			for I in 1 .. 500 loop
				System.Native_Tasks.Yield;
			end loop;
			Ada.Debug.Put ("before wait in task");
			Ada.Synchronous_Task_Control.Suspend_Until_True (ev);
			Ada.Debug.Put ("after wait in task");
			raise Program_Error; -- it does not come here
		end T1;
	begin
		Ada.Debug.Put ("before abort");
		abort T1;
		Ada.Debug.Put ("after abort");
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end task_synchronous_control_abort;
