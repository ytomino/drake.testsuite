-- { dg-do run }
with Ada.Calendar;
with Ada.Dispatching;
with System.Tasks;
procedure task_abort is
	use type Ada.Calendar.Time;
begin
	-- break delay
	declare
		T2_State : Positive := 1;
		task T2;
		task body T2 is
		begin
			-- force switching to environment task
			for I in 1 .. 500 loop
				Ada.Dispatching.Yield;
			end loop;
			-- before wait in task
			delay until Ada.Calendar.Clock + 999.9;
			T2_State := 2; -- after wait in task
			raise Program_Error; -- it does not come here
		end T2;
	begin
		-- before abort
		abort T2;
		-- after abort
		pragma Assert (T2_State = 1);
	end;
	-- break a task having a child task
	declare
		T3_State : Positive := 1;
		T3_Child_State : Positive := 1;
		task T3;
		task body T3 is
			task T3_Child is
			end T3_Child;
			task body T3_Child is
			begin
				T3_Child_State := 2; -- before wait in nested task
				delay 999.9;
				T3_Child_State := 3; -- after wait in nested task
				raise Program_Error; -- it does not come here
			exception
				when Standard'Abort_Signal =>
					System.Tasks.When_Abort_Signal;
					T3_Child_State := 4; -- aborted in nested task
					raise;
			end T3_Child;
		begin
			T3_State := 2; -- before wait in task
			delay 999.9;
			T3_State := 3; -- after wait in task
			raise Program_Error; -- it does not come here
		exception
			when Standard'Abort_Signal =>
				System.Tasks.When_Abort_Signal;
				T3_State := 4; -- aborted in task
				raise;
		end T3;
	begin
		delay 0.1;
		-- before abort
		pragma Assert (T3_State = 2);
		pragma Assert (T3_Child_State = 2);
		abort T3;
		delay 0.1;
		-- after abort
		pragma Assert (T3_State = 4);
		pragma Assert (T3_Child_State = 4);
	end;
	-- break a task waiting an other tasks
	declare
		T4_State : Positive := 1;
		T4_Child_State : Positive := 1;
		task T4;
		task body T4 is
		begin
			declare
				task T4_Child is
				end T4_Child;
				task body T4_Child is
				begin
					T4_Child_State := 2; -- before wait in nested task
					delay 999.9;
					T4_Child_State := 3; -- after wait in nested task
					raise Program_Error; -- it does not come here
				exception
					when Standard'Abort_Signal =>
						System.Tasks.When_Abort_Signal;
						T4_Child_State := 4; -- aborted in nested task
						raise;
				end T4_Child;
			begin
				T4_State := 2; -- before wait in task
			end; -- wait T4_Child in Leave_Master called from here
			T4_State := 3; -- after wait in task
			delay 0.0; -- abort checking
			T4_State := 4;
			raise Program_Error; -- it does not come here
		exception
			when Standard'Abort_Signal =>
				System.Tasks.When_Abort_Signal;
				T4_State := 5; -- aborted in task
				raise;
		end T4;
	begin
		delay 0.1; -- wait until T4 has waited T4_Child
		-- before abort
		pragma Assert (T4_State = 2);
		pragma Assert (T4_Child_State = 2);
		abort T4;
		delay 0.1;
		-- after abort
		pragma Assert (T4_State = 5);
		pragma Assert (T4_Child_State = 4);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end task_abort;
