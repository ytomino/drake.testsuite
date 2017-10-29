-- { dg-do compile }
with Ada.Calendar;
with Ada.Real_Time;
procedure task_select is
	use type Ada.Calendar.Time;
	use type Ada.Real_Time.Time;
begin
	-- Timed_Task_Entry_Call
	declare
		task T1 is
			entry E1;
		end T1;
		task body T1 is
		begin
			loop
				accept E1;
			end loop;
		end T1;
	begin
		select
			T1.E1;
		or
			delay 1.0;
		end select;
		select
			T1.E1;
		or
			delay until Ada.Calendar.Clock + 1.0;
		end select;
		select
			T1.E1;
		or
			delay until Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (1.0);
		end select;
		abort T1;
	end;
	-- Timed_Protected_Entry_Call
	declare
		protected P2 is
			entry E2;
		end P2;
		protected body P2 is
			entry E2 when True is
			begin
				null;
			end E2;
		end P2;
	begin
		select
			P2.E2;
		or
			delay 1.0;
		end select;
		select
			P2.E2;
		or
			delay until Ada.Calendar.Clock + 1.0;
		end select;
		select
			P2.E2;
		or
			delay until Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (1.0);
		end select;
	end;
	-- Timed_Selective_Wait
	declare
		task T3 is
			entry E3;
		end T3;
		task body T3 is
		begin
			select
				accept E3;
			or
				delay 1.0;
			end select;
			select
				accept E3;
			or
				delay until Ada.Calendar.Clock + 1.0;
			end select;
			select
				accept E3;
			or
				delay until Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (1.0);
			end select;
		end T3;
	begin
		T3.E3;
		T3.E3;
		T3.E3;
		abort T3;
	end;
end task_select;
