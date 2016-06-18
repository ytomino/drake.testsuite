-- { dg-do run }
with Ada;
procedure task_count is
	Caller_Count : Natural := 3;
	-- callee
	task Callee is
		entry Start;
		entry Bottleneck;
	end Callee;
	task body Callee is
	begin
		accept Start;
		while Caller_Count > 0 loop
			accept Bottleneck do
				declare
					Count : constant Natural := Bottleneck'Count;
				begin
					Caller_Count := Caller_Count - 1;
					pragma Assert (Count = Caller_Count);
				end;
			end Bottleneck;
		end loop;
	end Callee;
	-- callers
	task type Caller is
	end Caller;
	task body Caller is
	begin
		Callee.Bottleneck;
	end Caller;
	Callers : array (1 .. Caller_Count) of Caller;
begin
	delay 0.1;
	Callee.Start;
	delay 0.1;
	pragma Assert (Caller_Count = 0);
	pragma Debug (Ada.Debug.Put ("OK"));
end task_count;
