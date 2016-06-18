-- { dg-do run }
with Ada;
procedure task_protected_count is
	Caller_Count : Natural := 3;
	-- callee
	protected Callee is
		entry Start;
		entry Bottleneck;
	private
		Started : Boolean := False;
	end Callee;
	protected body Callee is
		entry Start when True is
		begin
			Started := True;
		end Start;
		entry Bottleneck when Started is
			Count : constant Natural := Bottleneck'Count;
		begin
			Caller_Count := Caller_Count - 1;
			pragma Assert (Count = Caller_Count);
		end Bottleneck;
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
end task_protected_count;
