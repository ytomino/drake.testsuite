-- { dg-do run }
-- { dg-final scan-file-not "\bsystem\.tasks\b" "b~protected_rw.ads" }
with Ada;
procedure protected_rw is
	protected RW is
		procedure Sync_Proc (Value : Integer);
		function Sync_Func return Integer;
	private
		Value : Integer := 0;
	end RW;
	protected body RW is
		procedure Sync_Proc (Value : Integer) is
		begin
			RW.Value := Value;
		end Sync_Proc;
		function Sync_Func return Integer is
		begin
			return Value;
		end Sync_Func;
	end RW;
	Value : Integer;
begin
	RW.Sync_Proc (10);
	Value := RW.Sync_Func;
	pragma Assert (Value = 10);
	pragma Debug (Ada.Debug.Put ("OK"));
end protected_rw;
