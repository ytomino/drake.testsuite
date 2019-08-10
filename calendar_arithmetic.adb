-- { dg-do run }
with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Environment_Variables;
with System.Native_Calendar;
procedure calendar_arithmetic is
	use type Ada.Calendar.Time;
	use type Ada.Calendar.Arithmetic.Day_Count;
	function Is_Leap_Second (T : Ada.Calendar.Time) return Boolean is
		Year : Ada.Calendar.Year_Number;
		Month : Ada.Calendar.Month_Number;
		Day : Ada.Calendar.Day_Number;
		Seconds : Ada.Calendar.Day_Duration;
		Leap_Second : Boolean;
	begin
		Ada.Calendar.Formatting.Split (T,
			Year => Year,
			Month => Month,
			Day => Day,
			Seconds => Seconds,
			Leap_Second => Leap_Second,
			Time_Zone => 0);
		return Leap_Second;
	end Is_Leap_Second;	
	procedure Try (Leap_Second_Enabled : in Boolean) is
		procedure Try_Add (
			Left : in Ada.Calendar.Time;
			Right : in Ada.Calendar.Arithmetic.Day_Count;
			Expected : in Ada.Calendar.Time)
		is
			Result : Ada.Calendar.Time;
		begin
			Result := Left + Right;
			pragma Assert (Result = Expected);
		end Try_Add;
		procedure Try_Diff (
			Left, Right : in Ada.Calendar.Time;
			Days : in Ada.Calendar.Arithmetic.Day_Count;
			Seconds : in Duration;
			Leap_Seconds : in Ada.Calendar.Arithmetic.Leap_Seconds_Count)
		is
			Actual_LS : Ada.Calendar.Arithmetic.Leap_Seconds_Count;
			R_Days : Ada.Calendar.Arithmetic.Day_Count;
			R_Seconds : Duration;
			R_Leap_Seconds : Ada.Calendar.Arithmetic.Leap_Seconds_Count;
		begin
			Ada.Calendar.Arithmetic.Difference (Left, Right,
				Days => R_Days, Seconds => R_Seconds, Leap_Seconds => R_Leap_Seconds);
			pragma Assert (R_Days = Days);
			pragma Assert (R_Seconds = Seconds);
			if Leap_Second_Enabled then
				Actual_LS := Leap_Seconds;
			else
				Actual_LS := 0;
			end if;
			pragma Assert (R_Leap_Seconds = Actual_LS);
			pragma Assert (
				Left =
				Right + 24 * 60 * 60.0 * Duration (Days) + Seconds + Duration (Actual_LS));
		end Try_Diff;
		T1_Image : constant String := "1997-06-30 23:59:59";
		T1 : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Value (T1_Image);
		T2_Image : constant String := "1998-12-31 23:59:59";
		T2 : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Value (T2_Image);
	begin
		pragma Assert (not Is_Leap_Second (T1));
		pragma Assert (not Is_Leap_Second (T2));
		if Leap_Second_Enabled then
			pragma Assert (Is_Leap_Second (T1 + 1.0));
			pragma Assert (Is_Leap_Second (T2 + 1.0));
			null;
		end if;
		-- Addition
		Try_Add (T1, 548, T2 - 24 * 60 * 60.0);
		Try_Add (T2, -548,
			T1 + 24 * 60 * 60.0 + Duration (Boolean'Pos (Leap_Second_Enabled)));
		Try_Add (T1, 549, T2);
		Try_Add (T2, -549, T1);
		Try_Add (T1, 550,
			T2 + 24 * 60 * 60.0 + Duration (Boolean'Pos (Leap_Second_Enabled)));
		Try_Add (T2, -550, T1 - 24 * 60 * 60.0);
		Try_Add (
			T1 - 24 * 60 * 60.0 + 1.0,
			550,
			T2 + 1.0 + Duration (Boolean'Pos (Leap_Second_Enabled)));
		Try_Add (
			T2 + 24 * 60 * 60.0 + 1.0 + Duration (Boolean'Pos (Leap_Second_Enabled)),
			-550,
			T1 + 1.0 + Duration (Boolean'Pos (Leap_Second_Enabled)));
		Try_Add (
			T1 - 24 * 60 * 60.0 + 1.0,
			1,
			T1 + 1.0 + Duration (Boolean'Pos (Leap_Second_Enabled)));
		Try_Add (
			T1 + 1.0 + Duration (Boolean'Pos (Leap_Second_Enabled)),
			-1,
			T1 - 24 * 60 * 60.0 + 1.0);
		if Leap_Second_Enabled then
			Try_Add (T1 + 1.0, 548, T2 - 24 * 60 * 60.0);
			Try_Add (T2 + 1.0, -548, T1 + 24 * 60 * 60.0 + 1.0);
			Try_Add (T1 + 1.0, 549, T2 + 1.0);
			Try_Add (T2 + 1.0, -549, T1 + 1.0);
			Try_Add (T1 + 1.0, 550, T2 + 24 * 60 * 60.0 + 1.0);
			Try_Add (T2 + 1.0, -550, T1 - 24 * 60 * 60.0);
		end if;
		-- Difference
		Try_Diff (T2, T1, Days => 549, Seconds => 0.0, Leap_Seconds => 1);
		Try_Diff (T1, T2, Days => -549, Seconds => 0.0, Leap_Seconds => -1);
		Try_Diff (T2, T1 - 1.0, Days => 549, Seconds => 1.0, Leap_Seconds => 1);
		Try_Diff (T2 + 1.0, T1, Days => 549, Seconds => 1.0, Leap_Seconds => 1);
		Try_Diff (T2 - 1.0, T1, Days => 548, Seconds => 86399.0, Leap_Seconds => 1);
		Try_Diff (
			T2,
			T1 + 1.0 + Duration (Boolean'Pos (Leap_Second_Enabled)), -- next day
			Days => 548, Seconds => 86399.0, Leap_Seconds => 0);
		Try_Diff (
			T2 - 1.0,
			T1 + 1.0 + Duration (Boolean'Pos (Leap_Second_Enabled)), -- next day
			Days => 548, Seconds => 86398.0, Leap_Seconds => 0);
		if Leap_Second_Enabled then
			Try_Diff (
				T2,
				T1 + 1.0, -- just leap second
				Days => 548, Seconds => 86399.0, Leap_Seconds => 1);
			Try_Diff (
				T2 - 1.0,
				T1 + 1.0, -- just leap second
				Days => 548, Seconds => 86398.0, Leap_Seconds => 1);
			Try_Diff (
				T1 + 1.99,
				T1 - 24 * 60 * 60.0 + 1.0,
				Days => 0, Seconds => 86400.99, Leap_Seconds => 0);
		end if;
	end Try;
	UTC_LS_Name : constant String := "/usr/share/zoneinfo/right/GMT";
begin
	Try (Leap_Second_Enabled => False);
	if Ada.Directories.Exists (UTC_LS_Name) then
		Ada.Debug.Put (UTC_LS_Name);
		Ada.Environment_Variables.Set ("TZ", UTC_LS_Name);
		System.Native_Calendar.Initialize_Time_Zones; -- force to reinitialize
		Try (Leap_Second_Enabled => True);
	else
		Ada.Debug.Put (UTC_LS_Name & " is not found.");
	end if;
	pragma Debug (Ada.Debug.Put ("OK"));
end calendar_arithmetic;
