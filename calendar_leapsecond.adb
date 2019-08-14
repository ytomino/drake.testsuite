-- { dg-do run }
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Directories;
with Ada.Environment_Variables;
with System.Native_Calendar;
procedure calendar_leapsecond is
	use type Ada.Calendar.Time;
	use type Ada.Calendar.Time_Zones.Time_Offset;
	procedure Try_UTC_Time_Offset (
		T : Ada.Calendar.Time;
		TZ_Offset : Ada.Calendar.Time_Zones.Time_Offset) is
	begin
		pragma Assert (Ada.Calendar.Time_Zones.UTC_Time_Offset (T) = TZ_Offset);
		null;
	end Try_UTC_Time_Offset;
	procedure Try_Seconds (
		T : Ada.Calendar.Time;
		TZ_Offset : Ada.Calendar.Time_Zones.Time_Offset;
		GMT_Seconds : Ada.Calendar.Day_Duration;
		Local_Seconds : Ada.Calendar.Day_Duration)
	is
		Year : Ada.Calendar.Year_Number;
		Month : Ada.Calendar.Month_Number;
		Day : Ada.Calendar.Day_Number;
		Seconds : Ada.Calendar.Day_Duration;
		Leap_Second : Boolean;
	begin
		Ada.Calendar.Formatting.Split (T,
			Year => Year, Month => Month, Day => Day, Seconds => Seconds,
			Leap_Second => Leap_Second, Time_Zone => 0);
		pragma Assert (Seconds = GMT_Seconds);
		pragma Assert (
			Ada.Calendar.Formatting.Time_Of (
				Year => Year, Month => Month, Day => Day, Seconds => Seconds,
				Leap_Second => Leap_Second, Time_Zone => 0) =
			T);
		Ada.Calendar.Formatting.Split (T,
			Year => Year, Month => Month, Day => Day, Seconds => Seconds,
			Leap_Second => Leap_Second, Time_Zone => TZ_Offset);
		pragma Assert (Seconds = Local_Seconds);
		pragma Assert (
			Ada.Calendar.Formatting.Time_Of (
				Year => Year, Month => Month, Day => Day, Seconds => Seconds,
				Leap_Second => Leap_Second, Time_Zone => TZ_Offset) =
			T);
	end Try_Seconds;
	procedure Try_Image (
		T : Ada.Calendar.Time;
		TZ_Offset : Ada.Calendar.Time_Zones.Time_Offset;
		Second : Ada.Calendar.Formatting.Second_Number'Base)
	is
		GMT_Image : constant String :=
			Ada.Calendar.Formatting.Image (T, Time_Zone => 0);
		Local_Image : constant String :=
			Ada.Calendar.Formatting.Image (T, Time_Zone => TZ_Offset);
	begin
		pragma Assert (
			Ada.Calendar.Formatting.Value (GMT_Image, Time_Zone => 0) = T);
		pragma Assert (
			Ada.Calendar.Formatting.Value (Local_Image, Time_Zone => TZ_Offset) =
			T);
		pragma Assert (Integer'Value (GMT_Image (18 .. 19)) = Second);
		pragma Assert (Integer'Value (Local_Image (18 .. 19)) = Second);
		null;
	end Try_Image;
	procedure Try_TZ (
		TZ_Name : String;
		TZ_Offset : Ada.Calendar.Time_Zones.Time_Offset;
		Local_Year : Ada.Calendar.Year_Number;
		Local_Month : Ada.Calendar.Month_Number;
		Local_Day : Ada.Calendar.Day_Number;
		Local_Seconds : Ada.Calendar.Day_Duration;
		Local_Hour : Ada.Calendar.Formatting.Hour_Number;
		Local_Minute : Ada.Calendar.Formatting.Minute_Number) is
	begin
		if Ada.Directories.Exists (TZ_Name) then
			Ada.Debug.Put (TZ_Name);
			Ada.Environment_Variables.Set ("TZ", TZ_Name);
			System.Native_Calendar.Initialize_Time_Zones; -- force to reinitialize
			declare -- -2
				The_Prev_2_Time : constant Ada.Calendar.Time :=
					Ada.Calendar.Time_Of (1998, 12, 31, 86398.0);
			begin
				Try_UTC_Time_Offset (The_Prev_2_Time, TZ_Offset);
				Try_Seconds (The_Prev_2_Time, TZ_Offset,
					86398.0, Local_Seconds - 1.0);
				Try_Image (The_Prev_2_Time, TZ_Offset, 58);
			end;
			declare -- -1
				The_Prev_Time : constant Ada.Calendar.Time :=
					Ada.Calendar.Time_Of (1998, 12, 31, 86399.0);
			begin
				Try_UTC_Time_Offset (The_Prev_Time, TZ_Offset);
				Try_Seconds (The_Prev_Time, TZ_Offset, 86399.0, Local_Seconds);
				Try_Image (The_Prev_Time, TZ_Offset, 59);
			end;
			declare
				The_LS_Time : constant Ada.Calendar.Time :=
					Ada.Calendar.Time_Of (1999, 1, 1, 0.0) - 1.0;
			begin
				Try_UTC_Time_Offset (The_LS_Time, TZ_Offset);
				Try_Seconds (The_LS_Time, TZ_Offset, 86399.0, Local_Seconds);
				-- split/time_of
				declare
					Year : Ada.Calendar.Year_Number;
					Month : Ada.Calendar.Month_Number;
					Day : Ada.Calendar.Day_Number;
					Seconds : Ada.Calendar.Day_Duration;
					Hour : Ada.Calendar.Formatting.Hour_Number;
					Minute : Ada.Calendar.Formatting.Minute_Number;
					Second : Ada.Calendar.Formatting.Second_Number;
					Sub_Second : Ada.Calendar.Formatting.Second_Duration;
					Leap_Second : Boolean;
				begin
					-- Calendar
					Ada.Calendar.Split (
						The_LS_Time,
						Year => Year,
						Month => Month,
						Day => Day,
						Seconds => Seconds);
					pragma Assert (Year = 1998);
					pragma Assert (Month = 12);
					pragma Assert (Day = 31);
					pragma Assert (Seconds = 86399.0); -- not including LS
					pragma Assert (Ada.Calendar.Seconds (The_LS_Time) = 86399.0);
					pragma Assert (
						Ada.Calendar.Time_Of (1998, 12, 31, 86399.0) + 1.0 =
						The_LS_Time);
					-- Formatting (Seconds)
					Ada.Calendar.Formatting.Split (
						The_LS_Time,
						Year => Year,
						Month => Month,
						Day => Day,
						Seconds => Seconds,
						Leap_Second => Leap_Second,
						Time_Zone => 0);
					pragma Assert (Year = 1998);
					pragma Assert (Month = 12);
					pragma Assert (Day = 31);
					pragma Assert (Seconds = 86399.0); -- not including LS
					pragma Assert (Leap_Second);
					pragma Assert (
						Ada.Calendar.Formatting.Seconds (The_LS_Time,
							Time_Zone => 0) =
						86399.0);
					pragma Assert (
						Ada.Calendar.Formatting.Time_Of (1998, 12, 31, 86399.0,
							Leap_Second => True, Time_Zone => 0) =
						The_LS_Time);
					Ada.Calendar.Formatting.Split (
						The_LS_Time,
						Year => Year,
						Month => Month,
						Day => Day,
						Seconds => Seconds,
						Leap_Second => Leap_Second,
						Time_Zone => TZ_Offset);
					pragma Assert (Year = Local_Year);
					pragma Assert (Month = Local_Month);
					pragma Assert (Day = Local_Day);
					pragma Assert (Seconds = Local_Seconds); -- not including LS
					pragma Assert (Leap_Second);
					pragma Assert (
						Ada.Calendar.Formatting.Seconds (The_LS_Time,
							Time_Zone => TZ_Offset) =
						Local_Seconds);
					pragma Assert (
						Ada.Calendar.Formatting.Time_Of (
							Local_Year, Local_Month, Local_Day, Local_Seconds,
							Leap_Second => True, Time_Zone => TZ_Offset) =
						The_LS_Time);
					-- Formatting (Hour/Minute/Second/Sub_Second)
					Ada.Calendar.Formatting.Split (
						The_LS_Time,
						Year => Year,
						Month => Month,
						Day => Day,
						Hour => Hour,
						Minute => Minute,
						Second => Second,
						Sub_Second => Sub_Second,
						Leap_Second => Leap_Second,
						Time_Zone => 0);
					pragma Assert (Year = 1998);
					pragma Assert (Month = 12);
					pragma Assert (Day = 31);
					pragma Assert (Hour = 23);
					pragma Assert (Minute = 59);
					pragma Assert (Second = 59);
					pragma Assert (Sub_Second = 0.0);
					pragma Assert (Leap_Second);
					pragma Assert (
						Ada.Calendar.Formatting.Time_Of (
							1998, 12, 31, 23, 59, 59, 0.0,
							Leap_Second => True, Time_Zone => 0) =
						The_LS_Time);
					Ada.Calendar.Formatting.Split (
						The_LS_Time,
						Year => Year,
						Month => Month,
						Day => Day,
						Hour => Hour,
						Minute => Minute,
						Second => Second,
						Sub_Second => Sub_Second,
						Leap_Second => Leap_Second,
						Time_Zone => TZ_Offset);
					pragma Assert (Year = Local_Year);
					pragma Assert (Month = Local_Month);
					pragma Assert (Day = Local_Day);
					pragma Assert (Hour = Local_Hour);
					pragma Assert (Minute = Local_Minute);
					pragma Assert (Second = 59);
					pragma Assert (Sub_Second = 0.0);
					pragma Assert (Leap_Second);
					pragma Assert (
						Ada.Calendar.Formatting.Time_Of (
							Local_Year, Local_Month, Local_Day,
							Local_Hour, Local_Minute, 59, 0.0,
							Leap_Second => True, Time_Zone => TZ_Offset) =
						The_LS_Time);
				end;
				Try_Image (The_LS_Time, TZ_Offset, 60);
			end;
			declare -- +1
				The_Next_Time : constant Ada.Calendar.Time :=
					Ada.Calendar.Time_Of (1999, 1, 1, 0.0);
			begin
				Try_UTC_Time_Offset (The_Next_Time, TZ_Offset);
				Try_Seconds (The_Next_Time, TZ_Offset, 0.0, Local_Seconds + 1.0);
				Try_Image (The_Next_Time, TZ_Offset, 0);
			end;
			declare -- +2
				The_Next_2_Time : constant Ada.Calendar.Time :=
					Ada.Calendar.Time_Of (1999, 1, 1, 1.0);
			begin
				Try_UTC_Time_Offset (The_Next_2_Time, TZ_Offset);
				Try_Seconds (The_Next_2_Time, TZ_Offset, 1.0, Local_Seconds + 2.0);
				Try_Image (The_Next_2_Time, TZ_Offset, 1);
			end;
		else
			Ada.Debug.Put (TZ_Name & " is not found.");
		end if;
	end Try_TZ;
begin
	Try_TZ (
		"/usr/share/zoneinfo/right/Japan", 9 * 60,
		1999, 1, 1, 32399.0, 8, 59);
	Try_TZ (
		"/usr/share/zoneinfo/right/Europe/Paris", 1 * 60,
		1999, 1, 1, 3599.0, 0, 59);
	Try_TZ (
		"/usr/share/zoneinfo/right/Etc/GMT+1", -1 * 60,
		1998, 12, 31, 82799.0, 22, 59);
	Try_TZ (
		"/usr/share/zoneinfo/right/US/Hawaii", -10 * 60,
		1998, 12, 31, 50399.0, 13, 59);
	pragma Debug (Ada.Debug.Put ("OK"));
end calendar_leapsecond;
