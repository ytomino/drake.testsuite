with Ada.Unchecked_Deallocation;
procedure tasking6 is
begin
	declare
		task T1 is
			entry E1;
			entry E2;
		end T1;
		Flag : Boolean := False;
		The_Exception : exception;
		task body T1 is
		begin
			Ada.Debug.Put ("begin T1");
			accept E1 do
				Ada.Debug.Put ("in E1");
				Flag := True;
			end E1;
			begin
				accept E2 do
					Ada.Debug.Put ("in E2");
					raise The_Exception;
				end E2;
				raise Program_Error;
			exception
				when The_Exception => Ada.Debug.Put ("caught");
			end;
			Ada.Debug.Put ("end T1");
		end T1;
	begin
		Ada.Debug.Put ("*** normal case ***");
		Ada.Debug.Put ("before");
		pragma Assert (not Flag);
		T1.E1;
		pragma Assert (Flag);
		Ada.Debug.Put ("after");
		Ada.Debug.Put ("*** raise in accept ***");
		begin
			T1.E2;
			raise Program_Error;
		exception
			when The_Exception => Ada.Debug.Put ("caught");
		end;
	end;
	declare
		task type T2 is
			entry E1;
			entry E2;
		end T2;
		task body T2 is
		begin
			Ada.Debug.Put ("begin T2");
			accept E1 do
				Ada.Debug.Put ("in E1");
			end E1;
			accept E2 do
				Ada.Debug.Put ("in E2");
			end E2;
			Ada.Debug.Put ("end T2");
		end T2;
		function F return T2 is
		begin
			Ada.Debug.Put ("*** rendezvous in built-in-place ***");
			return Result : T2 do
				Ada.Debug.Put ("before");
				Result.E1;
				Ada.Debug.Put ("after");
			end return;
		end F;
		Z : T2 := F;
	begin
		Ada.Debug.Put ("*** rendezvous returned task ***");
		Ada.Debug.Put ("before");
		Z.E2;
		Ada.Debug.Put ("after");
	end;
	declare
		task type T3 is
			entry E1;
		end T3;
		task body T3 is
		begin
			Ada.Debug.Put ("begin T3");
			accept E1;
			Ada.Debug.Put ("end T3");
		end T3;
		type T3_Access is access T3;
		procedure Free is new Ada.Unchecked_Deallocation (T3, T3_Access);
		P : T3_Access := null;
	begin
		Ada.Debug.Put ("*** rendezvous with dynamic ***");
		P := new T3;
		Ada.Debug.Put ("before");
		P.E1;
		Ada.Debug.Put ("after");
		Free (P);
	end;
	declare
		Flag : Boolean := False;
		task T4 is
			entry E1;
		end T4;
		task body T4 is
		begin
			Ada.Debug.Put ("in T4");
			if Flag then
				accept E1;
			end if;
			delay 1.0;
		end T4;
	begin
		Ada.Debug.Put ("*** ignored rendezvous ***");
		delay 1.0;
		Ada.Debug.Put ("before");
		T4.E1;
		Ada.Debug.Put ("after");
		raise Program_Error;
	exception
		when Tasking_Error => null;
	end;
	declare
		task T5 is
			entry E (Positive range 1 .. 2);
		end T5;
		task body T5 is
		begin
			Ada.Debug.Put ("begin T4");
			accept E (1);
			Ada.Debug.Put ("end T4");
		end T5;
	begin
		Ada.Debug.Put ("*** indexed entries ***");
		Ada.Debug.Put ("before");
		T5.E (1);
		Ada.Debug.Put ("after");
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end tasking6;
