-- { dg-do run }
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Containers.Generic_Sort;
procedure sort_thresholds is
	package AC renames Ada.Containers;
	generic
		type Idx is range <>;
	procedure Generic_Try;
	procedure Generic_Try is
	begin
		for Length in Idx range 1 .. 5 loop
			declare -- Integer
				type T is array (Idx range <>) of Character;
				Unsorted, Sorted : T (1 .. Length);
			begin
				for I in 1 .. Length loop
					Unsorted (I) :=
						Character'Val (Character'Pos ('A') + Length - I);
					Sorted (I) := Character'Val (Character'Pos ('A') - 1 + I);
				end loop;
				declare -- Generic_Array_Sort
					procedure Sort is
						new AC.Generic_Array_Sort (Idx, Character, T);
					X : T (Idx'First .. Idx'First + (Length - 1)) := Unsorted;
					Y : T (Idx'Last - (Length - 1) .. Idx'Last) := Unsorted;
				begin
					Sort (X);
					pragma Assert (X = Sorted);
					Sort (Y);
					pragma Assert (Y = Sorted);
				end;
				declare -- Generic_Constrained_Array_Sort
					subtype I_F is Idx range Idx'First .. Idx'First + (Length - 1);
					subtype T_F is T (I_F);
					procedure Sort_F is
						new AC.Generic_Constrained_Array_Sort (I_F, Character, T_F);
					subtype I_L is Idx range Idx'Last - (Length - 1) .. Idx'Last;
					subtype T_L is T (I_L);
					procedure Sort_L is
						new AC.Generic_Constrained_Array_Sort (I_L, Character, T_L);
					X : T_F := Unsorted;
					Y : T_L := Unsorted;
				begin
					Sort_F (X);
					pragma Assert (X = Sorted);
					Sort_L (Y);
					pragma Assert (Y = Sorted);
				end;
				declare -- Generic_Sort
					function Before_F (I, J : Idx) return Boolean;
					procedure Swap_F (I, J : Idx);
					procedure Sort_F is new AC.Generic_Sort (Idx, Before_F, Swap_F);
					X : T (Idx'First .. Idx'First + (Length - 1)) := Unsorted;
					function Before_F (I, J : Idx) return Boolean is
					begin
						return X (I) < X (J);
					end Before_F;
					procedure Swap_F (I, J : Idx) is
						Temp : constant Character := X (I);
					begin
						X (I) := X (J);
						X (J) := Temp;
					end Swap_F;
					function Before_L (I, J : Idx) return Boolean;
					procedure Swap_L (I, J : Idx);
					procedure Sort_L is new AC.Generic_Sort (Idx, Before_L, Swap_L);
					Y : T (Idx'Last - (Length - 1) .. Idx'Last) := Unsorted;
					function Before_L (I, J : Idx) return Boolean is
					begin
						return Y (I) < Y (J);
					end Before_L;
					procedure Swap_L (I, J : Idx) is
						Temp : constant Character := Y (I);
					begin
						Y (I) := Y (J);
						Y (J) := Temp;
					end Swap_L;
				begin
					Sort_F (X'First, X'Last);
					pragma Assert (X = Sorted);
					Sort_L (Y'First, Y'Last);
					pragma Assert (Y = Sorted);
				end;
			end;
		end loop;
	end Generic_Try;
	procedure Try_Integer is new Generic_Try (Integer);
	procedure Try_Long_Long_Integer is new Generic_Try (Long_Long_Integer);
begin
	Try_Integer;
	Try_Long_Long_Integer;
	pragma Debug (Ada.Debug.Put ("OK"));
end sort_thresholds;
