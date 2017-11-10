-- { dg-do run }
with Ada.Unchecked_Deallocation;
with System.Formatting;
with System.Long_Long_Integer_Types;
with System.Storage_Elements;
with System.System_Allocators.Allocated_Size;
procedure standard_allocators is
	use type System.Storage_Elements.Storage_Offset;
	package System_Allocators renames System.System_Allocators;
	procedure Put (X : System.Storage_Elements.Integer_Address) is
		S : String (1 .. Standard'Address_Size / 4);
		Last : Natural;
		Error : Boolean;
	begin
		System.Formatting.Image (
			System.Long_Long_Integer_Types.Long_Long_Unsigned (X),
			S,
			Last,
			16,
			Width => S'Length,
			Error => Error);
		Ada.Debug.Put (S);
	end Put;
	procedure Put (X : System.Storage_Elements.Storage_Count) is
		S : String (1 .. Standard'Address_Size / 4);
		Last : Natural;
		Error : Boolean;
	begin
		System.Formatting.Image (
			System.Long_Long_Integer_Types.Long_Long_Unsigned (X),
			S,
			Last,
			Padding => ' ',
			Width => S'Length,
			Error => Error);
		Ada.Debug.Put (S);
	end Put;
begin
	declare -- new and Unchecked_Deallocation
		type Integer_Access is access all Integer;
		procedure Free is new Ada.Unchecked_Deallocation (Integer, Integer_Access);
		x, y : Integer_Access;
	begin
		x := new Integer;
		y := new Integer;
		pragma Assert (
			abs (x'Pool_Address - y'Pool_Address) >=
			System.Storage_Elements.Storage_Offset'Max (
				System_Allocators.Allocated_Size (x'Pool_Address),
				System_Allocators.Allocated_Size (y'Pool_Address)));
		Free (x);
		Free (y);
		pragma Assert (y = null);
		Free (y); -- null
	end;
	declare -- low-level
		x : System.Address;
	begin
		x := System_Allocators.Allocate (3);
		Put (System.Storage_Elements.To_Integer (x));
		Put (System_Allocators.Allocated_Size (x));
		x := System_Allocators.Reallocate (x, 33);
		Put (System.Storage_Elements.To_Integer (x));
		Put (System_Allocators.Allocated_Size (x));
		System_Allocators.Free (x);
		System_Allocators.Free (System.Null_Address); -- null
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end standard_allocators;
