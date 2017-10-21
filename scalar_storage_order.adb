-- { dg-do run }
with Ada;
with Interfaces;
with System.Storage_Elements;
procedure scalar_storage_order is
	use type Interfaces.Unsigned_8;
	use type System.Storage_Elements.Storage_Offset;
	function Hex_Dump (
		Storage_Address : System.Address;
		Storage_Size : System.Storage_Elements.Storage_Count)
		return String
	is
		Hex : constant array (0 .. 15) of Character := "0123456789ABCDEF";
		T : String (1 .. Integer (Storage_Size) * 3);
	begin
		for I in 0 .. Storage_Size - 1 loop
			declare
				X : Interfaces.Unsigned_8;
				for X'Address use Storage_Address + I;
			begin
				T (Integer (I) * 3 + 1) := Hex (Integer (X / 16));
				T (Integer (I) * 3 + 2) := Hex (Integer (X rem 16));
				T (Integer (I) * 3 + 3) := ' ';
			end;
		end loop;
		return (T (1 .. T'Last - 1));
	end Hex_Dump;
	type Unsigned_24 is mod 2 ** 24;
begin
	-- little endian
	declare
		Dump : constant String := "56 34 12";
		type LE_Rec is record
			Value : Unsigned_24;
		end record;
		pragma Pack (LE_Rec);
		for LE_Rec'Size use 24;
		for LE_Rec'Bit_Order use System.Low_Order_First;
		for LE_Rec'Scalar_Storage_Order use System.Low_Order_First;
		X : LE_Rec := (Value => 16#123456#);
		type LE_Arr is array (1 .. 1) of Unsigned_24;
		for LE_Arr'Component_Size use 24;
		for LE_Arr'Scalar_Storage_Order use System.Low_Order_First;
		Y : LE_Arr := (1 => 16#123456#);
		-- nested
		type LE_Nested_Rec is record
			C : LE_Rec;
		end record;
		pragma Pack (LE_Nested_Rec);
		for LE_Nested_Rec'Size use 24;
		for LE_Nested_Rec'Bit_Order use System.Low_Order_First;
		for LE_Nested_Rec'Scalar_Storage_Order use System.Low_Order_First;
		NX : LE_Nested_Rec := (C => (Value => 16#123456#));
		type LE_Nested_Arr is array (1 .. 1) of LE_Arr;
		for LE_Nested_Arr'Component_Size use 24;
		for LE_Nested_Arr'Scalar_Storage_Order use System.Low_Order_First;
		NY : LE_Nested_Arr := (1 => (1 => 16#123456#));
	begin
		pragma Assert (Hex_Dump (X'Address, 24 / Standard'Storage_Unit) = Dump);
		pragma Assert (Hex_Dump (Y'Address, 24 / Standard'Storage_Unit) = Dump);
		pragma Assert (Hex_Dump (NX'Address, 24 / Standard'Storage_Unit) = Dump);
		pragma Assert (Hex_Dump (NY'Address, 24 / Standard'Storage_Unit) = Dump);
		pragma Assert (X.Value = NX.C.Value);
		pragma Assert (Y (1) = NY (1) (1));
	end;
	-- big endian
	declare
		Dump : constant String := "12 34 56";
		type BE_Rec is record
			Value : Unsigned_24;
		end record;
		pragma Pack (BE_Rec);
		for BE_Rec'Size use 24;
		for BE_Rec'Bit_Order use System.High_Order_First;
		for BE_Rec'Scalar_Storage_Order use System.High_Order_First;
		X : BE_Rec := (Value => 16#123456#);
		type BE_Arr is array (1 .. 1) of Unsigned_24;
		for BE_Arr'Component_Size use 24;
		for BE_Arr'Scalar_Storage_Order use System.High_Order_First;
		Y : BE_Arr := (1 => 16#123456#);
		-- nested
		type BE_Nested_Rec is record
			C : BE_Rec;
		end record;
		pragma Pack (BE_Nested_Rec);
		for BE_Nested_Rec'Size use 24;
		for BE_Nested_Rec'Bit_Order use System.High_Order_First;
		for BE_Nested_Rec'Scalar_Storage_Order use System.High_Order_First;
		NX : BE_Nested_Rec := (C => (Value => 16#123456#));
		type BE_Nested_Arr is array (1 .. 1) of BE_Arr;
		for BE_Nested_Arr'Component_Size use 24;
		for BE_Nested_Arr'Scalar_Storage_Order use System.High_Order_First;
		NY : BE_Nested_Arr := (1 => (1 => 16#123456#));
	begin
		pragma Assert (Hex_Dump (X'Address, 24 / Standard'Storage_Unit) = Dump);
		pragma Assert (Hex_Dump (Y'Address, 24 / Standard'Storage_Unit) = Dump);
		pragma Assert (Hex_Dump (NX'Address, 24 / Standard'Storage_Unit) = Dump);
		pragma Assert (Hex_Dump (NY'Address, 24 / Standard'Storage_Unit) = Dump);
		pragma Assert (X.Value = NX.C.Value);
		pragma Assert (Y (1) = NY (1) (1));
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end scalar_storage_order;
