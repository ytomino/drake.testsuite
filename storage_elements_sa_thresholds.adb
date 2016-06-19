-- { dg-do run }
with Ada.Streams.Unbounded_Storage_IO;
with System.Storage_Elements;
procedure storage_elements_sa_thresholds is
	use System.Storage_Elements;
	In_First_Zero_SA : constant
		Storage_Array (Storage_Offset'First + 1 .. Storage_Offset'First) :=
		(others => <>);
	In_First_One_SA : constant
		Storage_Array (Storage_Offset'First .. Storage_Offset'First) :=
		(Storage_Offset'First => 16#11#);
	Out_First_Zero_SA :
		Storage_Array (Storage_Offset'First + 1 .. Storage_Offset'First);
	Out_First_One_SA :
		Storage_Array (Storage_Offset'First .. Storage_Offset'First);
	In_Last_Zero_SA : constant
		Storage_Array (Storage_Offset'Last .. Storage_Offset'Last - 1) :=
		(others => <>);
	In_Last_One_SA : constant
		Storage_Array (Storage_Offset'Last .. Storage_Offset'Last) :=
		(Storage_Offset'Last => 16#22#);
	Out_Last_Zero_SA :
		Storage_Array (Storage_Offset'Last .. Storage_Offset'Last - 1);
	Out_Last_One_SA :
		Storage_Array (Storage_Offset'Last .. Storage_Offset'Last);
begin
	-- streaming with Ada.Streams.Block_Transmission
	declare
		Buffer : Ada.Streams.Unbounded_Storage_IO.Buffer_Type;
	begin
		-- output
		Storage_Array'Write (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			In_First_Zero_SA);
		Storage_Array'Output (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			In_First_Zero_SA);
		Storage_Array'Write (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			In_First_One_SA);
		Storage_Array'Output (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			In_First_One_SA);
		Storage_Array'Write (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			In_Last_Zero_SA);
		Storage_Array'Output (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			In_Last_Zero_SA);
		Storage_Array'Write (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			In_Last_One_SA);
		Storage_Array'Output (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			In_Last_One_SA);
		-- reset the index to the first
		Ada.Streams.Unbounded_Storage_IO.Reset (Buffer);
		-- input
		Out_First_Zero_SA := (others => 0);
		Storage_Array'Read (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			Out_First_Zero_SA);
		pragma Assert (Out_First_Zero_SA = In_First_Zero_SA);
		Out_First_Zero_SA := Storage_Array'Input (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
		pragma Assert (Out_First_Zero_SA = In_First_Zero_SA);
		Out_First_One_SA := (others => 0);
		Storage_Array'Read (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			Out_First_One_SA);
		pragma Assert (Out_First_One_SA = In_First_One_SA);
		Out_First_One_SA := Storage_Array'Input (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
		pragma Assert (Out_First_One_SA = In_First_One_SA);
		Out_Last_Zero_SA := (others => 0);
		Storage_Array'Read (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			Out_Last_Zero_SA);
		pragma Assert (Out_Last_Zero_SA = In_Last_Zero_SA);
		Out_Last_Zero_SA := Storage_Array'Input (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
		pragma Assert (Out_Last_Zero_SA = In_Last_Zero_SA);
		Out_Last_One_SA := (others => 0);
		Storage_Array'Read (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			Out_Last_One_SA);
		pragma Assert (Out_Last_One_SA = In_Last_One_SA);
		Out_Last_One_SA := Storage_Array'Input (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
		pragma Assert (Out_Last_One_SA = In_Last_One_SA);
	end;
	-- finished
	pragma Debug (Ada.Debug.Put ("OK"));
end storage_elements_sa_thresholds;
