with Ada;
with System.Multiprocessors;
procedure multiprocessors is
begin
	Ada.Debug.Put (System.Multiprocessors.CPU'Image (System.Multiprocessors.Number_Of_CPUs));
end multiprocessors;
