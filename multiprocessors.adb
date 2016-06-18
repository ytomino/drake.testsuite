-- { dg-do run }
-- Please compare with
--   Darwin or FreeBSD: sysctl -n hw.ncpu
--   Linux: nproc
--   Windows: echo %NUMBER_OF_PROCESSORS%
with Ada;
with System.Multiprocessors;
procedure multiprocessors is
begin
	Ada.Debug.Put (System.Multiprocessors.CPU'Image (System.Multiprocessors.Number_Of_CPUs));
	pragma Debug (Ada.Debug.Put ("OK"));
end multiprocessors;
