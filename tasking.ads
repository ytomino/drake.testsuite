-- { dg-do link }
pragma Warnings (Off, "is not referenced");
with Ada.Asynchronous_Task_Control;
with Ada.Dispatching.EDF;
with Ada.Dispatching.Non_Preemptive;
with Ada.Dispatching.Round_Robin;
with Ada.Dynamic_Priorities;
with Ada.Execution_Time.Group_Budgets;
with Ada.Execution_Time.Interrupts;
with Ada.Execution_Time.Timers;
with Ada.Real_Time.Timing_Events;
with Ada.Synchronous_Task_Control.EDF;
with Ada.Synchronous_Barriers;
with Ada.Synchronous_Task_Control;
pragma Unreferenced (Ada.Synchronous_Task_Control);
with Ada.Task_Attributes;
with Ada.Task_Identification;
with Ada.Task_Termination;
with System.Multiprocessors.Dispatching_Domains;
package tasking is
end tasking;
