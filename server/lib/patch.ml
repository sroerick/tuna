(* Tuna_server.Patch: the structural CAS patch now lives in the shared
   REPL library (borg/local.borg L2) so the local REPL and the server
   run one implementation.  Re-exported under its old name. *)
include Tuna_repl.Patch
