(* Re-export Midden_store.Pgx_eio so Tuna's kernel queries and Midden_store
   share one ambient Eio context and one connection type. *)
include Midden_store.Pgx_eio
