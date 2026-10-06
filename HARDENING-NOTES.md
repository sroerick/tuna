# httpun hardening plan (persisted 10-06 after the 4th child wall-death)

Branch `httpun-hardening`, cut from master 0b5e18c (fix 3430f9b + error-path
regression tests; 3430f9b deployed to town 10-05 18:35Z). This file persists
the recon so no session re-derives it. Four detached children have died at
the 1h wall on this workstream (papercut 1U5MHFI; the 4th burned ~4.8M tokens
for zero code). Successor protocol: land ONE slice per session, commit by
minute 40 no matter what.

## Recon (from child tuna-httpun-hardening-4, verified on clean master)
- server/lib/web.ml (516 lines) is the whole HTTP floor; per-connection
  switches exist from the 10-04 fix; no chunked/streaming code in the server
  tree (nothing that depends on chunked responses in-tree).
- Test harness boots real `serve` instances on loopback with raw sockets
  (tests/web_errorpath_tests.ml, 366 lines, already on master); dune 3.24.2
  (opam switch poohstack); clean-master `dune build` exit 0.
- httpun supports 413/431 status codes natively; Eio 1.5 Pi interface
  understood; all call sites mapped.
- API facts still to pin before S2/S4: Eio.Fiber.first signature,
  accept_fork socket-close semantics, Flow.copy_string, Time.sleep.

## Slices (small loops; one per session)
  - S1 DONE 10-06 on this branch: declared Content-Length past max_body_bytes
    OR bytes actually read past it -> 413 + Connection: close (web.ml request
    handler; read_body's Body_too_large no longer swallowed into an empty
    body). Cap = the existing 1 MiB max_body_bytes (+TUNA_MAX_BODY_BYTES).
    Suite 7/7: new cases = declared CL past cap + chunked stream past cap
    (CL framing never delivers more than it declares; chunked is the read
    path).
- S2 body-read timeout (slowloris) -> 408 or close.
- S3 connection cap -> 503 refusal when saturated.
- S4 per-connection lifetime cap.
- S5 run.ml delegation of the knobs; tests mirror the errorpath harness,
  1-2 cases per slice.

## Successor protocol
1. Read this file + git log --oneline -3 on this branch. No re-recon.
2. Implement one slice, dune build, run the web tests.
3. COMMIT by minute 40 regardless of state; never leave the tree dirty.
4. Do not push, do not merge to master, do not deploy (deploy chain follows
   master only; merge+deploy = roerick).
