# MR3 gate: the filmstrip swap, honestly stated

Card 6e833f9b moves the home strip's markup and CSS out of
`server/lib/pages/filmstrip.ml` into `filmstrip-shared/lib.ml` (module
`Filmstrip_shared`), which tuna reaches through `tuna/lib/strip_adapter.ml`.
TU3 (72b824b5) is the safety net for that move.

## What the gate proves, and what it does not

`filmstrip-shared/test/gate.sh` runs three checks:

1. **Data equivalence.** `emit` prints one line per tile
   (`name|glyph|href|summary|stamp`) from a fixed tile set through
   `Filmstrip_shared`; the gate writes `test/tiles_post_swap.txt` and
   `diff -u test/tiles_pre_swap.txt test/tiles_post_swap.txt` must be empty.
   The capture is the tile *data*, not the byte-identical HTML: the swap
   deliberately adds `data-glyph`/`data-name` to the anchor, so a markup diff
   would report the intended change as a regression and hide a real one.
   `dune runtest` runs this same diff, so the check cannot rot into a script
   nobody invokes.
2. **Rendered contract.** The gate greps the container that
   `Filmstrip_shared.strip_html` actually emits - not a CSS string nothing
   renders - for `.strip-wrap`, the feed path, `data-glyph`, `data-name`,
   `min-height:44px`, `min-width:9rem`, `overflow-x:auto`,
   `overflow-x:hidden`, the phone media query and the plain anchor.
3. **CDP mobile metrics.** `tools/filmstrip_cdp.mjs` drives 390x844 and
   1280x800 against a live page. This half needs a serving server and a
   browser driver; when either is absent the gate says so loudly and skips.
   It is never silenced with `|| true`.

## Correction to the earlier claim

An earlier revision of this document said the TU3 gate was green on the
tuna-native strip before the swap. That was false for the measured metrics:
the native inline CSS in `filmstrip.ml` carried no `min-height:44px`, no
`@media` block and no `overflow-x` rule, so the phone metrics the CDP script
measures (44px tap targets, no page-level horizontal overflow) could not have
passed there. The swap *introduces* those rules in the shared package; the
gate is therefore a proof that the shared strip meets the contract, and a
proof that the tile data survived the move - not a claim that the old
markup already met the contract.

## The one interface

`filmstrip-shared/lib.ml` owns the tile record, `tile_html`, `tiles_html`,
`css` and `strip_html` (the `.strip-wrap` scroller, the `.strip` container,
the feed attributes). `tuna/lib/strip_adapter.ml` is a type equation with
that record (`type tile = Filmstrip_shared.tile = { ... }`) and nothing else,
so a field added on either side is a build error on the other; tuna keeps
supplying only the data, from its one home
`server/lib/pages/filmstrip.ml`.
