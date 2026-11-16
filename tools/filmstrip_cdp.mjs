#!/usr/bin/env node
/* TU3: the evaluation gate for GET / — the filmstrip.

   Ground truth for the tile set is server/lib/pages/filmstrip.ml (the one
   place the tile list lives); the page's own HTML is never used as its own
   oracle.  What this gate asserts:

     * anonymous-200: GET / and every tile href answer 200 with no cookie, no
       bearer, no session (a 3xx means the strip has a private tile);
     * the server-rendered DOM already carries the strip and every tile
       anchor, so the strip works with JavaScript disabled;
     * the rendered tile set equals the declared tile set, tile for tile;
     * each tile has glyph + name + summary + href, and at 390x844 and
       1280x800 every tap target is at least 44x44 CSS px and the strip does
       not scroll the page horizontally.

   Environment:
     TUNA_BASE               base URL of a running server (default
                             http://127.0.0.1:8080)
     TUNA_CHROMIUM           chromium/chrome/headless_shell to drive over CDP
                             (default: the usual /usr/bin paths)
     TUNA_FILMSTRIP_REQUIRE  =1 turns every skip below into a failure, so CI
                             can demand the full browser gate

   Exit status: 0 when everything checked passes (or when this machine has no
   node/chromium/server and TUNA_FILMSTRIP_REQUIRE is unset), 1 on any
   regression — no `|| true` anywhere on the way. */

import { spawn } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import process from "node:process";

const STRICT = process.env.TUNA_FILMSTRIP_REQUIRE === "1";
const BASE = (process.env.TUNA_BASE || "http://127.0.0.1:8080").replace(/\/+$/, "");
const ROOT = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const SRC = path.join(ROOT, "server", "lib", "pages", "filmstrip.ml");

const MIN_TAP = 44; /* CSS px: the mobile tap-target floor */
const VIEWPORTS = [
  { name: "390x844", width: 390, height: 844, dsf: 3, mobile: true },
  { name: "1280x800", width: 1280, height: 800, dsf: 1, mobile: false },
];

let failures = 0;
const fail = (msg) => { failures++; console.error(`filmstrip_cdp: FAIL ${msg}`); };
const ok = (msg) => { console.log(`filmstrip_cdp: ok   ${msg}`); };
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

/* A skip is a skip: exit 0 unless the caller asked for the full gate. */
function bail(msg) {
  if (STRICT) { fail(`${msg} (TUNA_FILMSTRIP_REQUIRE=1)`); finish(); }
  console.log(`filmstrip_cdp: skip (${msg})`);
  finish();
}

function finish() {
  process.exit(failures ? 1 : 0);
}

/* ---- the declared tile set: server/lib/pages/filmstrip.ml ------------- */

function declaredTiles() {
  const src = fs.readFileSync(SRC, "utf8");
  /* The tile list is `let tiles = [ … ]`; each field is read by NAME rather
     than by one positional pattern, so a field the record grows later (TU2
     added `stamp` after `href`) cannot make the gate blind to the tiles. */
  const list = src.match(/let tiles =\s*\[([\s\S]*?)\]/);
  const body = list ? list[1] : "";
  const field = (rec, k) => {
    const m = rec.match(new RegExp(`\\b${k}\\s*=\\s*"((?:[^"\\\\]|\\\\.)*)"`));
    return m ? m[1] : "";
  };
  const out = [];
  for (const m of body.matchAll(/\{([^}]*)\}/g)) {
    const rec = m[1];
    out.push({
      glyph: field(rec, "glyph"),
      name: field(rec, "name"),
      summary: field(rec, "summary"),
      href: field(rec, "href"),
    });
  }
  return out;
}

/* ---- the static gate: runs with no server and no browser ------------- */

/* LAW 1: private surfaces (rooms, DMs, identity, grants admin) are never
   tiled — an anonymous reader of such a tile gets a 303 to /login.  This
   check needs no server, so a private tile is caught even on a machine with
   neither a running server nor a browser. */
const PRIVATE_PREFIXES = [
  "/room", "/dm", "/identity", "/grant", "/admin", "/login", "/logout",
  "/session", "/settings", "/signup", "/feed",
];

function privatePrefix(href) {
  return PRIVATE_PREFIXES.find((p) => href === p || href.startsWith(`${p}/`) || href.startsWith(`${p}.`));
}

function staticTileChecks(tiles) {
  if (tiles.length === 0) {
    fail(`no tiles parsed out of ${path.relative(ROOT, SRC)}`);
    return;
  }
  const seen = new Set();
  for (const t of tiles) {
    if (!t.href.startsWith("/")) fail(`tile ${t.name} href ${t.href} is not a same-origin path`);
    if (!t.glyph.trim() || !t.name.trim() || !t.summary.trim()) fail(`tile ${t.href} is missing glyph/name/summary`);
    const priv = privatePrefix(t.href);
    if (priv) fail(`tile ${t.name} ${t.href} is a private surface (${priv}*): LAW 1 keeps it off the strip`);
    if (seen.has(t.href)) fail(`duplicate tile href ${t.href}`);
    seen.add(t.href);
  }
  if (failures === 0) ok(`${tiles.length} declared tiles: public paths only, each with glyph/name/summary`);
}

/* ---- anonymous-200 and the no-JavaScript DOM ------------------------- */

async function httpChecks(tiles) {
  if (tiles.length === 0) {
    fail(`no tiles parsed out of ${path.relative(ROOT, SRC)}`);
    return;
  }

  let home;
  try {
    home = await fetch(`${BASE}/`, { redirect: "manual", headers: { accept: "text/html" } });
  } catch (e) {
    bail(`no server at ${BASE} (${e.cause?.code || e.message}); set TUNA_BASE`);
    return;
  }

  if (home.status !== 200) {
    const loc = home.headers.get("location");
    fail(`anonymous GET / -> ${home.status}${loc ? ` (${loc})` : ""}: the front door must answer 200 with no cookie, no bearer, no session`);
  } else {
    ok("anonymous GET / -> 200");
  }

  const html = home.status === 200 ? await home.text() : "";
  if (html) {
    if (!html.includes('id="filmstrip"')) fail("GET / has no #filmstrip container in the server-rendered HTML");
    const rendered = (html.match(/class="tile"/g) || []).length;
    if (rendered !== tiles.length) {
      fail(`GET / renders ${rendered} tiles; ${path.relative(ROOT, SRC)} declares ${tiles.length}`);
    }
    for (const t of tiles) {
      if (!html.includes(`href="${t.href}"`)) {
        fail(`tile ${t.name} (${t.href}) is declared but not server-rendered in GET /`);
      }
    }
  }

  for (const t of tiles) {
    let r;
    try {
      r = await fetch(BASE + t.href, { redirect: "manual", headers: { accept: "text/html,*/*" } });
    } catch (e) {
      fail(`tile ${t.name} ${t.href} unreachable: ${e.cause?.code || e.message}`);
      continue;
    }
    if (r.status !== 200) {
      const loc = r.headers.get("location");
      fail(`tile ${t.name} ${t.href} -> ${r.status}${loc ? ` (${loc})` : ""}: every tile must be anonymous-200`);
    } else {
      ok(`tile ${t.name} ${t.href} -> 200 anonymous`);
    }
  }
}

/* ---- the browser: metrics over CDP ----------------------------------- */

class Cdp {
  constructor(ws) {
    this.ws = ws;
    this.next = 0;
    this.pending = new Map();
    this.events = new Map();
    ws.addEventListener("message", (e) => {
      const msg = JSON.parse(e.data);
      if (msg.id !== undefined && this.pending.has(msg.id)) {
        const { res, rej } = this.pending.get(msg.id);
        this.pending.delete(msg.id);
        msg.error ? rej(new Error(msg.error.message)) : res(msg.result || {});
        return;
      }
      if (msg.method) {
        const h = this.events.get(msg.method);
        if (h) h(msg.params || {});
      }
    });
    ws.addEventListener("close", () => {
      for (const { rej } of this.pending.values()) rej(new Error("devtools connection closed"));
      this.pending.clear();
    });
  }

  send(method, params = {}, sessionId) {
    const id = ++this.next;
    const p = new Promise((res, rej) => this.pending.set(id, { res, rej }));
    this.ws.send(JSON.stringify(sessionId ? { id, method, params, sessionId } : { id, method, params }));
    return p;
  }

  once(method, timeoutMs = 30000) {
    return new Promise((res, rej) => {
      const timer = setTimeout(() => {
        this.events.delete(method);
        rej(new Error(`timed out waiting for ${method}`));
      }, timeoutMs);
      this.events.set(method, (params) => { clearTimeout(timer); this.events.delete(method); res(params); });
    });
  }
}

function findChromium() {
  const cands = [
    process.env.TUNA_CHROMIUM, process.env.CHROME_PATH,
    "/usr/bin/chromium", "/usr/bin/chromium-browser", "/usr/local/bin/chromium",
    "/usr/bin/google-chrome", "/usr/bin/google-chrome-stable",
    "/usr/bin/headless_shell", "/usr/bin/chrome-headless-shell",
  ];
  for (const c of cands) if (c && fs.existsSync(c)) return c;
  return null;
}

async function launchChromium(bin) {
  const profile = fs.mkdtempSync(path.join(os.tmpdir(), "filmstrip-cdp-"));
  const proc = spawn(bin, [
    "--headless=new", "--remote-debugging-port=0", "--no-sandbox", "--no-first-run",
    "--disable-gpu", "--disable-dev-shm-usage", "--disable-extensions",
    `--user-data-dir=${profile}`, "about:blank",
  ], { stdio: ["ignore", "ignore", "pipe"] });
  let stderr = "";
  proc.stderr.on("data", (d) => { stderr += d.toString(); if (stderr.length > 8192) stderr = stderr.slice(-8192); });

  const portFile = path.join(profile, "DevToolsActivePort");
  const deadline = Date.now() + 30000;
  while (!fs.existsSync(portFile) && Date.now() < deadline && proc.exitCode === null) await sleep(100);
  if (!fs.existsSync(portFile)) {
    try { proc.kill("SIGKILL"); } catch { /* already gone */ }
    throw new Error(`chromium opened no devtools port: ${stderr.trim().slice(-400)}`);
  }
  const port = Number(fs.readFileSync(portFile, "utf8").split("\n")[0]);

  const wsDeadline = Date.now() + 20000;
  for (;;) {
    try {
      const v = await (await fetch(`http://127.0.0.1:${port}/json/version`)).json();
      return { proc, wsUrl: v.webSocketDebuggerUrl };
    } catch (e) {
      if (Date.now() > wsDeadline) {
        try { proc.kill("SIGKILL"); } catch { /* already gone */ }
        throw new Error(`chromium devtools endpoint never came up: ${e.message}`);
      }
      await sleep(100);
    }
  }
}

const MEASURE = `(() => {
  const strip = document.getElementById("filmstrip");
  if (!strip) return { ok: false, err: "no #filmstrip" };
  const tiles = Array.prototype.map.call(strip.querySelectorAll("a.tile"), (t) => {
    const r = t.getBoundingClientRect();
    const q = (c) => { const n = t.querySelector(c); return n ? (n.textContent || "").trim() : ""; };
    return { href: t.getAttribute("href") || "", w: Math.round(r.width), h: Math.round(r.height),
             glyph: q(".glyph"), name: q(".name"), summary: q(".summary") };
  });
  return { ok: true, vw: window.innerWidth,
           scrollW: Math.max(document.documentElement.scrollWidth,
                             document.body ? document.body.scrollWidth : 0),
           tiles: tiles };
})()`;

async function viewportChecks(cdp, vp, tiles) {
  const { targetId } = await cdp.send("Target.createTarget", { url: "about:blank" });
  const { sessionId } = await cdp.send("Target.attachToTarget", { targetId, flatten: true });
  const s = (method, params) => cdp.send(method, params, sessionId);
  try {
    await s("Page.enable");
    await s("Runtime.enable");
    await s("DOM.enable");
    await s("Emulation.setDeviceMetricsOverride", {
      width: vp.width, height: vp.height, deviceScaleFactor: vp.dsf, mobile: vp.mobile,
    });

    /* 1. no JavaScript at all: the strip and every tile must already be in
          the server-rendered DOM. */
    const expected = new Map(tiles.map((t) => [t.href, t]));
    const loaded = cdp.once("Page.loadEventFired");
    await s("Emulation.setScriptExecutionDisabled", { value: true });
    await s("Page.navigate", { url: `${BASE}/` });
    await loaded;

    const doc = await s("DOM.getDocument", { depth: -1 });
    const { outerHTML } = await s("DOM.getOuterHTML", { nodeId: doc.root.nodeId });
    const html = String(outerHTML || "");
    if (!/id="filmstrip"/.test(html)) {
      fail(`[${vp.name}] no #filmstrip in the server-rendered DOM with scripts disabled`);
    }
    for (const t of tiles) {
      if (!html.includes(`href="${t.href}"`)) {
        fail(`[${vp.name}] tile ${t.name} (${t.href}) is not a plain anchor with scripts disabled`);
      }
    }
    ok(`[${vp.name}] strip and ${tiles.length} tiles are server-rendered with JS disabled`);

    /* 2. with JavaScript on: the metrics. */
    await s("Emulation.setScriptExecutionDisabled", { value: false });
    const reloaded = cdp.once("Page.loadEventFired");
    await s("Page.reload", { ignoreCache: true });
    await reloaded;

    const { result } = await s("Runtime.evaluate", { expression: MEASURE, returnByValue: true });
    const m = result && result.value;
    if (!m || !m.ok) {
      fail(`[${vp.name}] ${(m && m.err) || "could not read the strip"}`);
      return;
    }

    if (m.scrollW > m.vw + 1) {
      fail(`[${vp.name}] the strip scrolls the page horizontally (${m.scrollW} > ${m.vw})`);
    }

    if (m.tiles.length !== tiles.length) {
      fail(`[${vp.name}] rendered ${m.tiles.length} tiles, ${path.relative(ROOT, SRC)} declares ${tiles.length}`);
    }

    for (const r of m.tiles) {
      const want = expected.get(r.href);
      if (!want) {
        fail(`[${vp.name}] rendered a tile for ${r.href || "(no href)"}, which the tile table does not declare`);
        continue;
      }
      if (r.h < MIN_TAP || r.w < MIN_TAP) {
        fail(`[${vp.name}] tile ${want.name} (${r.href}) is ${r.w}x${r.h} CSS px; tap target floor is ${MIN_TAP}x${MIN_TAP}`);
      }
      if (!r.glyph) fail(`[${vp.name}] tile ${want.name} (${r.href}) shows no glyph`);
      if (!r.name) fail(`[${vp.name}] tile ${want.name} (${r.href}) shows no name`);
      if (!r.summary) fail(`[${vp.name}] tile ${want.name} (${r.href}) shows no summary`);
    }
    ok(`[${vp.name}] ${m.tiles.length} tiles, each >= ${MIN_TAP}x${MIN_TAP} CSS px, no horizontal overflow`);
  } finally {
    try { await cdp.send("Target.closeTarget", { targetId }); } catch { /* target already gone */ }
  }
}

async function browserChecks(tiles) {
  /* We only get here when the server answered: a server without a browser
     is a partial gate, so a missing chromium is a failure, not a skip.
     (The one skip this gate allows is "no server at all", which keeps the
     stanza runnable on a laptop with node but no service running.) */
  const bin = findChromium();
  if (!bin) { fail("chromium absent but the server answered: the mobile metrics gate cannot run; set TUNA_CHROMIUM"); return; }
  if (typeof WebSocket === "undefined") { fail(`node ${process.version} has no global WebSocket: the CDP gate cannot run`); return; }

  let launched;
  try {
    launched = await launchChromium(bin);
  } catch (e) {
    fail(`chromium could not be driven: ${e.message}`);
    return;
  }
  let cdp;
  try {
    cdp = await new Promise((res, rej) => {
      const ws = new WebSocket(launched.wsUrl);
      ws.addEventListener("open", () => res(new Cdp(ws)));
      ws.addEventListener("error", () => rej(new Error("devtools websocket refused")));
    });
    for (const vp of VIEWPORTS) {
      try {
        await viewportChecks(cdp, vp, tiles);
      } catch (e) {
        fail(`[${vp.name}] ${e.message}`);
      }
    }
  } catch (e) {
    fail(`devtools connection failed: ${e.message}`);
  } finally {
    try { cdp && cdp.ws.close(); } catch { /* already closed */ }
    try { launched.proc.kill("SIGKILL"); } catch { /* already gone */ }
  }
}

/* ---- main ------------------------------------------------------------ */

async function main() {
  console.log(`filmstrip_cdp: GET / at ${BASE}`);
  let tiles;
  try {
    tiles = declaredTiles();
  } catch (e) {
    fail(`cannot read the tile table ${path.relative(ROOT, SRC)}: ${e.message}`);
    return finish();
  }

  staticTileChecks(tiles);
  await httpChecks(tiles);
  await browserChecks(tiles);

  finish();
}

main().catch((e) => { fail(`uncaught: ${e.stack || e.message}`); finish(); });
