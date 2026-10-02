#!/usr/bin/env python3
"""Canonical ternary tree helpers for the forensics corpus.

Encoding (AGENTS rule 2 / reference tree-calculus):
  0        leaf
  1 <c>    stem
  2 <l> <r> fork
Hash = sha256 of the ternary string, lowercase hex.

Path digits (matching server/lib/patch.ml + provenance):
  0 = stem child, 1 = fork left, 2 = fork right; "" is the root.
"""

import hashlib


def parse(s, i=0):
    if i >= len(s):
        raise ValueError("unexpected end of ternary")
    c = s[i]
    i += 1
    if c == '0':
        return ('L',), i
    if c == '1':
        n, i = parse(s, i)
        return ('S', n), i
    if c == '2':
        l, i = parse(s, i)
        r, i = parse(s, i)
        return ('F', l, r), i
    raise ValueError(f"bad ternary char {c!r} at {i-1}")


def decode(s):
    t, i = parse(s)
    if i != len(s):
        raise ValueError(f"trailing ternary at offset {i}")
    return t


def encode(t):
    tag = t[0]
    if tag == 'L':
        return '0'
    if tag == 'S':
        return '1' + encode(t[1])
    if tag == 'F':
        return '2' + encode(t[1]) + encode(t[2])
    raise ValueError(f"bad tree {t!r}")


def size(t):
    tag = t[0]
    if tag == 'L':
        return 1
    if tag == 'S':
        return 1 + size(t[1])
    return 1 + size(t[1]) + size(t[2])


def at(t, path):
    for d in path:
        tag = t[0]
        if d == '0' and tag == 'S':
            t = t[1]
        elif d == '1' and tag == 'F':
            t = t[1]
        elif d == '2' and tag == 'F':
            t = t[2]
        else:
            raise ValueError(f"path {path!r} does not address a subtree")
    return t


def replace(t, path, new_sub):
    if not path:
        return new_sub
    d, rest = path[0], path[1:]
    tag = t[0]
    if d == '0' and tag == 'S':
        return ('S', replace(t[1], rest, new_sub))
    if d == '1' and tag == 'F':
        return ('F', replace(t[1], rest, new_sub), t[2])
    if d == '2' and tag == 'F':
        return ('F', t[1], replace(t[2], rest, new_sub))
    raise ValueError(f"path {path!r} does not address a subtree")


def hexhash(s):
    return hashlib.sha256(s.encode()).hexdigest()


def tree_hash(t):
    return hexhash(encode(t))


def all_paths(t, path=""):
    yield path, t
    tag = t[0]
    if tag == 'S':
        yield from all_paths(t[1], path + '0')
    elif tag == 'F':
        yield from all_paths(t[1], path + '1')
        yield from all_paths(t[2], path + '2')


def first_diff(a, b):
    """Deepest structural divergence (matching server/lib/patch.ml)."""
    if a[0] == 'L' and b[0] == 'L':
        return None
    if a[0] == 'S' and b[0] == 'S':
        p = first_diff(a[1], b[1])
        return None if p is None else '0' + p
    if a[0] == 'F' and b[0] == 'F':
        p = first_diff(a[1], b[1])
        if p is not None:
            return '1' + p
        p = first_diff(a[2], b[2])
        return None if p is None else '2' + p
    return ""


def flip_digit(t, path):
    """Structural class-1 mutation: toggle the tag at `path`
    (L<->S is illegal arity-wise, so 0<->1 is done by wrapping:
    a leaf becomes a stem-of-leaf, a stem becomes its child; a fork's
    right child is dropped).  Deterministic and structure-preserving
    where possible."""
    sub = at(t, path)
    tag = sub[0]
    if tag == 'L':
        new = ('S', ('L',))
    elif tag == 'S':
        new = sub[1]
    else:
        new = sub[1]  # fork -> its left child (dropped right arm)
    return replace(t, path, new)


def swap_arms(t, path):
    """Structural class-1 mutation: swap the two arms of a fork at
    `path` (no-op error on non-fork)."""
    sub = at(t, path)
    if sub[0] != 'F':
        raise ValueError(f"swap_arms: path {path!r} is not a fork")
    return replace(t, path, ('F', sub[2], sub[1]))
