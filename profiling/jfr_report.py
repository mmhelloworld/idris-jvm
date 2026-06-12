#!/usr/bin/env python3
"""Aggregate JFR execution samples from a compiler build into a phase/hotspot report.

Reads the output of `jfr print --events jdk.ExecutionSample <recording>` on stdin,
classifies each sample into a compiler phase bucket, and computes inclusive and
exclusive (leaf) sample counts per method.

Usage:
  jfr print --events jdk.ExecutionSample build.jfr | \
      jfr_report.py --json report.json [--wall 123] [--sha abc] [--branch b] \
                    [--mode warm] [--label desc] > report.md

Buckets (classified by walking frames leaf -> root, first domain frame wins):
  jvm-backend   Compiler.Jvm.* (monomorphization, inference, codegen) plus the
                Java assembler and ASM bytecode-writing libraries
  backend-common Compiler.* outside Compiler.Jvm (CompileExpr, inlining, ANF...)
  frontend      Core.*, TTImp.*, Idris.*, Parser.* (parse/elaborate/typecheck)
  runtime/other no domain frame found in the stack
"""

import argparse
import json
import re
import sys
from collections import Counter

FRAME_RE = re.compile(r"^\s*([\w.$]+)\(")

JVM_BACKEND_PREFIXES = ("M_Compiler.M_Jvm.",)
JVM_BACKEND_SUBSTRINGS = (".idrisjvm.assembler.", "org.objectweb.asm.")
BACKEND_COMMON_PREFIX = "M_Compiler."
FRONTEND_PREFIXES = ("M_Core.", "M_TTImp.", "M_Idris.", "M_Parser.", "M_Protocol.")

TOP_N = 30


def classify(frames):
    """frames are ordered leaf -> root. Return bucket name.

    Category priority over the whole stack, not first-matching-frame: the
    JVM backend executes inside the Core monad, so backend samples routinely
    have M_Core.* plumbing frames (traverse_, catch, wrapRef) at the leaf —
    frame-order classification misfiled those as frontend (observed: a
    "frontend 43%" bucket that --timing showed was mostly backend time).
    """
    if any(f.startswith(JVM_BACKEND_PREFIXES) or any(s in f for s in JVM_BACKEND_SUBSTRINGS)
           for f in frames):
        return "jvm-backend"
    if any(f.startswith(BACKEND_COMMON_PREFIX) for f in frames):
        return "backend-common"
    if any(f.startswith(FRONTEND_PREFIXES) for f in frames):
        return "frontend"
    return "runtime/other"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--json", help="write machine-readable report to this path")
    ap.add_argument("--wall", type=float, default=None, help="wall-clock build seconds")
    ap.add_argument("--sha", default="")
    ap.add_argument("--branch", default="")
    ap.add_argument("--mode", default="")
    ap.add_argument("--label", default="")
    args = ap.parse_args()

    total = 0
    truncated = 0
    buckets = Counter()
    inclusive = Counter()
    exclusive = Counter()
    backend_inclusive = Counter()

    in_stack = False
    stack_truncated = False
    frames = []

    def finish_sample():
        nonlocal total, truncated
        if not frames:
            return
        total += 1
        if stack_truncated:
            truncated += 1
        buckets[classify(frames)] += 1
        exclusive[frames[0]] += 1
        seen = set(frames)
        for f in seen:
            inclusive[f] += 1
            if f.startswith(JVM_BACKEND_PREFIXES):
                backend_inclusive[f] += 1

    for line in sys.stdin:
        if not in_stack:
            if "stackTrace = [" in line:
                in_stack = True
                stack_truncated = False
                frames = []
            continue
        stripped = line.strip()
        if stripped == "]":
            in_stack = False
            finish_sample()
            continue
        if stripped == "...":
            stack_truncated = True
            continue
        m = FRAME_RE.match(line)
        if m:
            frames.append(m.group(1))

    if total == 0:
        print("No execution samples found in recording.", file=sys.stderr)
        sys.exit(1)

    def pct(n):
        return 100.0 * n / total

    def top(counter, n=TOP_N):
        return [
            {"method": m, "samples": c, "pct": round(pct(c), 2)}
            for m, c in counter.most_common(n)
        ]

    report = {
        "sha": args.sha,
        "branch": args.branch,
        "mode": args.mode,
        "label": args.label,
        "wall_seconds": args.wall,
        "total_samples": total,
        "truncated_samples": truncated,
        "truncated_pct": round(100.0 * truncated / total, 2),
        "buckets": {
            k: {"samples": v, "pct": round(pct(v), 2)}
            for k, v in buckets.most_common()
        },
        "top_inclusive": top(inclusive),
        "top_jvm_backend_inclusive": top(backend_inclusive),
        "top_exclusive": top(exclusive, 20),
    }

    if args.json:
        with open(args.json, "w") as f:
            json.dump(report, f, indent=2)

    # Markdown report on stdout
    print(f"# Compiler build profile — {args.sha} ({args.branch}) {args.label}")
    print()
    if args.wall is not None:
        print(f"- Wall clock: **{args.wall:.0f}s** (mode: {args.mode})")
    print(f"- Execution samples: {total}")
    print(f"- Truncated stacks: {truncated} ({100.0 * truncated / total:.1f}%) — "
          "if high, raise JFR_STACKDEPTH; bucket attribution degrades")
    print()
    print("## Phase buckets")
    print()
    print("| Bucket | Samples | % |")
    print("|---|---:|---:|")
    for k, v in buckets.most_common():
        print(f"| {k} | {v} | {pct(v):.1f} |")
    print()

    def table(title, rows):
        print(f"## {title}")
        print()
        print("| Method | Samples | % |")
        print("|---|---:|---:|")
        for r in rows:
            print(f"| `{r['method']}` | {r['samples']} | {r['pct']:.1f} |")
        print()

    table("Top methods by inclusive time (any frame in stack)", report["top_inclusive"])
    table("Top Compiler.Jvm.* methods by inclusive time", report["top_jvm_backend_inclusive"])
    table("Top methods by exclusive (leaf) time", report["top_exclusive"])


if __name__ == "__main__":
    main()
