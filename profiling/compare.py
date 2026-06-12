#!/usr/bin/env python3
"""Compare two profiled compiler builds for regressions.

Usage: compare.py <baseline-run-dir> <candidate-run-dir>

Reads report.json from each run directory (produced by profile-build.sh) and
prints wall-clock delta, phase-bucket deltas, and the methods whose inclusive
share moved the most. Exits 1 if wall clock regressed by more than 10%.
"""

import json
import sys
from pathlib import Path

REGRESSION_THRESHOLD_PCT = 10.0


def load(run_dir):
    path = Path(run_dir) / "report.json"
    if not path.exists():
        sys.exit(f"error: {path} not found")
    return json.loads(path.read_text())


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__.strip())
    base, cand = load(sys.argv[1]), load(sys.argv[2])

    def ident(r):
        return f"{r.get('sha', '?')} {r.get('mode', '')} {r.get('label', '')}".strip()

    print(f"baseline:  {ident(base)}")
    print(f"candidate: {ident(cand)}")
    print()

    bw, cw = base.get("wall_seconds"), cand.get("wall_seconds")
    regressed = False
    if bw and cw:
        delta_pct = 100.0 * (cw - bw) / bw
        regressed = delta_pct > REGRESSION_THRESHOLD_PCT
        sign = "+" if delta_pct >= 0 else ""
        print(f"wall clock: {bw:.0f}s -> {cw:.0f}s ({sign}{delta_pct:.1f}%)"
              + ("  ** REGRESSION **" if regressed else ""))
        print()

    print("phase buckets (% of samples):")
    keys = dict.fromkeys(list(base["buckets"]) + list(cand["buckets"]))
    for k in keys:
        b = base["buckets"].get(k, {}).get("pct", 0.0)
        c = cand["buckets"].get(k, {}).get("pct", 0.0)
        print(f"  {k:16s} {b:6.1f} -> {c:6.1f}  ({c - b:+.1f})")
    print()

    def shares(r):
        return {row["method"]: row["pct"] for row in r["top_inclusive"]}

    bs, cs = shares(base), shares(cand)
    moved = sorted(
        ((m, bs.get(m, 0.0), cs.get(m, 0.0)) for m in {*bs, *cs}),
        key=lambda t: abs(t[2] - t[1]),
        reverse=True,
    )[:15]
    print("largest inclusive-share moves (top-30 methods of either run):")
    for m, b, c in moved:
        print(f"  {c - b:+6.1f}  {b:5.1f} -> {c:5.1f}  {m}")

    sys.exit(1 if regressed else 0)


if __name__ == "__main__":
    main()
