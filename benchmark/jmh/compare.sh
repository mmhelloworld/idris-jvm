#!/usr/bin/env bash
# Compare two JMH JSON result files produced by run.sh, focusing on
# throughput and allocation rate per operation.
#
# Usage: ./compare.sh results/baseline.json results/mono.json
set -euo pipefail

python3 - "${1:?usage: compare.sh <before.json> <after.json>}" \
          "${2:?usage: compare.sh <before.json> <after.json>}" <<'EOF'
import json
import sys

def load(path):
    out = {}
    with open(path) as f:
        for entry in json.load(f):
            name = entry["benchmark"].split(".")[-1]
            metric = entry["primaryMetric"]
            alloc = entry.get("secondaryMetrics", {}).get("gc.alloc.rate.norm", {})
            out[name] = (metric["score"], metric["scoreUnit"], alloc.get("score"))
    return out

before_path, after_path = sys.argv[1], sys.argv[2]
before, after = load(before_path), load(after_path)

header = (f"{'benchmark':<16} {'before':>14} {'after':>14} {'unit':<10} "
          f"{'alloc before':>14} {'alloc after':>14} {'alloc change':>13}")
print(f"before = {before_path}, after = {after_path}")
print(header)
print("-" * len(header))
for name in sorted(set(before) | set(after)):
    b_score, b_unit, b_alloc = before.get(name, (float("nan"), "?", None))
    a_score, _, a_alloc = after.get(name, (float("nan"), "?", None))
    if b_alloc and a_alloc:
        change = f"{(a_alloc - b_alloc) / b_alloc * 100:+.1f}%"
    else:
        change = "n/a"
    b_alloc_s = f"{b_alloc:,.0f} B/op" if b_alloc is not None else "n/a"
    a_alloc_s = f"{a_alloc:,.0f} B/op" if a_alloc is not None else "n/a"
    print(f"{name:<16} {b_score:>14,.2f} {a_score:>14,.2f} {b_unit:<10} "
          f"{b_alloc_s:>14} {a_alloc_s:>14} {change:>13}")
EOF
