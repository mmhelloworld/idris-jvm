#!/usr/bin/env python3
"""Aggregate JFR allocation samples into a hotspot report.

Reads `jfr print --events jdk.ObjectAllocationSample <recording>` on stdin
and prints allocation share by object class and by (class, allocation site).
The allocation site is the innermost stack frame that is not JDK allocation
plumbing, i.e. the method that executed `new`.

Usage:
  jfr print --events jdk.ObjectAllocationSample typecheck.jfr | alloc_report.py

Shares are of sampled allocation *weight* (JFR extrapolates each sample to
the allocation volume it represents), so percentages estimate real
allocation-rate share.
"""

import re
import sys
from collections import Counter

UNITS = {"bytes": 1, "kB": 1024, "KiB": 1024,
         "MB": 1024 ** 2, "MiB": 1024 ** 2,
         "GB": 1024 ** 3, "GiB": 1024 ** 3}

TOP_N = 25


def main():
    by_class = Counter()
    by_site = Counter()
    cur_class = None
    cur_weight = 0.0
    in_stack = False
    for line in sys.stdin:
        line = line.rstrip()
        if "objectClass" in line:
            m = re.match(r"\s*objectClass = (.+?)( \(|$)", line)
            if m:
                cur_class = m.group(1).strip()
            continue
        m = re.match(r"\s*weight = ([\d.]+) ?(\w+)", line)
        if m:
            cur_weight = float(m.group(1)) * UNITS.get(m.group(2), 1)
            continue
        if "stackTrace" in line:
            in_stack = True
            continue
        if in_stack:
            m = re.match(r"\s*([\w.$]+)\(", line)
            in_stack = False
            if m and cur_class:
                by_class[cur_class] += cur_weight
                by_site[(cur_class, m.group(1))] += cur_weight
                cur_class = None

    total = sum(by_class.values())
    if not total:
        print("no allocation samples found (was the recording made with settings=profile?)")
        return
    print(f"# Allocation profile — {total / 1024**3:.2f} GiB sampled weight\n")
    print("## By class\n")
    print("| % | Class |")
    print("|---:|---|")
    for cls, weight in by_class.most_common(TOP_N):
        print(f"| {weight / total * 100:.1f} | `{cls}` |")
    print("\n## By (class, allocation site)\n")
    print("| % | Class | Allocated in |")
    print("|---:|---|---|")
    for (cls, site), weight in by_site.most_common(TOP_N):
        print(f"| {weight / total * 100:.1f} | `{cls}` | `{site}` |")


if __name__ == "__main__":
    main()
