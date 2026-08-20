#!/usr/bin/env python3
"""Attribute a hot method to its callers.

Reads `jfr print --events jdk.ExecutionSample <recording>` on stdin and, for
every stack containing a frame matching the given substring, counts the first
caller above it that is not generic plumbing (trampoline, currying, csegen
constants, Prelude fold bodies). Answers "who is driving this hot method?".

Usage:
  jfr print --events jdk.ExecutionSample typecheck.jfr \
      | callers.py <method-substring> [--raw]

  --raw   do not skip plumbing frames; show the immediate caller
"""

import re
import sys
from collections import Counter

PLUMBING = ("tailRecFrame", "$tcOpt", "Runtime.", "Functions",
            "lambda$csegen", "M_Prelude")


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    target = sys.argv[1]
    raw = "--raw" in sys.argv
    callers = Counter()
    frames = []
    in_stack = False
    matched_stacks = 0
    for line in sys.stdin:
        if "stackTrace" in line:
            in_stack = True
            frames = []
            continue
        if in_stack:
            m = re.match(r"\s*([\w.$]+)\(", line)
            if m:
                frames.append(m.group(1))
            else:
                in_stack = False
                for i, frame in enumerate(frames):
                    if target in frame:
                        matched_stacks += 1
                        for caller in frames[i + 1:]:
                            if target in caller:
                                continue
                            if not raw and any(p in caller for p in PLUMBING):
                                continue
                            callers[caller] += 1
                            break
                        break
    print(f"# stacks containing '{target}': {matched_stacks}\n")
    for caller, count in callers.most_common(20):
        print(f"{count:6d}  {caller}")


if __name__ == "__main__":
    main()
