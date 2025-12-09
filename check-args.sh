#!/bin/bash

for bench in fannkuch-redux; do
    echo "=== $bench ==="
    racket benchmarks/shootout/$bench.rkt --help 2>&1 | grep -B1 -A1 "arg>" | head -5
    echo
done
