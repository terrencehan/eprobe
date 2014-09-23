#!/bin/sh

me="$(dirname $0)"
filter="$1"

uniq -c | awk '{print $2, " ", $1, " ", $3}' | $me/flamegraph.pl --filter "$filter"
