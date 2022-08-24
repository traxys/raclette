#!/usr/bin/env bash

for report in md/*.md; do
	echo "# $(basename "$report" .md)" | glow -
	glow "$report"
done
