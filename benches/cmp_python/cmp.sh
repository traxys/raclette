#!/usr/bin/env bash

mkdir -p md
mkdir -p json

for bench in *.bench; do
	printf "%s# %s%s\n" \
		"$(tput bold)" \
		"$(basename "$bench" .bench)" \
		"$(tput sgr0)"
	printf "  - python: %s%s%s\n" \
		"$(tput sitm)" \
		"$(head -n1 "$bench")" \
		"$(tput sgr0)"
	printf "  - raclette: %s%s%s\n" \
		"$(tput sitm)" \
		"$(tail -n1 "$bench")" \
		"$(tput sgr0)"

	hyperfine \
		--export-markdown "md/$(basename "$bench" .bench).md" \
		--export-json "json/$(basename "$bench" .bench).json" \
		--warmup 1 \
		--shell=none \
		-n python "python -c '$(head -n1 "$bench")'" \
		-n raclette "../../target/release/raclette -c '$(tail -n1 "$bench")'"
done
