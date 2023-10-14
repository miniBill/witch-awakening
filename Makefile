.PHONY: all
all: generated/Images.elm

GRADIENT_SRC = $(wildcard extract/*.png)
GRADIENT_OUT = $(patsubst extract/%.png,build/elm-codegen-flags/%.ppm,$(GRADIENT_SRC))

generated/Images.elm: codegen/Generate.elm codegen/Gen/Basics.elm build/elm-codegen-flags/sizes $(GRADIENT_OUT)
	yarn elm-codegen run --flags-from build/elm-codegen-flags

build/elm-codegen-flags/sizes: $(wildcard public/*.png) $(wildcard public/*.jpg) $(wildcard public/*.webp)
	mkdir -p build/elm-codegen-flags
	identify $^ > $@

codegen/Gen/Basics.elm: codegen/elm.codegen.json
	yarn elm-codegen install

build/elm-codegen-flags/%.ppm: extract/%.png
	mkdir -p build/elm-codegen-flags
	convert $^ -compress none $@

.PHONY: build
build:
	rm -rf out
	mkdir -p out
	mkdir -p out/public
	mkdir -p out/build
	yarn elm-watch make --optimize
	cp index.html favicon.ico out
	cp public/*.* out/public
	cp build/main.js out/build

.PHONY: run
run: generated/Images.elm
	sh -c "tmux new-session 'yarn elm-watch hot' \; split-window 'python -m http.server 8001'"
