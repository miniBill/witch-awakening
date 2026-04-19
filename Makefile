# Utils
.PHONY: build
build:
	(cd script; bun start)
	chmod -R a=rwX out || true
	rm -rf out
	mkdir -p out
	mkdir -p out/public
	mkdir -p out/build
	bunx elm-watch make --optimize
	cp -r index.html favicon.ico favicon out
	cp -r public/* out/public
	cp build/main.js out/build
	chmod -R a=rX out

.PHONY: run
run:
	(cd script; bun start)
	bunx elm-watch hot

.PHONY: test
test:
	(cd script; bun start)
	bunx elm-test-rs --watch

.PHONY: deploy
deploy: build
	rsync -a --no-times out/ witch-awakening.taglialegne.it:/var/www/witch-awakening.taglialegne.it
