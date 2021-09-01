export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

.PHONY: serve clean zip css

dev-hugo:
	hugo server

dev-tailwind:
	npx tailwindcss --postcss -i static/css/tailwind-main.css -o static/css/built.css --watch

js:
	mkdir -p static/js/
	cp node_modules/medium-zoom/dist/medium-zoom.min.js static/js/

# Visit the page after a second
# Unless Firefox is already open and has a tab visiting the page
# (I've only bothered to support Firefox here)
open-browser:
	-(sleep 1 && \
	 !(pgrep firefox && python firefox-page-opened.py "localhost:1313") && \
	 xdg-open "http://localhost:1313")

dev: public static/css/built.css
	npx concurrently "make open-browser" "make dev-hugo" "make dev-tailwind"

clean:
	git clean -Xdf

zip: public.zip

public.zip: public
	cd public/ && 7z a ../public.zip .

# the modified timestamp gets messed up on my system; fix that with
# the `touch`.
public: static/css/built.css js
	hugo --minify
	@touch public

static/css/built.css: static/css/tailwind-main.css
	npx tailwindcss --postcss -i static/css/tailwind-main.css -o static/css/built.css
