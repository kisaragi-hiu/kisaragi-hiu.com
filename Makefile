export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

.PHONY: clean zip css dev

dev-hugo:
	~/git/hugo/hugo server --disableFastRender

dev-tailwind:
	npx tailwindcss --postcss -i css/src.css -o static/css/built.css --watch

js:
	mkdir -p static/js/

dev: public static/css/built.css
	npx concurrently "make dev-hugo" "make dev-tailwind"

clean:
	git clean -Xdf

zip: public.zip

public.zip: public
	cd public/ && 7z a ../public.zip .

# the modified timestamp gets messed up on my system; fix that with
# the `touch`.
public: static/css/built.css js modules
	hugo --minify
	@touch public

static/css/built.css: css/src.css
	npx tailwindcss --minify --postcss -i css/src.css -o static/css/built.css

modules: static/barren-moon
.PHONY: modules

static/barren-moon:
	git clone "git@gitlab.com:kisaragi-hiu/barren-moon" -b release static/barren-moon
