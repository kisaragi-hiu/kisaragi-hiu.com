export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

.PHONY: clean zip css dev

node_modules: package.json
	npm install

dev-hugo:
	hugo server --disableFastRender

dev-tailwind:
	npx tailwindcss --postcss -i css/src.css -o static/css/built.css --watch

js:
	mkdir -p static/js/

dev:
	npx concurrently "make dev-hugo" "make dev-tailwind"

static/css/built.css: css/src.css
	npx tailwindcss --minify --postcss -i css/src.css -o static/css/built.css

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

MODULES := public/barren-moon public/timer public/tftt public/list-of-plants-of-formosa

modules: $(MODULES)

public/barren-moon:
	-rm -rf "$@"
	git clone "https://gitlab.com/kisaragi-hiu/barren-moon" -b release "$@"
public/list-of-plants-of-formosa:
	-rm -rf "$@"
	git clone "https://gitlab.com/kisaragi-hiu/list-of-plants-of-formosa" -b release "$@"
public/timer:
	-rm -rf "$@"
	git clone "https://github.com/kisaragi-hiu/timer" -b release "$@"
public/tftt:
	-rm -rf "$@"
	git clone "https://github.com/kisaragi-hiu/trumpet-fingerings-practice-tool-experiment" -b release "$@"

.PHONY: modules $(MODULES)
