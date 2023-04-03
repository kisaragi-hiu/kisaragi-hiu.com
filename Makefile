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

static/_redirects: _redirects.js
	node _redirects.js > static/_redirects

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

build.vercel: static/css/built.css js
	@hugo --minify -d .vercel/output/static
	@echo "Creating Vercel output config..."
	@echo '{"version":3}' > .vercel/output/config.json
