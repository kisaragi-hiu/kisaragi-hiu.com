export LANG=en_US.UTF-8

.DEFAULT_GOAL := build

.PHONY: clean zip css dev lint

lint:
	bunx @biomejs/biome lint .

bun.lockb: package.json
	bun install

dev:
	hugo server --buildDrafts

static/js:
	mkdir -p static/js/

clean:
	git clean -Xdf

zip: public.zip

public.zip: public
	cd public/ && 7z a ../public.zip .

# the modified timestamp gets messed up on my system; fix that with
# the `touch`.
public: static/js
	hugo --minify
	@touch public

vercel.json: generate-vercel-config.ts
	bun generate-vercel-config.ts > vercel.json

build.vercel: static/js vercel.json
	@hugo --minify -d .vercel/output/static
	@echo "Creating Vercel output config..."
	@echo '{"version":3}' > .vercel/output/config.json
