.DEFAULT_GOAL := build

.PHONY: build category category-source clean css html org org-files serve tag-source tags templates zip

serve: public
	hugo server

clean:
	git clean -Xdf

zip: public
	@rm public.zip -r || true
	@rm public/.* -r || true
	@rm public/*.{sh,md,org} || true
	@rm public/*/*.org || true
	@rm public/rkt -rf || true
	@rm public/*/compiled/ -rf || true
	@rm public/template.html || true
	cd public/ && 7z a ../public.zip .

public: css
	hugo

css: static/css/main.css

static/css/main.css: static/css/main.css.pp
	raco pollen render static/css/main.css.pp
	sed -i '/^ *$$/d' static/css/main.css

