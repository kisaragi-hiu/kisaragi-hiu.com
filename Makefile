.DEFAULT_GOAL := build

.PHONY: serve clean zip css

serve: public static/css/main.css
	sass "static/css/main.scss:static/css/main.css" --watch &
	hugo server

clean:
	git clean -Xdf

zip: public.zip

public.zip: public
	cd public/ && 7z a ../public.zip .

# the modified timestamp gets messed up on my system; fix that with
# the `touch`.
public: static/css/main.css
	hugo
	@touch public

static/css/main.css: static/css/main.scss
	sass --sourcemap=none "$<" "$@"
