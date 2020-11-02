.DEFAULT_GOAL := build

.PHONY: build category category-source clean css html org org-files serve tag-source tags templates zip

serve: build
	raco pollen start

clean:
	git clean -Xdf

zip: public
	cd public/ && 7z a ../public.zip .

public: build
	@rm public -r || true
	raco pollen publish . $(HOME)/public
	mv ~/public .

build: html css

html: templates tags category
	raco pollen render -p index.ptree

css: css/main.css.pp
	raco pollen render css/main.css.pp
	sed -i '/^ *$$/d' css/main.css

# css: css/main.scss
# 	sassc css/main.scss css/main.css

# * Turning Org files into Pollen Markup files
# We cannot use .pmd because Tag functions don't work there.
# We cannot use .pp.md because then the tag functions have to return Markdown.
# Ultimately we need a Org -> Pollen Markup converter.
# ORG = $(patsubst %.org,%.html.pm,$(wildcard *.org))
# org: $(ORG)
# $(ORG): %.html.pm: %.org
# 	emacs "$<" --batch -f ox-pollen-export-to-pollen --kill

org-files := $(patsubst %.org,%.html.pm,$(wildcard *.org)) $(patsubst %.org,%.html.pm,$(wildcard blog/*.org)) $(patsubst %.org,%.html.pm,$(wildcard projects/*.org))

$(org-files): %.html.pm: %.org
	cask emacs "$<" --batch -l ox-pollen -f ox-pollen-export-to-pollen --kill

.cask:
	cask install

org: .cask
	make -j$(shell nproc) $(org-files)

# * Tags and Categories
tags: org blog
	racket make-tag-pages.rkt
	raco pollen render -p tags/*.pm

category: org blog
	racket make-category-pages.rkt
	raco pollen render -p category/*.pm

# * Templates
templates: template.html

template.html: main-template.html.pp
	raco pollen render main-template.html.pp
	mv main-template.html template.html
