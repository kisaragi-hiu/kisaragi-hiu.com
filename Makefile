.DEFAULT_GOAL := build

.PHONY: build category-pages clean css serve zip

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

.cask:
	cask install

# * Turning Org files into Pollen Markup files
# We cannot use .pmd because Tag functions don't work there.
# We cannot use .pp.md because then the tag functions have to return Markdown.
# Ultimately we need a Org -> Pollen Markup converter.
# ORG = $(patsubst %.org,%.html.pm,$(wildcard *.org))
# org: $(ORG)
# $(ORG): %.html.pm: %.org
# 	emacs "$<" --batch -f ox-pollen-export-to-pollen --kill

org-files := $(patsubst %.org,%.html.pm,$(shell find . -name "*.org"))
all-files := $(patsubst %.html.pm,%.html, \
                        $(shell find . -name "*.html.pm" \
                                       -not -name "category.html.pm" \
                                       -not -path "*/tags/*" \
                                       -not -path "*/category/*") \
                        $(org-files))
templates := template.html

template.html: main-template.html.pp
	raco pollen render main-template.html.pp
	mv main-template.html template.html

%.html.pm: %.org .cask
	cask emacs "$<" --batch -l ox-pollen -f ox-pollen-export-to-pollen --kill

%.html: %.html.pm $(templates)
	raco pollen render "$<"

build: $(all-files) tags category css

css: css/main.css.pp

css/main.css.pp:
	raco pollen render css/main.css.pp
	sed -i '/^ *$$/d' css/main.css

# css: css/main.scss
# 	sassc css/main.scss css/main.css

# * Tags and Categories
tags: $(all-files)
	racket make-tag-pages.rkt
	raco pollen render -p tags/*.html.pm

category: category.html category-pages

category.html: $(templates) category-pages
	raco pollen render category.html

category-pages: $(all-files)
	racket make-category-pages.rkt
	raco pollen render -p category/*.html.pm
