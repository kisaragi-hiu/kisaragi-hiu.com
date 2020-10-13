.DEFAULT_GOAL := help

.PHONY: build before-pollen html css publish

zip: public
	cd public/ && 7z a ../public.zip .

public: build
	@rm public -r || true
	raco pollen publish . $(HOME)/public
	mv ~/public .

build: html css

html: templates tags category css
	raco pollen render index.ptree

css:
	raco pollen render css/main.css.pp

clean:
	git clean -Xdf

# * Tags and Categories
tag-source:
	racket make-tag-pages.rkt

tags: tag-source
	raco pollen render tags/*.pm

category-source:
	racket make-category-pages.rkt

category: category-source
	raco pollen render category/*.pm

# * Templates
templates: template.html

template.html:
	raco pollen render main-template.html.pp
	mv main-template.html template.html
