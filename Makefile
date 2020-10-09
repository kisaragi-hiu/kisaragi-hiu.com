.DEFAULT_GOAL := help

.PHONY: build before-pollen html css publish

build: html css

before-pollen:
	racket before-pollen.rkt

templates: template.html

template.html:
	raco pollen render main-template.html.pp
	mv main-template.html template.html

category: before-pollen
	raco pollen render category/*.pm

html: templates category css
	raco pollen render index.ptree

css:
	raco pollen render css/main.css.pp

public: build
	@rm public -r || true
	raco pollen publish . $(HOME)/public
	mv ~/public .

clean:
	git clean -Xdf

publish: public

zip: public
	cd public/ && 7z a ../public.zip .
