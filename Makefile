BINARIES=interpreter TestStarsepLang
SHELL=/usr/bin/env bash

.PHONY: all clean build docs test linkSources

all: $(BINARIES)

test: TestStarsepLang good
	@for e in good/* ; do \
		echo -e "\e[93m$1----------- TESTING\e[96m $$e \e[93m$1--------------\e[0m"; \
		./TestStarsepLang < "$$e" ; \
	done

TestStarsepLang: build
	cd $< && \
	happy -gca ParStarsepLang.y && \
	alex -g LexStarsepLang.x && \
	ghc --make TestStarsepLang.hs -o ../$@

linkSources: src/Main.hs src/Interpreter.hs src/Environment.hs
	ln -srf $^ build

interpreter: TestStarsepLang linkSources
	cd build && \
	ghc --make Main.hs -o ../$@

build/SkelStarsepLang.hs: build

# src/Interpreter.hs: build/SkelStarsepLang.hs
#	echo "module Interpreter where" > $@
#	tail -n+4 $< >> $@

build: grammar/StarsepLang.cf
	mkdir -p $@ && \
	cd $@ && \
	bnfc -haskell ../$<

docs: docs/DocStarsepLang.html docs/DocStarsepLang.pdf

build/DocStarsepLang.txt: build

docs/DocStarsepLang.html: build/DocStarsepLang.txt
	txt2tags -t html -o $@ $<

build/DocStarsepLang.tex: build/DocStarsepLang.txt
	txt2tags -t tex -o $@ $<

docs/DocStarsepLang.pdf: build/DocStarsepLang.tex
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make -outdir=tmp $< && \
	mv tmp/DocStarsepLang.pdf $@

clean:
	rm -rf build tmp $(BINARIES)
