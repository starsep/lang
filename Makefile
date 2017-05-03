BINARIES=interpreter TestStarsepLang
SHELL=/usr/bin/env bash
SOURCES=src/Interpreter.hs src/Main.hs src/Environment.hs src/Typecheck.hs src/Errors.hs

.PHONY: all clean build docs test linkSources

all: $(BINARIES)

test: TestStarsepLang good
	@for e in good/* ; do \
		echo -e "\e[93m$1----------- TESTING\e[96m $$e \e[93m$1--------------\e[0m"; \
		./TestStarsepLang < "$$e" ; \
	done

run: TestStarsepLang good
	@for e in good/* ; do \
		echo -e "\e[93m$1----------- RUNNING\e[96m $$e \e[93m$1--------------\e[0m"; \
		./interpreter < "$$e" ; \
	done

TestStarsepLang: build
	cd $< && \
	happy -gca ParStarsepLang.y && \
	alex -g LexStarsepLang.x && \
	ghc --make TestStarsepLang.hs -o ../$@

linkSources: $(SOURCES)
	ln -srf $^ build

interpreter: TestStarsepLang linkSources
	cd build && \
	ghc --make Main.hs -o ../$@

build/SkelStarsepLang.hs: build

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
