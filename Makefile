BINARIES=interpreter TestStarsepLang
SHELL=/usr/bin/env bash

.PHONY: all clean parser docs test

all: $(BINARIES)

test: TestStarsepLang good
	for e in good/* ; do \
		./TestStarsepLang < "$$e" ; \
	done

TestStarsepLang: parser
	cd $< && \
	happy -gca ParStarsepLang.y && \
	alex -g LexStarsepLang.x && \
	ghc --make TestStarsepLang.hs -o ../TestStarsepLang

interpreter: src/Main.hs TestStarsepLang
	ln -sf ../src/Main.hs parser && \
	ln -sf ../src/Interpreter.hs parser && \
	cd parser && \
	ghc --make Main.hs -o ../$@

parser: grammar/StarsepLang.cf
	mkdir -p $@ && \
	cd $@ && \
	bnfc -haskell ../$<

docs: docs/DocStarsepLang.html docs/DocStarsepLang.pdf

parser/DocStarsepLang.txt: parser

docs/DocStarsepLang.html: parser/DocStarsepLang.txt
	txt2tags -t html -o $@ $<

parser/DocStarsepLang.tex: parser/DocStarsepLang.txt
	txt2tags -t tex -o $@ $<

docs/DocStarsepLang.pdf: parser/DocStarsepLang.tex
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make -outdir=tmp $< && \
	mv tmp/DocStarsepLang.pdf $@

clean:
	rm -rf parser build tmp $(BINARIES)
