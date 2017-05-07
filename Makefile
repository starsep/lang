BINARIES=interpreter TestStarsepLang
SHELL=/usr/bin/env bash
SOURCES=src/Interpreter.hs src/Main.hs src/Environment.hs src/Typecheck.hs src/Errors.hs

.PHONY: all clean build docs test testGood testBad testWarn run runWarn runBad runGood linkSources

all: $(BINARIES)

test: testGood testBad testWarn

define test_examples
	@for e in $1/* ; do \
		echo -e "\e[93m----------- TESTING\e[96m $$e \e[93m--------------\e[0m"; \
		./TestStarsepLang < "$$e" ; \
	done
endef

testGood: good TestStarsepLang
	$(call test_examples,$<)

testBad: bad TestStarsepLang
	$(call test_examples,$<)

testWarn: warn TestStarsepLang
	$(call test_examples,$<)

define run_examples
	@for e in $1/* ; do \
		echo -e "\e[93m----------- RUNNING\e[96m $$e \e[93m--------------\e[0m"; \
		./interpreter < "$$e" ; \
	done
endef

run: runGood runBad runWarn

runGood: good interpreter
	$(call run_examples,$<)

runBad: bad interpreter
	$(call run_examples,$<)

runWarn: warn interpreter
	$(call run_examples,$<)

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
