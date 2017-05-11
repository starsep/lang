BUILD=build
SHELL=/usr/bin/env bash
GHCFLAGS=-Wall
GHC=ghc
TMP=tmp

BINARIES=interpreter TestStarsepLang
SOURCES=Environment.hs Errors.hs Interpreter.hs Main.hs Typecheck.hs
LINKED_SOURCES=$(addprefix $(BUILD)/,$(SOURCES))
BNFC_SOURCES_FILES=AbsStarsepLang.hs ErrM.hs LexStarsepLang.hs \
	ParStarsepLang.hs PrintStarsepLang.hs TestStarsepLang.hs
BNFC_SOURCES=$(addprefix $(BUILD)/,$(BNFC_SOURCES_FILES))

.PHONY: all clean docs test testGood testBad testWarn run runWarn runBad runGood

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
	-$(call test_examples,$<)

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
	-$(call run_examples,$<)

runWarn: warn interpreter
	$(call run_examples,$<)

TestStarsepLang: $(BNFC_SOURCES)
	cd $(BUILD) && \
	$(GHC) --make TestStarsepLang.hs -o ../$@

$(LINKED_SOURCES): $(BUILD)/%: src/%
	ln -srf $^ $(BUILD)

$(BNFC_SOURCES): grammar/StarsepLang.cf
	mkdir -p $(BUILD) && \
	cd $(BUILD) && \
	bnfc -haskell ../$< && \
	happy -gca ParStarsepLang.y && \
	alex -g LexStarsepLang.x

	echo "module Interpreter where" > src/Interpreter.hs && \
	tail -n+4 $(BUILD)/SkelStarsepLang.hs >> src/Interpreter.hs

	rm -f $(BUILD)/SkelStarsepLang.hs && \
	sed -i "/SkelStarsepLang/d" $(BUILD)/TestStarsepLang.hs

interpreter: $(BNFC_SOURCES) $(LINKED_SOURCES)
	cd $(BUILD) && \
	$(GHC) $(GHCFLAGS) --make Main.hs -o ../$@

docs: docs/DocStarsepLang.html docs/DocStarsepLang.pdf

$(BUILD)/DocStarsepLang.txt: $(BNFC_SOURCES)

docs/DocStarsepLang.html: $(BUILD)/DocStarsepLang.txt
	txt2tags -t html -o $@ $<

$(BUILD)/DocStarsepLang.tex: $(BUILD)/DocStarsepLang.txt
	txt2tags -t tex -o $@ $<

docs/DocStarsepLang.pdf: $(BUILD)/DocStarsepLang.tex
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make -outdir=$(TMP) $< && \
	mv $(TMP)/DocStarsepLang.pdf $@

clean:
	rm -rf $(BUILD) $(TMP) $(BINARIES)
