BINARIES=interpreter TestStarsepLang

all: $(BINARIES)

TestStarsepLang: bnfc_parser
	cd $< && \
	happy -gca ParStarsepLang.y && \
	alex -g LexStarsepLang.x && \
	ghc --make TestStarsepLang.hs -o ../TestStarsepLang

interpreter: src/Main.hs TestStarsepLang
	ln -sf ../src/Main.hs bnfc_parser && \
	ln -sf ../src/Interpreter.hs bnfc_parser && \
	cd bnfc_parser && \
	ghc --make Main.hs -o ../$@

bnfc_parser: grammar/StarsepLang.cf
	mkdir -p $@ && \
	cd $@ && \
	bnfc -haskell ../$<

bnfc_docs: docs/DocStarsepLang.html docs/DocStarsepLang.pdf

bnfc_parser/DocStarsepLang.txt: bnfc_parser

docs/DocStarsepLang.html: bnfc_parser/DocStarsepLang.txt
	txt2tags -t html -o $@ $<

bnfc_parser/DocStarsepLang.tex: bnfc_parser/DocStarsepLang.txt
	txt2tags -t tex -o $@ $<

docs/DocStarsepLang.pdf: bnfc_parser/DocStarsepLang.tex
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make -outdir=tmp $< && \
	mv tmp/DocStarsepLang.pdf $@

clean:
	rm -rf bnfc_parser build tmp $(BINARIES)
