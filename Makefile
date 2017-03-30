all: bnfc_parser

bnfc_parser: grammar/StarsepLang.cf
	mkdir -p bnfc_parser && \
	cd bnfc_parser && \
	bnfc -m -haskell ../grammar/StarsepLang.cf

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
	rm -rf bnfc_parser build tmp
