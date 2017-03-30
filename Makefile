all: bnfc_parser

bnfc_parser: grammar/StarsepLang.cf
	mkdir -p bnfc_parser && \
	cd bnfc_parser && \
	bnfc ../grammar/StarsepLang.cf

clean:
	rm -rf bnfc_parser build
