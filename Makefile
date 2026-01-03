

# Extracting current package version
VERSION := $(shell grep '^Version:' DESCRIPTION | awk '{print $$2}')

.PHONY: document
document:
	Rscript -e "devtools::document()"

.PHONY: install build
build: document
	@echo Building current version: $(VERSION)
	(cd ../ && R CMD build apache2logparser)
install: build
	@echo Installing current version: $(VERSION)
	(cd ../ && R CMD INSTALL apache2logparser_$(VERSION).tar.gz)
check: build
	@echo Checking current version: $(VERSION)
	(cd ../ && R CMD check apache2logparser_$(VERSION).tar.gz)

clean:
	-rm file*_demo.sqlite3
