help:
	@echo "Makefile commands:"
	@echo "  make build    - Build the package"
	@echo "  make install  - Install the package"
	@echo "  make docs     - Generate documentation"

build: docs
	R CMD build .

install: docs
	R CMD INSTALL .

docs:
	Rscript -e "devtools::document()"

try:
	Rscript -e "citer::citer_on_load()"
	
.PHONY: help build install docs try
