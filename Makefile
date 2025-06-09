help:
	@echo "Makefile commands:"
	@echo "  make build    - Build the package"
	@echo "  make install  - Install the package"
	@echo "  make docs     - Generate documentation"
	@echo "  make check    - Check the package"
	@echo "  make try      - Run the package to ensure it loads correctly"

build: docs
	R CMD build .

install: docs
	R CMD INSTALL .

check: build
	R CMD check --as-cran citer_*.tar.gz

docs:
	Rscript -e "devtools::document()"

try:
	Rscript -e "citer::citer_on_load()"

.PHONY: help build install docs try
