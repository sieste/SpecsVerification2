.DEFAULT_GOAL := nothing
ROOT_DIR = $(shell pwd)


# TEX specific variables
TEX_DIR  = $(ROOT_DIR)/tex
TITLE = ensemble-verification
RNW_FILE = $(TEX_DIR)/$(TITLE).Rnw
TEX_FILE = $(TEX_DIR)/$(TITLE).tex
PDF_FILE = $(TEX_DIR)/$(TITLE).pdf
BIB_FILE = $(TEX_DIR)/$(TITLE).bib


# R specific variables
RSCRIPT_CMD = Rscript --vanilla -e '.libPaths("/home/stefan/lib/R/")'
R_CMD = R --vanilla 
R_DIR = $(ROOT_DIR)/R
R_PKG = SpecsVerification2
R_PKG_DIR = $(R_DIR)/$(R_PKG)
R_PKG_version = $(shell awk -F": +" '/^Version/ { print $$2 }' $(R_PKG_DIR)/DESCRIPTION)
R_PKG_tgz = $(R_DIR)/$(R_PKG)_$(R_PKG_version).tar.gz
R_libtmp = $(R_DIR)/libtmp



nothing:
paper: $(PDF_FILE)
r-package: $(R_PKG_tgz)


# create pdf file using bibtex and pdflatex
$(PDF_FILE): $(BIB_FILE) $(TEX_FILE) 
	cd $(TEX_DIR);\
	pdflatex $(TEX_FILE);\
	bibtex $(TITLE);\
	pdflatex $(TEX_FILE);\
	pdflatex $(TEX_FILE);\
	cd $(ROOT_DIR)


# create the tex source with sweave
$(TEX_FILE): $(RNW_FILE) $(R_PKG_tgz)
	cd $(TEX_DIR);\
	$(RSCRIPT_CMD) -e 'Sweave("$(RNW_FILE)")';\
	cd $(ROOT_DIR)


# roxygenize, build, and install r library 
$(R_PKG_tgz): $(shell find $(R_PKG_DIR) -type f)
	cd $(R_DIR);\
	$(RSCRIPT_CMD) -e 'roxygen2::roxygenize(package.dir="$(R_PKG_DIR)", clean=TRUE)';\
	$(R_CMD) CMD build $(R_PKG);\
	$(R_CMD) CMD INSTALL $(R_PKG_tgz) -l $(R_libtmp);\
	cd $(ROOT_DIR)


