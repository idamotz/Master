SRCDIR=src
SRCFILE=thesis
PDF=$(SRCFILE).pdf
CITATIONS=citations.bib

export TEXINPUTS := ifimaster/:duoforside/:$(TEXINPUTS)

pdf: $(SRCDIR)/$(SRCFILE).tex $(SRCDIR)/$(CITATIONS)
	cd $(SRCDIR) && latexmk -pdf -use-make $(SRCFILE) && mv $(PDF) ..


watch: $(SRCDIR)/$(SRCFILE).tex $(SRCDIR)/$(CITATIONS)
	cd $(SRCDIR) && latexmk -pdf -pvc -use-make $(SRCFILE) && mv $(PDF) ..

debug: $(SRCDIR)/$(SRCFILE).tex $(SRCDIR)/$(CITATIONS)
	cd $(SRCDIR) && pdflatex -shell-escape $(SRCFILE) && mv $(PDF) ..


clean:
	rm $(PDF); cd $(SRCDIR) && latexmk -CA; rm $(SRCFILE).bbl $(SRCFILE).run.xml; rm -rf _minted-thesis
