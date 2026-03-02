SHELL   = /usr/bin/bash
IMAGES  = reddit-graph.png reddit-graph-directed.png reddit-graph-directed-users-1.png reddit-graph-directed-users-full.png
TEX     = lualatex
TEXARGS = -halt-on-error -interaction=nonstopmode

all: $(IMAGES)

clean:
	rm -f $(IMAGES)

%.eps: %.tex graphstyle.sty Makefile
	wd=$$(pwd); tmpdir=$$(mktemp -d); trap 'rm -rf $$tmpdir' EXIT; cp -t $$tmpdir $^; cd $$tmpdir; $(TEX) $(TEXARGS) $*.tex; cp $*.pdf $$wd;
	pdftops -eps -origpagesizes -level3 -nocrop -r 1200 $*.pdf $@

%.png: %.eps
	convert -density 1200 $^ -resize x350 $@

.PHONY: all clean