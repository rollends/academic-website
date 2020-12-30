IMAGES=reddit-graph.png reddit-graph-directed.png reddit-graph-directed-users-1.png reddit-graph-directed-users-full.png

all: $(IMAGES)

clean:
	rm -f *.eps *.png *.aux *.log

%.eps: %.tex graphstyle.sty
	lualatex $*.tex
	pdftops -eps -origpagesizes -level3 -nocrop -r 1200 $*.pdf $@

%.png: %.eps
	convert -density 1200 $^ -resize x350 $@

.PHONY: all clean