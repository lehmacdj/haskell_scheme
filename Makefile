all: hscheme

hscheme: $(wildcard *.hs)
	ghc -o hscheme --make Main.hs

.PHONY: clean
clean:
	rm *.o *.hi hscheme
