.PHONY: all clean

-include config.mk

all: lrs_build

-include lrs_build.d

lrs_build: main.rs
	lrsc $(ops) --emit=link,dep-info $<

clean:
	rm -f lrs_build
