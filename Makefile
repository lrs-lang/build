.PHONY: all clean

-include config.mk

all: lrs_build

-include lrs_build.d

lrs_build:
	lrsc $(ops) --emit=link,dep-info src/main.rs

clean:
	rm -f lrs_build
