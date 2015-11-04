musl := "/home/julian/c/musl"

.PHONY: all clean lib

all: lrs_build

lrs_build: main.rs ../lib/obj/liblrs.rlib Makefile
	@# lrsc -O -C lto -L ../lib/obj -C link-args="-nostdlib $(musl)/lib/crt1.o -L $(musl)/lib -static -l c -l pthread" "$<" -o "$@"
	@# lrsc -O -C lto -L ../obj -L /usr/local/lib "$<" -o "$@"
	@# lrsc -g -C lto -L ../lib/obj "$<" -o "$@"
	lrsc -O -C lto -L ../lib/obj "$<" -o "$@"

clean:
	rm bin/*
