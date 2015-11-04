musl := "/home/julian/c/musl"

.PHONY: all clean lib

all: lrs_build

lrs_build: main.rs ../lib/obj/liblrs.rlib Makefile
	@# rustc -O -C lto -L ../lib/obj -C link-args="-nostdlib $(musl)/lib/crt1.o -L $(musl)/lib -static -l c -l pthread" "$<" -o "$@"
	@# rustc -O -C lto -L ../obj -L /usr/local/lib "$<" -o "$@"
	@# rustc -g -C lto -L ../lib/obj "$<" -o "$@"
	rustc -O -C lto -L ../lib/obj "$<" -o "$@"

clean:
	rm bin/*
