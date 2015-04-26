musl := "/home/julian/c/musl"

.PHONY: all clean lib

all: lrs_build

lrs_build: main.rs ../linux/obj/liblinux.rlib Makefile
	rustc -O -C lto -C no-stack-check -Z no-landing-pads -L ../linux/obj -C link-args="-nostdlib $(musl)/lib/crt1.o -L $(musl)/lib -static -l c -l pthread" "$<" -o "$@"
	@# rustc -O -C lto -Z no-landing-pads -L ../obj -L /usr/local/lib "$<" -o "$@"
	@# rustc -g -L ../obj "$<" -o "$@"

clean:
	rm bin/*
