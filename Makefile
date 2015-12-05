.PHONY: all clean

-include config.mk

all: lrs_build

-include lrs_build.d
-include obj/lrsb_types.d
-include obj/lrsb_util.d
-include obj/lrsb_lexer.d
-include obj/lrsb_parser.d
-include obj/lrsb_eval.d

obj:
	mkdir -p obj

obj/liblrsb_types.rlib: | obj
	lrsc $(lib_ops) --emit=link,dep-info -L obj --out-dir obj src/lrsb/types/lib.rs

obj/liblrsb_util.rlib: obj/liblrsb_types.rlib
	lrsc $(lib_ops) --emit=link,dep-info -L obj --out-dir obj src/lrsb/util/lib.rs

obj/liblrsb_lexer.rlib: obj/liblrsb_types.rlib
	lrsc $(lib_ops) --emit=link,dep-info -L obj --out-dir obj src/lrsb/lexer/lib.rs

obj/liblrsb_parser.rlib: obj/liblrsb_types.rlib obj/liblrsb_lexer.rlib
	lrsc $(lib_ops) --emit=link,dep-info -L obj --out-dir obj src/lrsb/parser/lib.rs

obj/liblrsb_eval.rlib: obj/liblrsb_types.rlib
	lrsc $(lib_ops) --emit=link,dep-info -L obj --out-dir obj src/lrsb/eval/lib.rs

lrs_build: obj/liblrsb_types.rlib obj/liblrsb_util.rlib obj/liblrsb_lexer.rlib obj/liblrsb_parser.rlib obj/liblrsb_eval.rlib
	lrsc $(bin_ops) --emit=link,dep-info -L obj src/main.rs

clean:
	rm -rf obj
	rm -f lrs_build
