.PHONY: all clean

-include config.mk

all: lrs_build

-include lrs_build.d
-include obj/lrsb_types.d
-include obj/lrsb_util.d
-include obj/lrsb_lexer.d
-include obj/lrsb_parser.d
-include obj/lrsb_eval.d
-include obj/lrsb_funcs.d

obj:
	mkdir -p obj

obj/liblrsb_types.rlib: | obj
	lrsc $(lib_ops) --emit=link,dep-info -L obj --out-dir obj liblrsb/types/lib.rs

obj/liblrsb_util.rlib: obj/liblrsb_types.rlib
	lrsc $(lib_ops) --emit=link,dep-info -L obj --out-dir obj liblrsb/util/lib.rs

obj/liblrsb_lexer.rlib: obj/liblrsb_types.rlib
	lrsc $(lib_ops) --emit=link,dep-info -L obj --out-dir obj liblrsb/lexer/lib.rs

obj/liblrsb_parser.rlib: obj/liblrsb_types.rlib obj/liblrsb_lexer.rlib
	lrsc $(lib_ops) --emit=link,dep-info -L obj --out-dir obj liblrsb/parser/lib.rs

obj/liblrsb_eval.rlib: obj/liblrsb_types.rlib
	lrsc $(lib_ops) --emit=link,dep-info -L obj --out-dir obj liblrsb/eval/lib.rs

obj/liblrsb_funcs.rlib: obj/liblrsb_types.rlib obj/liblrsb_eval.rlib
	lrsc $(lib_ops) --emit=link,dep-info -L obj --out-dir obj liblrsb/funcs/lib.rs

lrs_build: obj/liblrsb_types.rlib obj/liblrsb_util.rlib obj/liblrsb_lexer.rlib obj/liblrsb_parser.rlib obj/liblrsb_eval.rlib obj/liblrsb_funcs.rlib
	lrsc $(bin_ops) --emit=link,dep-info -L obj src/main.rs

clean:
	rm -rf obj
	rm -f lrs_build
