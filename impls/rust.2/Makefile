%: src/bin/%.rs
	cargo build --release --bin $*
	cp target/release/$* $@

STEP0_DEPS = Cargo.toml
STEP1_DEPS = $(STEP0_DEPS) src/lib.rs src/reader.rs src/printer.rs

step0_repl: $(STEP0_DEPS)
step1_read_print: $(STEP1_DEPS)

.PHONY: clean

clean:
	cargo clean
	rm -f $(STEPS)
	rm -f mal
