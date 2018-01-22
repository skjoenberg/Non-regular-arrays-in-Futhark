.PHONY: all clean

FC=futhark-c
FC_FLAGS=

all: sparse

sparse:	sparse-matrix.fut
	$(FC) $< -o $@

test:
	futhark-test sparse-test.fut dense-test.fut

clean:
	rm -f sparse-matrix sparse-matrix.c
	rm -f sparse sparse.c
	rm -f dense-test dense-test.c
	rm -f sparse-test sparse-test.c
