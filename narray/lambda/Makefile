.PHONY:		all clean

FC=futhark-c
FC_FLAGS=

all:		lambda

lambda:		lambda.fut
		$(FC) $< -o $@

clean:
		rm -f lambda lambda.c
