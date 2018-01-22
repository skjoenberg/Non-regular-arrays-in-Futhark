.PHONY:		all clean

FC=futhark-c
FC_FLAGS=

all:		f32narray i32narray nonregular

i32narray:	i32narray.fut
		$(FC) $< -o $@

f32narray:	f32narray.fut
		$(FC) $< -o $@

nonregular:	nonregular.fut
		$(FC) $< -o $@

test:
	futhark-test each_test.fut sgmscan_test.fut sgmreduce_test.fut sgmfilter_test.fut

clean:
		rm -f i32narray i32narray.c
		rm -f f32narray f32narray.c
		rm -f nonregular nonregular.c
		rm -f each_test each_test.c
		rm -f sgmscan_test sgmscan_test.c
		rm -f sgmreduce_test sgmreduce_test.c
		rm -f sgmfilter_test sgmfilter_test.c
