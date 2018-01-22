.PHONY:		all clean

FC=futhark-c
FC_FLAGS=

all:		i32monoids f32monoids

i32monoids:	i32monoids.fut
		$(FC) $< -o $@

f32monoids:	f32monoids.fut
		$(FC) $< -o $@

clean:
		rm -f f32monoids f32monoids.c
		rm -f i32monoids i32monoids.c
