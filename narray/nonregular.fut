import "/futlib/monoid"

import "lambda/lambda"

module type narray = {
  type t
  type repr = { vals : []t,
                flgs : []i32 }
}

-- Some useful operations
module ops (narray: narray): {
  val mk: []narray.t -> []i32 -> narray.repr
  val ith : narray.repr -> i32 -> []narray.t
  val split : narray.repr -> (i32, i32) -> narray.repr
} = {
  let mk (valss: []narray.t) (flgss: []i32) : narray.repr =
    {vals = valss, flgs = flgss}
  let ith (narray: narray.repr) (i: i32) =
    let flgsum = map (\x -> x-1) (scan (+) 0 narray.flgs)
    in (unzip (filter (\(_, idx) -> idx == i) (zip narray.vals flgsum))).1
  let split (narray: narray.repr) (i: i32, j: i32) =
    let flgsum =  (unzip (filter (\(idx, _) -> idx != 0) (zip narray.flgs (iota (length narray.flgs))))).2
    in {vals=narray.vals, flgs=(scatter (copy narray.flgs) [flgsum[i]+j] [1])}
}

-- Segmented operations
module sgmNarray (N' : narray) = {
  module NOPS  = ops N'

  module sscan (M: monoid with t = N'.t) = {
    let sgmScan (narray : N'.repr) : N'.repr =
      let (vs, _) = unzip (scan (\(x, x_flag) (y, y_flag) ->
                                 if y_flag > 0
                                 then (y, x_flag | y_flag)
                                 else (M.op x y, x_flag | y_flag))
                           (M.ne, 0i32)
                           (zip narray.vals narray.flgs))
      in NOPS.mk vs narray.flgs
  }

  module sreduce (M: monoid with t = N'.t) = {
    module S = sscan M
    let sgmReduce (xs : N'.repr) =
      let len = length xs.flgs
      let flags = xs.flgs
      let scnvals = S.sgmScan xs
      let sgmends = map (\i -> if i == len - 1 then 1 else flags[i + 1]) (iota len)
      let (vs, _) = unzip (filter (\(_, y) -> y > 0) (zip scnvals.vals sgmends))
      in NOPS.mk vs (replicate (length vs) 1)
  }

--  module each (L: lambda with a = N'.t with b=N'.t) = {
  module each (L: lambda with a = N'.t with b = N'.t) = {
    let each (xs: N'.repr) = NOPS.mk (map L.op xs.vals) xs.flgs
  }

  module sfilter (L: lambda with a = N'.t with b=bool) = {
    module L_wrap = {
      type a = (i32, N'.t)
      type b = bool
      let op (x: a) = L.op x.2
    }

    let sgmFilter (xs : N'.repr) =
      let sgms  = scan (+) 0 xs.flgs
      let pairs = filter L_wrap.op (zip sgms xs.vals)
      let (fs, vs) = unzip(map (\(x, y) i -> if i == 0
                                              then (1, y)
                                              else if x > (pairs[i - 1]).1
                                                   then (1, y)
                                                   else (0, y)
                                ) (copy pairs) (iota (length pairs))
                          )
      in NOPS.mk vs fs
  }
}
