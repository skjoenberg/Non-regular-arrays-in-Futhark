--import "/futlib/monoid"

import "nonregular"
import "monoid/i32monoids"

module narrayi32: (narray with t = i32) =  {
  type t = i32
  type repr = { vals : []t,
                flgs : []i32 }
}

module narrayi32_ops = ops narrayi32

module sgm = sgmNarray narrayi32

module narray_i32lib = {
  module i32scanplus = sgm.sscan i32plus
  let scanplus (x: narrayi32.repr) = i32scanplus.sgmScan x

  module i32scanmul = sgm.sscan i32mul
  let scanmul (x: narrayi32.repr) = i32scanmul.sgmScan x

  module i32reduceplus = sgm.sreduce i32plus
  let reduceplus (x: narrayi32.repr) = i32reduceplus.sgmReduce x

  -- iota only makes sense for i32
  let sgm_iota (x : []i32) : narrayi32.repr =
    let ones = replicate (length x) 1
    let scns = scanplus (narrayi32_ops.mk ones x)
    let vs   = map (\x -> x - 1) scns.vals
    in {vals = vs, flgs = x}
}
