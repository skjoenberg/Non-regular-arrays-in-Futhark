import "/futlib/monoid"

import "nonregular"
import "monoid/f32monoids"

module narrayf32: (narray with t = f32) =  {
  type t = f32
  type repr = { vals : []t,
                flgs : []i32 }
}

module narrayf32_ops = ops narrayf32

module narray_f32lib = {
  module sgm = sgmNarray narrayf32

  -- note that this is not associative
  module f32scanplus = sgm.sscan f32plus
  let scanplus (x : narrayf32.repr) = f32scanplus.sgmScan x

  module f32scanmul = sgm.sscan f32mul
  let scanmul (x : narrayf32.repr) = f32scanmul.sgmScan x

  module f32reduceplus = sgm.sreduce f32plus
  let reduceplus (x : narrayf32.repr) = f32reduceplus.sgmReduce x
}
