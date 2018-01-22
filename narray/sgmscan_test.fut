-- Testing the sparse-matrix implementation
-- ==
-- input { [1,1,1,1,2,2,2,2] [1,0,0,0,1,0,0,0] }
-- output { [1,1,1,1,2,4,8,16] [1,0,0,0,1,0,0,0] }
--
-- input { [1,2,3,4,5,6] [1,0,0,1,0,0] }
-- output { [1,2,6,4,20,120] [1,0,0,1,0,0] }
--
-- input { [1,2,3,4,5,6] [1,1,1,1,1,1] }
-- output { [1,2,3,4,5,6] [1,1,1,1,1,1] }

import "i32narray"
import "futlib/monoid"

module mul : (monoid with t=i32)  = {
  type t = i32
  let ne = 1
  let op (x : t) (y : t) = x i32.* y
}

module scanmul = sgm.sscan mul

let main (vals: []i32) (flgs: []i32) =
  let narr = narrayi32_ops.mk vals flgs
  let narr_squared = scanmul.sgmScan narr
  in (narr_squared.vals, narr_squared.flgs)