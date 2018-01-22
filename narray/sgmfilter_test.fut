-- Testing the sparse-matrix implementation
-- ==
-- input { [4,6,4,6,4,6,6,6] [1,0,1,0,1,0,1,0] }
-- output { [6,6,6,6,6] [1,1,1,1,0] }
--
-- input { [4,5,6,7,8] [1,0,0,0,0] }
-- output { [6,7,8] [1,0,0] }
--
-- input { [4,5,6,7,8] [1,0,0,1,0] }
-- output { [6,7,8] [1,1,0] }
--

import "i32narray"
import "lambda/lambda"

module bigger_than_five : (lambda with a=i32 with b=bool)  = {
  type a = i32
  type b = bool
  let op (x : a) = x > 5
}

module n_btf = sgm.sfilter bigger_than_five

let main (vals: []i32) (flgs: []i32) =
  let narr = narrayi32_ops.mk vals flgs
  let narr_squared = n_btf.sgmFilter narr
  in (narr_squared.vals, narr_squared.flgs)