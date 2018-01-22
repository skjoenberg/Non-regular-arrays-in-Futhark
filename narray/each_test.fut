-- Testing the sparse-matrix implementation
-- ==
-- input { [1,0,2,5] [1,1,0,1] }
-- output { [1,0,4,25] [1,1,0,1] }
--
-- input { [1,2,3,4,5] [1,0,0,0,0] }
-- output { [1,4,9,16,25] [1,0,0,0,0] }
--
-- input { [1,2,3,4,5] [1,1,1,1,1] }
-- output { [1,4,9,16,25] [1,1,1,1,1] }


import "i32narray"
import "lambda/lambda"

module square : (lambda with a=i32 with b=i32)  = {
  type a = i32
  type b = i32
  let op (x : a) = x i32.* x
}

module n_square = sgm.each square

let main (vals: []i32) (flgs: []i32) =
  let narr = narrayi32_ops.mk vals flgs
  let narr_squared = n_square.each narr
  in (narr_squared.vals, narr_squared.flgs)