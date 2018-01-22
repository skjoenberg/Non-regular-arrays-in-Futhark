-- Testing the sparse-matrix implementation
-- ==
-- input { [1,2] -- ms
--         [0,1] -- cs
--         [1,1] -- fs
--         [0,1] -- rs
--         [3,4] -- v
--       }
-- output { [3,8] }
-- input { [1,2,1,1] -- ms
--         [0,2,3,1] -- cs
--         [1,0,0,1] -- fs
--         [0,3] -- rs
--         [0,3,4,0] -- vs
--       }
-- output { [8,0,0,3] }
-- input { [1,2,3,4,5] -- ms
--         [0,1,3,0,2] -- cs
--         [1,0,0,1,0] -- fs
--         [0,1] -- rs
--         [6,0,7,0] -- vs
--       }
-- output { [6,59] }


import "sparse-matrix"

let main (ms : []i32)  -- ms: matrix values
         (cs : []i32)  -- cs: column indexes
         (fs : []i32)  -- fs: flags
         (rs : []i32)  -- rs: row indexes
         (v : []i32)   -- v: dense vector
         : []i32 =
  let mat = (zip cs ms)
  let narr = narrayi32tuple_ops.mk mat fs
  let b = i32nmatrix.mk narr rs
  let c = i32nmatrix.mul_vec_dense b v
  in c
