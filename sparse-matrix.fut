import "/futlib/monoid"

import "narray/nonregular"
import "narray/i32narray"

-- Narray type for tuples of i32
module narrayi32tuple: (narray with t = (i32, i32)) =  {
  type t = (i32, i32)
  type repr = { vals : []t,
                flgs : []i32 }
}
module narrayi32tuple_sgm = sgmNarray narrayi32tuple
module narrayi32tuple_ops = ops narrayi32tuple

module type nmatrix = {
  module vals : narray
  type repr = {rows : vals.repr,
               row_numb: []i32}
  val mk: vals.repr -> []i32 -> repr
  val get_elem: repr -> (i32, i32) -> i32
  val sum_rows: repr -> repr
  val mul_vec_sparse : repr -> []vals.t -> []vals.t
  val mul_vec_dense : repr -> []i32 -> []i32
  val mul_rows_by_vec_sparse : repr -> []vals.t -> repr
  val mul_rows_by_vec_dense : repr -> []i32 -> repr
}

module i32nmatrix : nmatrix with vals.t = (i32, i32) = {
  module vals = { type t = (i32, i32)
                  type repr = {vals : [](i32, i32), flgs : []i32}}
  type repr = {rows : vals.repr, row_numb : []i32}
  let mk (v: vals.repr) (rs: []i32) = {rows = v, row_numb = rs}

  let first [n] (x: i32) (ns: [n]i32) : i32 =
    reduce (\acc i -> if (acc == -1 && ns[i] == x) then i else acc) (-1) (iota n)

  let get_elem (m : repr) (x : i32, y : i32) =
    let row = narrayi32tuple_ops.ith m.rows (first x (m.row_numb))
    let pair = (filter (\(i, _) -> i == y) row)
    in (pair[0]).2

  module sum_second : (monoid with t = (i32, i32)) = {
    type t = (i32, i32)
    let ne = (0, 0)
    let op (x: t) (y: t) = (0, x.2+y.2)
  }

  module reduceplus_second = narrayi32tuple_sgm.sreduce sum_second

  let sum_rows (m : repr) =
    let new_narray = reduceplus_second.sgmReduce m.rows
    in mk new_narray m.row_numb

  module is_not_zero = {
    type a = vals.t
    type b = bool
    let op (x:a) = x.2 != 0
  }

  module not_zero_seg = narrayi32tuple_sgm.sfilter is_not_zero

  let mul_rows_by_vec_sparse (m: repr) (v: []vals.t) =
    let mul_mat = map (\(mat_col_i, mat_val_i) ->
                       let idx = first mat_col_i (unzip v).1
                       in if idx != -1 then (mat_col_i, (v[idx]).2 * mat_val_i)
                          else (mat_col_i, 0)) m.rows.vals
    let new_narray = not_zero_seg.sgmFilter (narrayi32tuple_ops.mk mul_mat (m.rows.flgs))
    in mk new_narray m.row_numb

  let mul_vec_sparse (m: repr) (v: []vals.t) =
    let mul_mat = map (\(mat_col_i, mat_val_i) ->
                       let idx = first mat_col_i (unzip v).1
                       in if idx != -1 then (v[idx]).2 * mat_val_i
                          else 0) m.rows.vals
    let mul_sparse_mat = narray_i32lib.reduceplus (narrayi32_ops.mk mul_mat (m.rows.flgs))
    in zip mul_sparse_mat.vals m.row_numb

  let mul_rows_by_vec_dense (m: repr) (v: []i32) =
    let mul_mat = map (\(mat_col_i, mat_val_i) -> (mat_col_i, mat_val_i * v[mat_col_i])) m.rows.vals
    let new_narray = not_zero_seg.sgmFilter (narrayi32tuple_ops.mk mul_mat (m.rows.flgs))
    in mk new_narray m.row_numb

  let mul_vec_dense (m: repr) (v: []i32) =
    let mul_mat = map (\(mat_col_i, mat_val_i) -> mat_val_i * v[mat_col_i]) m.rows.vals
    let mul_sparse_mat = narray_i32lib.reduceplus (narrayi32_ops.mk mul_mat (m.rows.flgs))
    let zeroes = replicate (m.row_numb[(length m.row_numb)-1]+1) 0
    in scatter zeroes m.row_numb (mul_sparse_mat.vals)
}

let main() =
  let narr = narrayi32tuple_ops.mk [(0,1), (2,2), (3,1), (1,1)] [1,0,0,1]
  let b = i32nmatrix.mk narr [0,3]
  let c = i32nmatrix.mul_vec_sparse b [(1, 3), (2,4)]
  let d = i32nmatrix.mul_rows_by_vec_sparse b [(1, 3), (2,4)]
  let e = i32nmatrix.mul_vec_dense b [0,3,4,0]
  let f = i32nmatrix.mul_rows_by_vec_dense b [0,3,4,0]
  in ((unzip c).1, (unzip c).2,
      e,
      i32nmatrix.get_elem f (0,2), i32nmatrix.get_elem f (3,1),
      i32nmatrix.get_elem d (0,2), i32nmatrix.get_elem d (3,1))


--