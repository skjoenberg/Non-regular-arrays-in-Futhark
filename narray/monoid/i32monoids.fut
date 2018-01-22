import "/futlib/monoid"

module i32plus : (monoid with t=i32) = {
  type t = i32
  let ne = 0
  let op (x: i32) (y: i32) = x + y
}

module i32mul : (monoid with t=i32) = {
  type t = i32
  let ne = 1
  let op (x: i32) (y: i32) = x * y
}
