import "/futlib/monoid"

module f32plus : (monoid with t=f32) = {
  type t = f32
  let ne = 0.0f32
  let op (x: f32) (y: f32) = x + y
}

module f32mul : (monoid with t=f32) = {
  type t = f32
  let ne = 1.0f32
  let op (x: f32) (y: f32) = x * y
}
