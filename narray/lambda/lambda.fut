module type lambda = {
  type a
  type b
  val op : a -> b
}
