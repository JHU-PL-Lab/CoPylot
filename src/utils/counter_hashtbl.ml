module Counter =
struct
  type t = int

  let hash x = x
  let equal (x1 : int) (x2 : int) = (x1 = x2)
end
;;

module Counter_hashtbl = Hashtbl.Make(Counter);;
