open Batteries;;
open Jhupllib;;
open Pp_utils;;

module type Vertex_type =
sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pp: t pretty_printer
end;;

(*
  Creating the graph data type inside of a module.  This allows us to keep the
  graph data type intentionally abstract, thus permitting safe indexing and
  other helpful features.
*)
module type Graph_sig =
sig
  module V: Vertex_type
  type t
  type edge = Edge of V.t * V.t

  val compare_edge: edge -> edge -> int
  val pp_edge: edge pretty_printer

  val empty : t

  val add_edge : edge -> t -> t

  val edges_of : t -> edge Enum.t

  val has_edge : edge -> t -> bool

  val edges_from : V.t -> t -> edge Enum.t

  val edges_to : V.t -> t -> edge Enum.t

  val preds : V.t -> t -> V.t Enum.t

  val succs : V.t -> t -> V.t Enum.t

  val num_edges : t -> int
end;;

module Make(V:Vertex_type) :
  Graph_sig with module V = V
=
struct
  module V = V

  type edge = Edge of V.t * V.t
  [@@deriving ord, show];;
  ignore @@ show_edge;;

  module Edge =
  struct
    type t = edge
    let compare = compare_edge
    let pp = pp_edge
  end;;

  module Edge_set =
  struct
    module Impl = Set.Make(Edge);;
    include Impl;;
    include Pp_utils.Set_pp(Impl)(Edge);;
  end;;

  type t = Graph of Edge_set.t

  let empty = Graph(Edge_set.empty);;

  let add_edge edge (Graph(s)) = Graph(Edge_set.add edge s);;

  let edges_of (Graph(s)) = Edge_set.enum s;;

  let has_edge edge (Graph(s)) = Edge_set.mem edge s;;

  let edges_from acl (Graph(s)) =
    Edge_set.enum s
    |> Enum.filter (fun (Edge(acl',_)) -> V.equal acl acl')
  ;;

  let succs acl g =
    edges_from acl g |> Enum.map (fun (Edge(_,acl)) -> acl)
  ;;

  let edges_to acl (Graph(s)) =
    Edge_set.enum s
    |> Enum.filter (fun (Edge(_,acl')) -> V.equal acl acl')
  ;;

  let preds acl g =
    edges_to acl g |> Enum.map (fun (Edge(acl,_)) -> acl)
  ;;

  let num_edges (Graph(s)) =
    Edge_set.cardinal s
  ;;

end;;
