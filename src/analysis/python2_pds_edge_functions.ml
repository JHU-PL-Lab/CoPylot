open Batteries;;
open Python2_cfg;;
open Python2_pds;;
open Python2_normalized_ast;;


let create_edge_function (e : Control_cfg.edge) (state : Reachability.State.t)
  : (Reachability.Stack_action.t list * Reachability.Terminus.t) Enum.t =
  let Control_cfg.Edge (a1,a0) = e in
  let zero = Enum.empty in
  let%orzero Cfg_node(s) = state in
  [%guard (equal_vertex s a0)];
  let open Option.Monad in
  let zero () = None in
  let transitions_to_add = Enum.filter_map identity @@ List.enum
      [
        (* enum of pairs of (list of actions,terminus) *)
        (* begin
          let%orzero
            Assign(id,())
        end; *)
      ]
  in transitions_to_add
;;
