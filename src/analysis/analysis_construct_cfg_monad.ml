open Batteries;;
open Analysis_lookup;;

module Cfg_monad :
sig
  type 'a m

  val return: 'a -> 'a m
  val bind: 'a m -> ('a -> 'b m) -> 'b m

  val pick_enum: 'a Enum.t -> 'a m
  val enum: 'a m -> pds -> 'a Enum.t * pds

  val update_pds : pds -> 'a m -> 'a m

  val zero: unit -> 'a m
end =
struct
  type 'a m = pds -> 'a Enum.t * pds;;

  let return x =
    fun pds ->
      Enum.singleton x, pds
  ;;

  let bind x f =
    fun pds ->
      let a, pds = x pds in
      let b_monads = Enum.map f a in
      Enum.fold
        (fun (bs, pds) m ->
           let new_bs, new_pds = m pds in
           Enum.append bs new_bs, new_pds
        )
        (Enum.empty (), pds)
        b_monads
  ;;

  let pick_enum enum =
    fun pds ->
      enum, pds
  ;;

  let enum m pds =
    m pds
  ;;

  let update_pds pds m =
    fun _ ->
      m pds
  ;;

  let zero () =
    fun pds ->
      Enum.empty (), pds
  ;;
end

let fold_cfg_monad_enum
    (starting_pds : pds)
    (monads: 'a Cfg_monad.m Enum.t)
  : 'a Enum.t * pds =
  Enum.fold
    (fun (bs, pds) m ->
       let new_bs, new_pds = Cfg_monad.enum m pds in
       Enum.append bs new_bs, new_pds
    )
    (Enum.empty (), starting_pds)
    monads
;;
