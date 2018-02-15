open Batteries;;
open Jhupllib;;
open Nondeterminism;;
open Nondeterminism_monad;;
open Analysis_types;;

let unary_operation op v =
  match op with
  | Unop_not ->
    begin
      match v with
      | Boolean_value b -> Enum.singleton (Boolean_value (not b))
      | _ -> Enum.empty ()
    end
  | Unop_is_function ->
    begin
      match v with
      | Function_value _ -> Enum.singleton (Boolean_value true)
      | _ -> Enum.singleton (Boolean_value false)
    end
  | Unop_is_int ->
    begin
      match v with
      | Integer_value _ -> Enum.singleton (Boolean_value true)
      | _ -> Enum.singleton (Boolean_value false)
    end
;;

let int_plus n1 n2 =
  [
    begin
      let%orzero Int_lossy Pos, Int_lossy Pos = n1,n2 in
      return (Integer_value n1)
    end;
    begin
      let%orzero Int_lossy Pos, Int_lossy Neg = n1,n2 in
      let%bind n =
        pick_enum (List.enum [Int_lossy Pos;Int_lossy Neg;Int_exact 0]) in
      return (Integer_value n)
    end;
    begin
      let%orzero Int_lossy Pos, Int_exact n = n1,n2 in
      if n >= 0 then return (Integer_value n1)
      else let%bind sum =
             pick_enum (List.enum [Int_lossy Pos;Int_lossy Neg;Int_exact 0]) in
        return (Integer_value sum)
    end;
    begin
      let%orzero Int_lossy Neg, Int_lossy Pos = n1,n2 in
      let%bind n =
        pick_enum (List.enum [Int_lossy Pos;Int_lossy Neg;Int_exact 0]) in
      return (Integer_value n)
    end;
    begin
      let%orzero Int_lossy Neg, Int_lossy Neg = n1,n2 in
      return (Integer_value n1)
    end;
    begin
      let%orzero Int_lossy Neg, Int_exact n = n1,n2 in
      if n <= 0 then return (Integer_value n1)
      else let%bind sum =
             pick_enum (List.enum [Int_lossy Pos;Int_lossy Neg;Int_exact 0]) in
        return (Integer_value sum)
    end;
    begin
      let%orzero Int_exact x, Int_exact y = n1,n2 in
      if x + y > 0 then return (Integer_value (Int_lossy Pos))
      else if x + y = 0 then return (Integer_value (Int_exact 0))
      else return (Integer_value (Int_lossy Neg))
    end;
    (* begin
       let%orzero Zero,Zero = n1,n2 in
       return (Integer_value Zero)
       end;
       begin
       let%orzero Zero,Pos = n1,n2 in
       return (Integer_value Pos)
       end;
       begin
       let%orzero Zero,Neg = n1,n2 in
       return (Integer_value Neg)
       end; *)
  ]
  |> List.enum
  |> Enum.map Nondeterminism_monad.enum
  |> Enum.concat
;;

let int_minus n1 n2 =
  [
    begin
      let%orzero Int_lossy Pos, Int_lossy Pos = n1,n2 in
      let%bind n =
        pick_enum (List.enum [Int_lossy Pos;Int_lossy Neg;Int_exact 0]) in
      return (Integer_value n)
    end;
    begin
      let%orzero Int_lossy Pos, Int_lossy Neg = n1,n2 in
      return (Integer_value n1)
    end;
    begin
      let%orzero Int_lossy Pos, Int_exact n = n1,n2 in
      if n <= 0 then return (Integer_value n1)
      else let%bind diff =
             pick_enum (List.enum [Int_lossy Pos;Int_lossy Neg;Int_exact 0]) in
        return (Integer_value diff)
    end;
    begin
      let%orzero Int_lossy Neg, Int_lossy Pos = n1,n2 in
      return (Integer_value n1)
    end;
    begin
      let%orzero Int_lossy Neg, Int_lossy Neg = n1,n2 in
      let%bind n =
        pick_enum (List.enum [Int_lossy Pos;Int_lossy Neg;Int_exact 0]) in
      return (Integer_value n)
    end;
    begin
      let%orzero Int_lossy Neg, Int_exact n = n1,n2 in
      if n >= 0 then return (Integer_value n1)
      else let%bind diff =
             pick_enum (List.enum [Int_lossy Pos;Int_lossy Neg;Int_exact 0]) in
        return (Integer_value diff)
    end;
    begin
      let%orzero Int_exact x, Int_exact y = n1,n2 in
      if x - y > 0 then return (Integer_value (Int_lossy Pos))
      else if x - y = 0 then return (Integer_value (Int_exact 0))
      else return (Integer_value (Int_lossy Neg))
    end;
    (* begin
       let%orzero Zero,Zero = n1,n2 in
       return (Integer_value Zero)
       end;
       begin
       let%orzero Zero,Pos = n1,n2 in
       return (Integer_value Neg)
       end;
       begin
       let%orzero Zero,Neg = n1,n2 in
       return (Integer_value Pos)
       end; *)
  ]
  |> List.enum
  |> Enum.map Nondeterminism_monad.enum
  |> Enum.concat
;;

let has_key obj str =
  match AbstractStringMap.Exceptionless.find str obj with
  | None -> Enum.singleton (Boolean_value false)
  | Some _ -> Enum.singleton (Boolean_value true)
;;

let list_concat lst_1 lst_2 =
  [
    (let%orzero List_exact (l1,n1), List_exact (l2,n2) = lst_1,lst_2 in
     let c = max n1 n2 in
     return @@ (if List.length l1 + List.length l2 <= c then List_value(List_exact (l1 @ l2, c)) else List_value(List_lossy (l1 @ l2))));
    (let%orzero List_exact (l1,_), List_lossy l2 = lst_1,lst_2 in return @@ List_value(List_lossy (l1 @ l2)));
    (let%orzero List_lossy l1, List_exact (l2,_) = lst_1,lst_2 in return @@ List_value(List_lossy (l1 @ l2)));
    (let%orzero List_lossy l1, List_lossy l2 = lst_1,lst_2 in return @@ List_value(List_lossy (l1 @ l2)));
  ]
  |> List.enum
  |> Enum.map Nondeterminism_monad.enum
  |> Enum.concat
;;


let binary_operation op v1 v2 =
  match op with
  | Binop_intplus ->
    begin
      match v1,v2 with
      | Integer_value n1,Integer_value n2 -> int_plus n1 n2
      | _ -> raise @@ Utils.Invariant_failure ("Wrong type in Binop_intplus: " ^ show_value v1 ^ ", " ^ show_value v2)
    end
  | Binop_intminus ->
    begin
      match v1,v2 with
      | Integer_value n1,Integer_value n2 -> int_minus n1 n2
      | _ -> raise @@ Utils.Invariant_failure ("Wrong type in Binop_intminus: " ^ show_value v1 ^ ", " ^ show_value v2)
    end
  | Binop_haskey ->
    begin
      match v1,v2 with
      | Object_value obj, String_value str -> has_key obj str
      (* | Object_value obj, String_value (String_lossy) -> Enum.singleton (Boolean_value (if AbstractStringMap.is_empty obj then false else true)) *)
      | _ -> raise @@ Utils.Invariant_failure ("Wrong type in Binop_haskey: " ^ show_value v1 ^ ", " ^ show_value v2)
    end
  | Binop_listconcat ->
    begin
      match v1,v2 with
      | List_value lst_1, List_value lst_2 -> list_concat lst_1 lst_2
      | _ -> raise @@ Utils.Invariant_failure ("Wrong type in Binop_listconcat: " ^ show_value v1 ^ ", " ^ show_value v2)
    end
  | Binop_equals ->
    Enum.singleton (if equal_value v1 v2 then Boolean_value true else Boolean_value false)
    (* TODO: int, string, list behave differently *)
;;
