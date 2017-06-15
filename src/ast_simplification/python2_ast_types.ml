type uid = int
[@@deriving eq, ord, show, to_yojson]
;;

type identifier = string
[@@deriving eq, ord, show, to_yojson]
;;

type 'a annotation =
    {
      uid: uid;
      exception_target: uid option;
      multi: bool;
      body: 'a;
    }
[@@deriving eq, ord, show, to_yojson]
;;
