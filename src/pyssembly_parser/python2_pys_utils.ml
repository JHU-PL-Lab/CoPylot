let uid_counter = ref (-1);;
let next_uid () =
  let n = !uid_counter in
  uid_counter :=  n-1;
  n

let reset_uid () =
  uid_counter := -1;
  ()
