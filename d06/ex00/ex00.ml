module Str = 
    struct
        type t = string
        let compare = Pervasives.compare
    end

module StringSet = Set.Make(Str)

let () =
    let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
    StringSet.iter print_endline set;
    print_endline (StringSet.fold ( ^ ) set "")
