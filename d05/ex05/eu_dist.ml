let eu_dist a b =
    let alen = Array.length a in
    let blen = Array.length b in
    if alen <> blen || alen = 0 then 0.0 else
    begin
        let dist = ref 0.0 in
        for i = 0 to (alen - 1) do
            dist := !dist +. ((a.(i) -. b.(i)) ** 2.0)
        done;
        sqrt !dist
    end

let () =
    print_float (eu_dist [|0.0; 1.5|] [|4.5; 3.61|]); print_char '\n';
    print_float (eu_dist [|0.0; 0.0|] [|1.0; 0.0|]); print_char '\n';
    print_float (eu_dist [|0.0; 1.5|] [|1.5; 0.0|]); print_char '\n';
    print_float (eu_dist [|3.0; 3.0|] [|3.0; 2.0|]); print_char '\n'
