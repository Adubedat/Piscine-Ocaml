let converges (f: 'a -> 'a) x n =
	let rec converges_loop x n =
		if n < 0 then false
		else if f x = x then true
		else converges_loop (f x) (n - 1)
	in
	converges_loop x n

let () =
	print_endline (string_of_bool (converges (fun x -> x / 2) 2 0));
	print_endline (string_of_bool (converges (fun x -> x / 2) 2 (-1)));
	print_endline (string_of_bool (converges (fun x -> x / 2) 2 2));
	print_endline (string_of_bool (converges (fun x -> x / 2) 2 3));
	print_endline (string_of_bool (converges (( * ) 2) 2 5))
