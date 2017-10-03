let iter (f: int -> int) x n =
	let rec iter_loop x n =
		if n < 0 then -1
		else if n = 0 then x
		else iter_loop (f x) (n - 1)
	in
	iter_loop x n

let () =
	print_int (iter (fun x -> x * 2) 2 0) ; print_char '\n';
	print_int (iter (fun x -> x * 2) 2 (-1)) ; print_char '\n';
	print_int (iter (fun x -> x * x) 2 4) ; print_char '\n';
	print_int (iter (fun x -> x * 2) 2 4) ; print_char '\n'
