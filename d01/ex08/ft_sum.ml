let ft_sum (f: int -> float) k n =
	if n < k then nan
	else
		let rec sum_loop k acc =
			if  k > n then acc
			else sum_loop (k + 1) (acc +. (f k))
		in
		sum_loop k 0.0

let () =
	print_float (ft_sum (fun i -> float_of_int (i * i)) 11 10) ; print_char '\n';
	print_float (ft_sum (fun i -> float_of_int (i * 2)) (-6) (0)) ; print_char '\n';
	print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10) ; print_char '\n'
