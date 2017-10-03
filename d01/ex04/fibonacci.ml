let rec fibonacci n =
	let rec fibo_loop n acc1 acc2 =
		if n < 0 then -1
		else if n = 0 then 0
		else if n = 1 then acc2
		else fibo_loop (n - 1) acc2 (acc1 + acc2)
	in
	fibo_loop n 0 1

let() =
	print_int (fibonacci 0) ; print_char '\n';
	print_int (fibonacci 6) ; print_char '\n';
	print_int (fibonacci (-42)) ; print_char '\n';
	print_int (fibonacci 1) ; print_char '\n';
	print_int (fibonacci 3) ; print_char '\n'
		
