let f i =
	((-1.0) ** i) /. ((2.0 *. i) +. 1.0)

let leibniz_pi delta =
	if delta < 0.0 then -1
	else
	let pi = 4.0 *. (atan 1.0) in
	let rec pi_loop k acc =
		let diff =
			if (4.0 *. acc) >= pi then
				(4.0 *. acc) -. pi
			else
				pi -. (4.0 *. acc)
		in
		if diff < delta then k
		else pi_loop (k + 1) (acc +. (f (float_of_int k)))
	in
	pi_loop 0 0.0

let () =
	print_int (leibniz_pi (-1.0)) ; print_char '\n';
	print_int (leibniz_pi 0.0043) ; print_char '\n';
	print_int (leibniz_pi 0.00000758) ; print_char '\n';
	print_int (leibniz_pi 1.0) ; print_char '\n'
	
