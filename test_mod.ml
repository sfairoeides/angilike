open BatBitSet ;;
open Printf ;;
open Sys ;;
(* PG = PointGraph ;;*)
module ABBS = ArrayBatBitSet ;;
module P3 = P32abbs ;;
module S = Skeleton ;;



let to_string_triplet trip =
  let to_string x y z = Printf.sprintf "-%d-%d-%d-" x y z in
  let (x, y, z) = trip in
  to_string x y z ;;

let to_string_doublet doub =
  let to_string x y = Printf.sprintf "-%d-%d-" x y in
  let (x, y) = doub in
  to_string x y ;;
  

let rec print_triplet_list = function
  [] -> ()
  | front::rest -> print_string (to_string_triplet front); print_string "\n" ; print_triplet_list rest ;;

let dump_obj filename obj = let ch = open_out_bin filename in 
  output_value ch obj ; close_out ch ;;
let slurp_obj filename = let chan = open_in_bin filename in
  input_value chan ;;
  
(* BEGIN PRELIMINARY TESTS COMMENT

Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;

let x_dim = 8 ;;
let y_dim = x_dim ;;
let z_dim = x_dim ;;

let build_toy_data () =
	let toy_data = ref [] in
	for x = 0 to x_dim - 1 do
	  for y = 0 to y_dim - 1 do
		for z = 0 to z_dim - 1 do
		  if x == x_dim/2 && y == y_dim/2
		  then toy_data := (x, y, z) :: !toy_data
		done
	  done
	done ;
	!toy_data

let toy_data = build_toy_data () ;;

Printf.printf "length of toy_data = %d\n%!" (List.length toy_data) ;;



Printf.printf "\n custom printing of toy_data:\n%!" ;;
print_triplet_list toy_data ;;
Printf.printf "\n end of custom printing of toy_data.\n%!" ;;



let toy_dump = "./toy_data.dat" ;;
Printf.printf "\n dumping toy_data to %s:\n%!" toy_dump ;;
dump_obj toy_dump toy_data ;;
Printf.printf "\n dumped toy_data.\n%!" ;;

Printf.printf "\n slurping toy_data_slurped from %s:\n%!" toy_dump ;;
let toy_data_slurped = slurp_obj toy_dump ;;
Printf.printf "\n slurped toy_data_slurped.\n%!" ;;

Printf.printf "\n custom printing of toy_data_slurped:\n%!" ;;
print_triplet_list toy_data_slurped ;;
Printf.printf "\n end of custom printing of toy_data_slurped.\n%!" ;;

let build_toy_data_array toy_data =
	Array.init (List.length toy_data) (List.nth toy_data)

let toy_data_array = build_toy_data_array toy_data ;;

Printf.printf "\n elements of toy_data_array:\n%!" ;;
for i = 0 to (Array.length toy_data_array) - 1 do
	Printf.printf "\n %d\t%s%!" i (to_string_triplet ( toy_data_array.(i) ) )
done ;;
Printf.printf "\n completed list of elements of toy_data_array.\n%!" ;;


(*let req z = match !z with Some x -> x | _ -> invalid_arg "Fail" ;;*)


 let filenamePLS = "./mouse_lung_dropbox_815.pls" ;;
Printf.printf "\n Invoking slurp_obj for %s\n%!" filenamePLS ;;
 let (vertices : (int * int * int) list) = slurp_obj filenamePLS ;;
(*let (vertices : (int * int * int) list) = Mu.slurp_obj (req filenamePLS) ;;*)
Printf.printf "\n Completed slurp_obj for %s\n%!" filenamePLS ;;


(* Fatal Error: out of memory *)
(* let filenamePG = "./mouse_lung_dropbox_815.pg" ;;
Printf.printf "\n Invoking slurp_obj for %s\n%!" filenamePG ;;
 let (lcc : PG.t) = Mu.slurp_obj filenamePG ;; 
Printf.printf "\n\t Completed slurp_obj for %s\n%!" filenamePG ;;*)

let get_max_dim_X_list3D list3D =
	let rec helper a list3D =  match list3D with
		| [] -> a
		| front::rest -> let (x, y, z) = front in
				helper (max a x) rest
	in helper 0 list3D

let get_min_dim_X_list3D list3D =
	let rec helper a list3D =  match list3D with
		| [] -> a
		| front::rest -> let (x, y, z) = front in
				helper (min a x) rest
	in helper (get_max_dim_X_list3D list3D) list3D

let get_max_dim_Y_list3D list3D =
	let rec helper a list3D =  match list3D with
		| [] -> a
		| front::rest -> let (x, y, z) = front in
				helper (max a y) rest
	in helper 0 list3D

let get_min_dim_Y_list3D list3D =
	let rec helper a list3D =  match list3D with
		| [] -> a
		| front::rest -> let (x, y, z) = front in
				helper (min a y) rest
	in helper (get_max_dim_X_list3D list3D) list3D

let get_max_dim_Z_list3D list3D =
	let rec helper a list3D =  match list3D with
		| [] -> a
		| front::rest -> let (x, y, z) = front in
				helper (max a z) rest
	in helper 0 list3D

let get_min_dim_Z_list3D list3D =
	let rec helper a list3D =  match list3D with
		| [] -> a
		| front::rest -> let (x, y, z) = front in
				helper (min a z) rest
	in helper (get_max_dim_X_list3D list3D) list3D
		
let get_max_dim_list3D list3D =
	let x_max = get_max_dim_X_list3D list3D in
	let y_max = get_max_dim_Y_list3D list3D in
	let z_max = get_max_dim_Z_list3D list3D in
		(x_max, y_max, z_max)

let get_min_dim_list3D list3D =
	let x_min = get_min_dim_X_list3D list3D in
	let y_min = get_min_dim_Y_list3D list3D in
	let z_min = get_min_dim_Z_list3D list3D in
		(x_min, y_min, z_min)

let max_dim = get_max_dim_list3D vertices ;;
let min_dim = get_min_dim_list3D vertices ;;

Printf.printf "\n max_dim = %s\n%!" (to_string_triplet max_dim) ;;
Printf.printf "\n min_dim = %s\n%!" (to_string_triplet min_dim) ;;

let linInd position dim_min dim_max =
	let (x, y, z) = position in
	let (x_min, y_min, z_min) = dim_min in
	let (x_max, y_max, z_max) = dim_max in
		(z - z_min)*(x_max - x_min + 1)*(y_max - y_min + 1) + (y - y_min)*(x_max - x_min + 1) + x - x_min ;;

let linInd2position linearIndex dim_min dim_max =
	let (x_min, y_min, z_min) = dim_min in
	let (x_max, y_max, z_max) = dim_max in
	let x_extent = x_max - x_min + 1 in
	let y_extent = y_max - y_min + 1 in
	let xMod = linearIndex mod x_extent in
	let yMod = linearIndex mod (x_extent*y_extent) in
		(xMod + x_min, yMod/x_extent + y_min, (linearIndex/(x_extent*y_extent)) + z_min) ;;

(*
Printf.printf "\n Checking uniqueness and consistency of linInd...\n%!" ;;
let (x_min, y_min, z_min) = min_dim in
let (x_max, y_max, z_max) = max_dim in
let count = ref 0 in
	for z = z_min to z_max do
		for y = y_min to y_max do
			for x = x_min to x_max do
				if (linInd (x, y, z) min_dim max_dim) != !count
				then Printf.printf "\nMismatch at count %d for position (%d, %d, %d), error with linInd giving %d\n%!" !count x y z (linInd (x, y, z) min_dim max_dim) ;
				let (u, v, w) = linInd2position !count min_dim max_dim in
					if (x != u || y != v || z != w)
					then Printf.printf "\nMismatch at count %d for position (%d, %d, %d), error with linInd2position giving %s\n%!" !count x y z (to_string_triplet (u, v, w)) ;
				count := !count + 1
			done
		done
	done ;
Printf.printf "\n Uniqueness and consistency check of linInd complete.\n%!" ;;
*)
Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;



Printf.printf "\n\n Starting BatBitSet tests.\n%!" ;;

let necessary_volume min_dim max_dim =
	let (x_min, y_min, z_min) = min_dim in
	let (x_max, y_max, z_max) = max_dim in
		(z_max - z_min + 1)*(y_max - y_min + 1)*(x_max - x_min + 1)

let to_string_bit bitset index =
	if BatBitSet.mem bitset index then "true" else "false"

(*let testbits = BatBitSet.create (necessary_volume min_dim max_dim) ;;
Printf.printf "initial testbits.(0) is %s\n" (to_string_bit testbits 0) ;;
BatBitSet.set testbits 0 ;;
Printf.printf "set testbits.(0) is %s\n" (to_string_bit testbits 0) ;;
BatBitSet.unset testbits 0 ;;
Printf.printf "unset testbits.(0) is %s\n" (to_string_bit testbits 0) ;;
BatBitSet.set testbits 0 ;;
Printf.printf "set testbits.(0) is %s\n" (to_string_bit testbits 0) ;;
BatBitSet.toggle testbits 0 ;;
Printf.printf "toggle testbits.(0) is %s\n" (to_string_bit testbits 0) ;;
BatBitSet.toggle testbits 0 ;;
Printf.printf "toggle testbits.(0) is %s\n" (to_string_bit testbits 0) ;;
let lastIndex = linInd max_dim min_dim max_dim ;;
Printf.printf "initial testbits.(%d) is %s\n" lastIndex (to_string_bit testbits lastIndex) ;;
BatBitSet.set testbits lastIndex ;;
Printf.printf "initial testbits.(%d) is %s\n" lastIndex (to_string_bit testbits lastIndex) ;;*)

let rec set_from_pls vbits vertices min_dim max_dim = match vertices with
	| [] -> ()
	| front::rest ->
		BatBitSet.set vbits (linInd front min_dim max_dim) ;
		set_from_pls vbits rest min_dim max_dim

let pls2bbs vertices =
	let min_dim = get_min_dim_list3D vertices in
	let max_dim = get_max_dim_list3D vertices in
	let vbits = BatBitSet.create (necessary_volume min_dim max_dim) in
		set_from_pls vbits vertices min_dim max_dim ;
	vbits ;;

Printf.printf "\n Creating vbits...\n%!" ;;
let vbits = pls2bbs vertices ;;
Printf.printf "\n Finished creating vbits.\n%!" ;;

let next_set_bit vbits n =
	let next_bit = BatBitSet.next_set_bit vbits n in
	match next_bit with
		| Some b -> b
		| None -> -2 ;;

let valid_triple position min_dim max_dim =
	let (x, y, z) = position in
	let (x_min, y_min, z_min) = min_dim in
	let (x_max, y_max, z_max) = max_dim in
		( x_min <= x && x <= x_max && y_min <= y && y <= y_max && z_min <= z && z <= z_max ) ;;
		
let get_neighborhood_by_triple vbits position min_dim max_dim =
	let neighbor_list = ref [] in
	let center_index = (linInd position min_dim max_dim) in
	if (BatBitSet.mem vbits center_index) then
		let (x, y, z) = position in
		for dx = -1 to 1 do
			for dy = -1 to 1 do
				for dz = -1 to 1 do
					if(dx != 0 || dy != 0 || dz != 0) then
						let nx = x + dx in
						let ny = y + dy in
						let nz = z + dz in
						if (valid_triple (nx, ny, nz) min_dim max_dim) then
							if (BatBitSet.mem vbits (linInd (x + dx, y + dy, z + dz) min_dim max_dim)) then
								neighbor_list := (x + dx, y + dy, z + dz) :: !neighbor_list
				done
			done
		done ;
		!neighbor_list
	else [] ;;

let next_bit = ref (next_set_bit vbits 0) in
let explore_to_bit = (next_set_bit vbits ((next_set_bit vbits 0) + 1)) in
while !next_bit >= 0 do
	if !next_bit <= explore_to_bit then begin
		Printf.printf "\n position %s is part of the vasculature.  It's neighbor list is:\n" (to_string_triplet (linInd2position !next_bit min_dim max_dim)) ;
		let n_list = (get_neighborhood_by_triple vbits (linInd2position !next_bit min_dim max_dim) min_dim max_dim) in
			print_triplet_list  n_list ;
		Printf.printf "\n%!"
		end ;
	next_bit := next_set_bit vbits (!next_bit + 1)
done

let vbits_dump = "./vbits.dat" ;;
Printf.printf "\n dumping vbits to %s:\n%!" vbits_dump ;;
dump_obj vbits_dump vbits ;;
Printf.printf "\n dumped vbits.\n" ;;

Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;

Printf.printf "\n word size: %d bits\n%!" Sys.word_size ;;

let max_allowed_int = int_of_float (2.**(float_of_int (Sys.word_size - 2)) -. 1.) ;;

Printf.printf "\n max_allowed_int = %d\n%!" max_allowed_int ;;

Printf.printf "\n max_allowed_int + 1 = %d\n%!" (max_allowed_int + 1) ;;

Printf.printf "\n max_allowed_int^2 = %d\n%!" (max_allowed_int*max_allowed_int) ;;

Printf.printf "\n The %s dataset in BatBitSet form takes up almost 1/%d of the allowable single-array space.\n%!" filenamePLS (max_allowed_int/(necessary_volume min_dim max_dim)) ;;

Printf.printf "\n\n Starting Arrays of BatBitSet tests.\n%!" ;;

let test_trip = ref (1, 2, 3) ;;
Printf.printf "\n test_trip = %s \t sorts to %s\n" (to_string_triplet !test_trip) (to_string_triplet (ABBS.sort_triplet !test_trip)) ;;
test_trip := (1, 3, 2) ;;
Printf.printf "\n test_trip = %s \t sorts to %s\n" (to_string_triplet !test_trip) (to_string_triplet (ABBS.sort_triplet !test_trip)) ;;
test_trip := (2, 1, 3) ;;
Printf.printf "\n test_trip = %s \t sorts to %s\n" (to_string_triplet !test_trip) (to_string_triplet (ABBS.sort_triplet !test_trip)) ;;
test_trip := (2, 3, 1) ;;
Printf.printf "\n test_trip = %s \t sorts to %s\n" (to_string_triplet !test_trip) (to_string_triplet (ABBS.sort_triplet !test_trip)) ;;
test_trip := (3, 1, 2) ;;
Printf.printf "\n test_trip = %s \t sorts to %s\n" (to_string_triplet !test_trip) (to_string_triplet (ABBS.sort_triplet !test_trip)) ;;
test_trip := (3, 2, 1) ;;
Printf.printf "\n test_trip = %s \t sorts to %s\n" (to_string_triplet !test_trip) (to_string_triplet (ABBS.sort_triplet !test_trip)) ;;


(*
let num_BatBitSet_arrays_necessary dim_min dim_max =
	let (x_min, y_min, z_min) = dim_min in
	let (x_max, y_max, z_max) = dim_max in
	let vol = (float_of_int (x_max - x_min))*.(float_of_int (y_max - y_min))*.(float_of_int (z_max - z_min)) in
	int_of_float (ceil (vol/.(float_of_int max_allowed_int)) )
*)

(*
let hyp_min_dim = (3, 6, 14) ;;
let hyp_side = ref (int_of_float (ceil (((0.5*.(float_of_int max_allowed_int))**(1./.3.))))) ;;
let hyp_max_dim = ref (!hyp_side, !hyp_side, !hyp_side) ;;
Printf.printf "\nFor hyp_max_dim = %s, one needs %d array(s)\n" (to_string_triplet !hyp_max_dim) (ABBS.num_BatBitSet_arrays_necessary hyp_min_dim !hyp_max_dim);;
hyp_side := (int_of_float (ceil (((1.5*.(float_of_int max_allowed_int))**(1./.3.)))))
let hyp_max_dim = ref (!hyp_side, !hyp_side, !hyp_side) ;;
Printf.printf "\nFor hyp_max_dim = %s, one needs %d array(s)\n" (to_string_triplet !hyp_max_dim) (ABBS.num_BatBitSet_arrays_necessary hyp_min_dim !hyp_max_dim);;
hyp_side := (int_of_float (ceil (((104.7*.(float_of_int max_allowed_int))**(1./.3.)))))
let hyp_max_dim = ref (!hyp_side, !hyp_side, !hyp_side) ;;
Printf.printf "\nFor hyp_max_dim = %s, one needs %d array(s)\n" (to_string_triplet !hyp_max_dim) (ABBS.num_BatBitSet_arrays_necessary hyp_min_dim !hyp_max_dim);;
*)

(*let max_allowed_size_in_GB_small = 0.01 ;;
let test_abbs_small = ABBS.create max_allowed_size_in_GB_small min_dim max_dim ;;*)


let max_allowed_size_in_GB = 1. ;;
let test_abbs = ABBS.create max_allowed_size_in_GB min_dim max_dim ;;
Printf.printf "\n test_abbs summary:\n%!" ;;
ABBS.print_summary test_abbs ;;

Printf.printf "\n Creating test_abbs_big...\n%!" ;;
let test_abbs_big = ABBS.create max_allowed_size_in_GB (1, 2, 3) (1234, 1235, 1236) ;;
Printf.printf "\n test_abbs_big created.\n%!" ;;
Printf.printf "\n test_abbs_big summary:\n%!" ;;
ABBS.print_summary test_abbs_big ;;

let test_position_big = ref (0, 0, 0) ;;
let array_position_big = ref (ABBS.array_and_index !test_position_big test_abbs_big) ;;
let check_test_position_big = ref (ABBS.position_raw !array_position_big  test_abbs_big) ;;
Printf.printf "\n position %s in test_abbs_big goes to %s and checks to %s\n" (to_string_triplet !test_position_big) (to_string_doublet !array_position_big) (to_string_triplet !check_test_position_big) ;;
test_position_big := (1, 2, 3) ;;
array_position_big := (ABBS.array_and_index !test_position_big test_abbs_big) ;;
check_test_position_big := (ABBS.position_raw !array_position_big test_abbs_big) ;;
Printf.printf "\n position %s in test_abbs_big goes to %s and checks to %s\n" (to_string_triplet !test_position_big) (to_string_doublet !array_position_big) (to_string_triplet !check_test_position_big) ;;
test_position_big := (1, 2, 4) ;;
array_position_big := (ABBS.array_and_index !test_position_big test_abbs_big) ;;
check_test_position_big := (ABBS.position_raw !array_position_big test_abbs_big) ;;
Printf.printf "\n position %s in test_abbs_big goes to %s and checks to %s\n" (to_string_triplet !test_position_big) (to_string_doublet !array_position_big) (to_string_triplet !check_test_position_big) ;;
test_position_big := (1, 3, 3) ;;
array_position_big := (ABBS.array_and_index !test_position_big test_abbs_big) ;;
check_test_position_big := (ABBS.position_raw !array_position_big test_abbs_big) ;;
Printf.printf "\n position %s in test_abbs_big goes to %s and checks to %s\n" (to_string_triplet !test_position_big) (to_string_doublet !array_position_big) (to_string_triplet !check_test_position_big) ;;
test_position_big := (2, 2, 3) ;;
array_position_big := (ABBS.array_and_index !test_position_big test_abbs_big) ;;
check_test_position_big := (ABBS.position_raw !array_position_big test_abbs_big) ;;
Printf.printf "\n position %s in test_abbs_big goes to %s and checks to %s\n" (to_string_triplet !test_position_big) (to_string_doublet !array_position_big) (to_string_triplet !check_test_position_big) ;;
test_position_big := (2, 4, 6) ;;
array_position_big := (ABBS.array_and_index !test_position_big test_abbs_big) ;;
check_test_position_big := (ABBS.position_raw !array_position_big test_abbs_big) ;;
Printf.printf "\n position %s in test_abbs_big goes to %s and checks to %s\n" (to_string_triplet !test_position_big) (to_string_doublet !array_position_big) (to_string_triplet !check_test_position_big) ;;
test_position_big := (50, 75, 3) ;;
array_position_big := (ABBS.array_and_index !test_position_big test_abbs_big) ;;
check_test_position_big := (ABBS.position_raw !array_position_big test_abbs_big) ;;
Printf.printf "\n position %s in test_abbs_big goes to %s and checks to %s\n" (to_string_triplet !test_position_big) (to_string_doublet !array_position_big) (to_string_triplet !check_test_position_big) ;;


Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;


Printf.printf "\n\n Starting p32abbs tests.\n%!" ;;

let check_abbs = ABBS.create 4.0 (0, 0, 0) (1, 1, 1) ;;
ABBS.print_summary check_abbs ;;
ABBS.print_all_elements check_abbs ;;
ABBS.set check_abbs (0, 0, 0) ;;
ABBS.print_all_elements check_abbs ;;
ABBS.set check_abbs (1, 0, 0) ;;
ABBS.print_all_elements check_abbs ;;
let (slice_start, slice_finish, min_intensity, max_intensity, slice_dim) = (0, 0, 0.0, 1.0, "y") in
let test_slice = ABBS.ascii_slice slice_start slice_finish check_abbs 2 2 min_intensity max_intensity slice_dim in
Printf.printf "\n test_slice from check_abbs (yes half) %d-%d:\n%s\n" slice_start slice_finish test_slice ;;
let (slice_start, slice_finish, min_intensity, max_intensity, slice_dim) = (1, 1, 0.0, 1.0, "y") in
let test_slice = ABBS.ascii_slice slice_start slice_finish check_abbs 2 2 min_intensity max_intensity slice_dim in
Printf.printf "\n test_slice from check_abbs (no half) %d-%d:\n%s\n" slice_start slice_finish test_slice ;;
let (slice_start, slice_finish, min_intensity, max_intensity, slice_dim) = (0, 0, 0.0, 1.0, "z") in
let test_slice = ABBS.ascii_slice slice_start slice_finish check_abbs 2 2 min_intensity max_intensity slice_dim in
Printf.printf "\n test_slice from check_abbs (both half z = 0) %d-%d:\n%s\n" slice_start slice_finish test_slice ;;
let (slice_start, slice_finish, min_intensity, max_intensity, slice_dim) = (1, 1, 0.0, 1.0, "z") in
let test_slice = ABBS.ascii_slice slice_start slice_finish check_abbs 2 2 min_intensity max_intensity slice_dim in
Printf.printf "\n test_slice from check_abbs (both half z = 1) %d-%d:\n%s\n" slice_start slice_finish test_slice ;;
ABBS.set check_abbs (0, 1, 0) ;;
ABBS.print_all_elements check_abbs ;;
ABBS.set check_abbs (1, 1, 0) ;;
ABBS.print_all_elements check_abbs ;;
let (slice_start, slice_finish, min_intensity, max_intensity, slice_dim) = (0, 0, 0.0, 1.0, "y") in
let test_slice = ABBS.ascii_slice slice_start slice_finish check_abbs 2 2 min_intensity max_intensity slice_dim in
Printf.printf "\n test_slice from check_abbs (yes all first) %d-%d:\n%s\n" slice_start slice_finish test_slice ;;
let (slice_start, slice_finish, min_intensity, max_intensity, slice_dim) = (1, 1, 0.0, 1.0, "y") in
let test_slice = ABBS.ascii_slice slice_start slice_finish check_abbs 2 2 min_intensity max_intensity slice_dim in
Printf.printf "\n test_slice from check_abbs (yes all second) %d-%d:\n%s\n" slice_start slice_finish test_slice ;;
let (slice_start, slice_finish, min_intensity, max_intensity, slice_dim) = (0, 0, 0.0, 1.0, "z") in
let test_slice = ABBS.ascii_slice slice_start slice_finish check_abbs 2 2 min_intensity max_intensity slice_dim in
Printf.printf "\n test_slice from check_abbs (both all z=0) %d-%d:\n%s\n" slice_start slice_finish test_slice ;;
let (slice_start, slice_finish, min_intensity, max_intensity, slice_dim) = (1, 1, 0.0, 1.0, "z") in
let test_slice = ABBS.ascii_slice slice_start slice_finish check_abbs 2 2 min_intensity max_intensity slice_dim in
Printf.printf "\n test_slice from check_abbs (both all z=1) %d-%d:\n%s\n" slice_start slice_finish test_slice ;;
let (slice_start, slice_finish, min_intensity, max_intensity, slice_dim) = (0, 1, 0.0, 1.0, "z") in
let test_slice = ABBS.ascii_slice slice_start slice_finish check_abbs 2 2 min_intensity max_intensity slice_dim in
Printf.printf "\n test_slice from check_abbs (both all z=0,1) %d-%d:\n%s\n" slice_start slice_finish test_slice ;;

(* PORTION START
let path = "./mouse_lung_dropbox_pnm" ;;
let start_img = 90 ;;
let end_img = 103 ;;
let thresh = 0.8 ;;
Printf.printf "\n Loading images %d to %d in %s for a threshold of %f\n (two progress bars)\n%!" start_img end_img path thresh ;;
let fromP3 = P3.load start_img end_img path thresh ".pnm" max_allowed_size_in_GB ;;
Printf.printf "\n Load complete.\n%!" ;;

Printf.printf "\n fromP3 summary:\n%!" ;;
ABBS.print_summary fromP3 ;;

Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;

let only103 = P3.load 103 103 path thresh ".pnm" max_allowed_size_in_GB ;;
Printf.printf "\n only103 summary:\n%!" ;;
ABBS.print_summary only103 ;;

let (test_slice_width, test_slice_height) = (20, 16) ;;

let (slice_start, slice_finish, min_intensity, max_intensity, slice_dim) = (0, 13, 0.0, 1.0, "z") in
let test_slice = ABBS.ascii_slice slice_start slice_finish fromP3 test_slice_width test_slice_height min_intensity max_intensity slice_dim in
Printf.printf "\n test_slice(s) %d-%d:\n%s\n" slice_start slice_finish test_slice ;;

let (slice_start, slice_finish, min_intensity, max_intensity, slice_dim) = (13, 13, 0.0, 1.0, "z") in
let test_slice = ABBS.ascii_slice slice_start slice_finish fromP3 test_slice_width test_slice_height min_intensity max_intensity slice_dim in
Printf.printf "\n test_slice(s) %d-%d:\n%s\n" slice_start slice_finish test_slice ;;

let (slice_start, slice_finish, min_intensity, max_intensity, slice_dim) = (0, 0, 0.0, 1.0, "z") in
let test_slice = ABBS.ascii_slice slice_start slice_finish only103 test_slice_width test_slice_height min_intensity max_intensity slice_dim in
Printf.printf "\n test_slice from only103 %d-%d:\n%s\n" slice_start slice_finish test_slice ;;

PORTION END *)


(*Printf.printf "\n all z slices are identical at the following pairs:\n%!" ;;
let (x_min, y_min, z_min) = ABBS.get_min_dim fromP3 in
let (x_max, y_max, z_max) = ABBS.get_max_dim fromP3 in
for x = x_min to x_max do
	for y = y_min to y_max do
		let z_are_identical = ref true in
		for z = z_min to z_max do
			z_are_identical := !z_are_identical && (ABBS.is_set fromP3 (x, y, z_min)) = (ABBS.is_set fromP3 (x, y, z));
		done ;
		if !z_are_identical then Printf.printf " (%d,%d) %!" x y
	done
done ;;*)

(*
Printf.printf "\n Priority Queue tests\n%!"

let rec update_frontier previous_frontier new_frontier = match new_frontier with
	[] -> ()
	| front::rest ->
		
		update_frontier previous_frontier rest
in*)

END PRELIMINARY TESTS COMMENT *)

(* SKELETON COMMENT *)

Printf.printf "\n\n Starting skeletonize tests.\n%!" ;;

Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;

Printf.printf "\n\n Loading small_network_test... (two progress bars)\n%!" ;;
let small_network_thresh = 0.5 ;;
let small_network_test = P3.load 0 7 "./small_network_test" small_network_thresh ".pnm" 1.0 ;;
Printf.printf "\n\n Loaded small_network_test.\n%!" ;;

(*for i = 0 to 6 do
	Printf.printf "\n slice %d:\n%s%!" i (ABBS.ascii_slice i i small_network_test 25 25 0. 1. "z")
done ;;*)
Printf.printf "\n small_network_test summary:\n%!" ;;
ABBS.print_summary small_network_test ;;
Printf.printf "\n small_network_test:\n%s%!" (ABBS.ascii_slice 0 7 small_network_test 25 25 0. 1. "z") ;;

(*let small_network_copy = ABBS.create_exact_copy small_network_test in
Printf.printf "\n small_network_copy summary:\n%!" ;
ABBS.print_summary small_network_copy ;
Printf.printf "\n small_network_copy:\n%s%!" (ABBS.ascii_slice 0 7 small_network_copy 25 25 0. 1. "z") ;;*)

(*let small_network_tight = ABBS.create_tight_copy small_network_test in
Printf.printf "\n small_network_tight summary:\n%!" ;
ABBS.print_summary small_network_tight ;
Printf.printf "\n small_network_tight:\n%s%!" (ABBS.ascii_slice 0 7 small_network_tight 25 25 0. 1. "z") ;;*)


(*Printf.printf "\n Some points that are in the vasculature of small_network_test:\n%!" ;;
let small_test_set_position = ref (40, 40, 6) in
let at_end = ref false in
while (not !at_end) do
	small_test_set_position := ABBS.next_set_position_after small_network_test !small_test_set_position ;
	let (x, y, z) = !small_test_set_position in
		Printf.printf " (%d,%d,%d) " x y z ;
		at_end := x < 0
done ;;
Printf.printf "\n Points listed.\n%!" ;;*)

Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;

let small_network_lcc = S.largest_connected_component small_network_test ;;
Printf.printf "\n small_network_lcc summary:\n%!" ;
ABBS.print_summary small_network_lcc ;;
Printf.printf "\n small_network_lcc:\n%s%!" (ABBS.ascii_slice 0 7 small_network_lcc  25 25 0. 1. "z") ;;
let small_network_tips_unpruned = S.tips_local_a small_network_lcc true;;
Printf.printf "\n small_network_tips_unpruned has %d elements\n%!" (List.length small_network_tips_unpruned) ;;
Printf.printf "\n small_network_test with unpruned tips:\n%s%!" (ABBS.ascii_slice_with_tips 0 7 small_network_lcc small_network_tips_unpruned 25 25 0. 1. "z") ;;
let small_network_tips = S.prune_tips small_network_tips_unpruned small_network_lcc ;;
Printf.printf "\n small_network_test with tips:\n%s%!" (ABBS.ascii_slice_with_tips 0 7 small_network_lcc small_network_tips 25 25 0. 1. "z") ;;
let small_network_extra_tips = List.concat [[(42, 45, 5); (39, 43, 4); (35, 38, 2);]; small_network_tips_unpruned] ;;
Printf.printf "\n small_network_test with extra tips, %d total:\n%s%!" (List.length small_network_extra_tips) (ABBS.ascii_slice_with_tips 0 7 small_network_lcc small_network_extra_tips 25 25 0. 1. "z") ;;
let small_network_extra_tips_pruned = S.prune_tips small_network_extra_tips small_network_lcc ;;
Printf.printf "\n small_network_test with extra tips pruned, %d total:\n%s%!" (List.length small_network_extra_tips_pruned) (ABBS.ascii_slice_with_tips 0 7 small_network_lcc small_network_extra_tips_pruned 25 25 0. 1. "z") ;;

(* skeleton WITH tips *)

Printf.printf "\n Skeletonizing small_network_lcc at %f \n%!" (Sys.time()) ;;
let small_network_skeleton = S.skeleton small_network_lcc small_network_tips true ;;
Printf.printf "\n Produced small_network_skeleton at %f \n%!" (Sys.time()) ;;
Printf.printf "\n small_network_lcc with small_network_skeleton summary:\n%!" ;
ABBS.print_summary small_network_skeleton ;;
Printf.printf "\n small_network_lcc with small_network_skeleton:\n%s%!" (ABBS.ascii_slice_with_tips_and_skeleton 0 7 small_network_lcc small_network_tips small_network_skeleton 50 48 0. 1. "z") ;;

Printf.printf "\n small_network_skeleton:\n%s%!" (ABBS.ascii_slice_with_tips_and_skeleton 0 7 small_network_skeleton small_network_tips small_network_skeleton 50 48 0. 1. "z") ;;

let separation (p1x, p1y, p1z) (p2x, p2y, p2z) =
	let sq x = x*x in
	sqrt (float_of_int (sq (p1x - p2x) + sq (p1y - p2y) + sq (p1z - p2z)))  

let rec print_segments_short_summaries (segs:S.seg list) = match segs with
	[] -> Printf.printf "\n%!"
	| front::rest -> Printf.printf "\n\t%s%!" front#short_summary ; print_segments_short_summaries rest ;;

let rec sum_segments_vol vol segments_remaining = match segments_remaining with
	[] -> vol
	| front::rest -> sum_segments_vol (vol +. front#get_vol) rest ;;

let rec compare_segment_lengths_with_tips segs = match segs with
	[] -> Printf.printf "\n%!"
	| front::rest -> Printf.printf "\t tipSep=%f numVerts=%d\t measLen=%f \n%!" (separation front#get_tip1 front#get_tip2) (List.length front#get_backbone) front#get_len ;
		compare_segment_lengths_with_tips rest ;;

let to_string_int_list_one_line ell =
	if (List.length ell < 1) then Printf.sprintf "()" else begin
		let rec to_str_int_list_one_line str remaining =  match remaining with
			[] -> Printf.sprintf "%s)" str
			| front::rest -> to_str_int_list_one_line (Printf.sprintf "%s, %d" str front) rest
		in
		to_str_int_list_one_line (Printf.sprintf "(%d" (List.hd ell)) (List.tl ell)
	end

let rec print_tree_finite queued_indices parents children sega visited_indices =  match queued_indices with
	[] -> Printf.printf "\n%!"
	| front::rest ->
		if BatBitSet.mem visited_indices front then begin
			Printf.printf "\t segment %d has already been described!\n" front ;
			print_tree_finite rest parents children sega visited_indices
		end else begin
			BatBitSet.set visited_indices front ;
			Printf.printf "\t segment %d \n\t\t {%s}\n\t\t is a child of %d\n\t\t and has %d children:%s\n" front sega.(front)#short_summary parents.(front) (List.length children.(front)) (to_string_int_list_one_line children.(front));
			print_tree_finite (List.concat [rest; children.(front)]) parents children sega visited_indices
		end

let print_tree queued_indices parents children sega =
	print_tree_finite queued_indices parents children sega (BatBitSet.create (Array.length parents)) ;;

let next_skel = ref (ABBS.next_set_position_after small_network_skeleton (-1, -1, -1)) in
let (x_init, _, _) = !next_skel in
let covered_skel = ref (x_init < 0) in
let found_seed = ref false in
let seg_seed = ref (-1, -1, -1) in
while not (!covered_skel || !found_seed) do
	if ((ABBS.degree small_network_skeleton !next_skel) = 2) then begin
		seg_seed := !next_skel ;
		found_seed := true
	end ;
	next_skel := ABBS.next_set_position_after small_network_skeleton !next_skel ;
	let (x, _, _) = !next_skel in
	covered_skel := (!covered_skel || x < 0)
done ;
let vertebrae = S.get_vertebrae !seg_seed small_network_skeleton in
Printf.printf "\nFound segment seed at %s with vertebrae:\n%!" (to_string_triplet !seg_seed) ;
print_triplet_list vertebrae ;
let adjacent_seg_seeds = S.get_adjacent_segments_seeds vertebrae small_network_skeleton in
Printf.printf "\nFound %d adjacent segment(s):\n%!" (List.length adjacent_seg_seeds ) ;
print_triplet_list adjacent_seg_seeds ;
let adjacent_vertebrae = S.get_adjacent_segments_vertebrae vertebrae small_network_skeleton in
Printf.printf "\nFound %d adjacent_vertebra(e):\n%!" (List.length adjacent_vertebrae) ;
print_triplet_list adjacent_vertebrae ;
let seg_test = S.analyze_segment !seg_seed small_network_lcc small_network_skeleton (ABBS.create_exact_copy small_network_skeleton) (ABBS.create_exact_copy small_network_skeleton)in 
Printf.printf "\n analyzed segment's short summary: %s\n%!" seg_test#short_summary ;
let segs_test = S.analyze_skeleton small_network_lcc small_network_skeleton in
Printf.printf "\n Found %d segments:\n%!" (List.length segs_test);
print_segments_short_summaries segs_test ;
Printf.printf "\n small_network_lcc.count = %d\n sum of volumes = %f%!" (ABBS.count small_network_lcc) (sum_segments_vol 0. segs_test);
let bad_tips = S.identify_bad_tips segs_test small_network_tips in
Printf.printf "\n Found %d bad tip(s):\n%!" (List.length bad_tips) ;
print_triplet_list bad_tips ;
let (small_network_skel_final, small_network_good_tips) = S.skeleton_final small_network_skeleton segs_test small_network_tips in
Printf.printf "\n small_network_skel_final:\n%s%!" (ABBS.ascii_slice_with_tips_and_skeleton 0 7 small_network_lcc small_network_good_tips small_network_skel_final 50 48 0. 1. "z") ;
let segs_test_final = S.analyze_skeleton small_network_lcc small_network_skel_final in
let sega_test = S.create_seg_array segs_test_final in
let root_index = S.choose_root sega_test small_network_good_tips in
Printf.printf "\n Root segment is index %d: %s\n%!" root_index sega_test.(root_index)#short_summary ;
let (root_dup, sega_dup, parents_test, children_test) = S.parents_and_children segs_test_final small_network_skel_final small_network_good_tips in
Printf.printf "\n The tree is:\n%!" ;
print_tree [root_dup] parents_test children_test sega_dup ;
 ;;



 
 
Printf.printf "\n\n Performing full run for small network from reading in to final segment analysis.\n%!" ;


let temp_start_time = Sys.time() in
Printf.printf "\n Reading image files \t Processor time: %f s\n%!" (temp_start_time) ;
let sn = P3.load 0 7 "./small_network_test" 0.5 ".pnm" 1.0 in
Printf.printf "\n Finding lcc \t Processor time: %f s\n%!" (Sys.time()) ;
let sn_lcc = S.largest_connected_component sn in
Printf.printf "\n Finding tips \t Processor time: %f s\n%!" (Sys.time()) ;
let sn_tips_unpruned = S.tips_local_a sn_lcc true in
Printf.printf "\n Pruning tips \t Processor time: %f s\n%!" (Sys.time()) ;
let sn_tips = S.prune_tips sn_tips_unpruned sn_lcc in
Printf.printf "\n Finding rough skeleton \t Processor time: %f s\n%!" (Sys.time()) ;
let sn_skeleton = S.skeleton sn_lcc sn_tips true in
Printf.printf "\n Finding rough segments \t Processor time: %f s\n%!" (Sys.time()) ;
let s_test = S.analyze_skeleton sn_lcc sn_skeleton in
Printf.printf "\n Finding final skelton and tips \t Processor time: %f s\n%!" (Sys.time()) ;
let (sn_skel_final, sn_good_tips) = S.skeleton_final sn_skeleton s_test sn_tips in
Printf.printf "\n Finding final segments \t Processor time: %f s\n%!" (Sys.time()) ;
let s_final_test = S.analyze_skeleton sn_lcc sn_skel_final in
Printf.printf "\n Organizing tree \t Processor time: %f s\n%!" (Sys.time()) ;
let (sa_root, sa, sn_parents, sn_children) = S.parents_and_children s_final_test sn_skel_final sn_good_tips in
let temp_finish_time = Sys.time() in
Printf.printf "\n Analysis complete \t Processor time: %f s\n%!" (Sys.time()) ;
Printf.printf "\n TOTAL TIME to perform one full run on the small network: %f s\n%!" (temp_finish_time -. temp_start_time) ;
Printf.printf "\n Found %d segments:\n%!" (List.length s_final_test) ;
print_segments_short_summaries s_final_test ;
Printf.printf "\n sn_lcc.count = %d\n sum of volumes = %f%!" (ABBS.count sn_lcc) (sum_segments_vol 0. s_final_test) ;
Printf.printf "\n tip separation and measured length:\n%!" ; compare_segment_lengths_with_tips s_final_test ;
Printf.printf "\n The tree is:\n%!" ;
print_tree [sa_root] sn_parents sn_children sa ;
Printf.printf "\n Angicart style output is:\n%s\n%!" (S.print_tree sa_root sn_parents sn_children sa)
 ;;
 
let full_analysis_timed img_index_start img_index_end img_dir thresh =
	Printf.printf "\n Full anaysis (timed)\n%!" ;
	let temp_start_time = Sys.time() in
	Printf.printf "\n Reading image files from %s at a threshold of %f \t Processor time: %f s\n%!" img_dir thresh (temp_start_time) ;
	let raw_abbs = P3.load img_index_start img_index_end img_dir thresh ".pnm" 1.0 in
	Printf.printf "\n Finding lcc \t Processor time: %f s\n%!" (Sys.time()) ;
	let abbs_lcc = S.largest_connected_component raw_abbs in
	Printf.printf "\t lcc has %d points.\n%!" (ABBS.count abbs_lcc) ;
	let (x_e, y_e, _) = ABBS.get_ext_dim abbs_lcc in
	Printf.printf "\n abbs_lcc:\n%s%!" (ABBS.ascii_slice 0 (img_index_end - img_index_start) abbs_lcc  (x_e/4) (y_e/4) 0. 0.02 "z") ;
	Printf.printf "\n Finding tips \t Processor time: %f s\n%!" (Sys.time()) ;
	let list_tips_unpruned = S.tips_local_a abbs_lcc true in
	Printf.printf "\n Pruning tips \t Processor time: %f s\n%!" (Sys.time()) ;
	let list_tips = S.prune_tips list_tips_unpruned abbs_lcc in
	Printf.printf "\t Found %d tips after pruning.\n%!" (List.length list_tips) ;
	Printf.printf "\n Finding rough skeleton \t Processor time: %f s\n%!" (Sys.time()) ;
	let abbs_rough_skel = S.skeleton abbs_lcc list_tips true in
	Printf.printf "\t rough skeleton has %d points.\n%!" (ABBS.count abbs_rough_skel) ;
	Printf.printf "\n Finding rough segments \t Processor time: %f s\n%!" (Sys.time()) ;
	let rough_segs = S.analyze_skeleton abbs_lcc abbs_rough_skel in
	Printf.printf "\t Found %d rough segs.\n%!" (List.length rough_segs) ;
	Printf.printf "\n Finding final skeleton and tips \t Processor time: %f s\n%!" (Sys.time()) ;
	let (abbs_final_skel, list_good_tips) = S.skeleton_final abbs_rough_skel rough_segs list_tips in
	Printf.printf "\t Final skeleton has %d points and %d tips\n%!" (ABBS.count abbs_final_skel) (List.length list_good_tips) ;
	Printf.printf "\n Finding final segments \t Processor time: %f s\n%!" (Sys.time()) ;
	let final_segs = S.analyze_skeleton abbs_lcc abbs_final_skel in
	Printf.printf "\n Organizing tree \t Processor time: %f s\n%!" (Sys.time()) ;
	let (root_index, sega, parents, children) = S.parents_and_children final_segs abbs_final_skel list_good_tips in
	let temp_finish_time = Sys.time() in
	let (x_ext, y_ext, _) = ABBS.get_ext_dim abbs_lcc in
	Printf.printf "\n abbs_lcc with abbs_final_skel and tips:\n\t z projection:\n%s\n%!" (ABBS.ascii_slice_with_tips_and_skeleton 0 (img_index_end - img_index_start) abbs_lcc list_good_tips abbs_final_skel (x_ext/4) (y_ext/4) 0. 0.01 "z") ;
	Printf.printf "\n Analysis complete \t Processor time: %f s\n%!" (Sys.time()) ;
	Printf.printf "\n TOTAL TIME to perform one full run: %f s\n%!" (temp_finish_time -. temp_start_time) ;
	Printf.printf "\n Found %d segments:\n%!" (List.length final_segs) ;
	(*print_segments_short_summaries final_segs ;
	Printf.printf "\n sn_lcc.count = %d\n sum of volumes = %f%!" (ABBS.count abbs_lcc) (sum_segments_vol 0. final_segs) ;
	Printf.printf "\n tip separation and measured length:\n%!" ; compare_segment_lengths_with_tips final_segs ;
	Printf.printf "\n The tree is:\n%!" ;
	print_tree [root_index] parents children sega ;*)
	Printf.printf "\n Angicart style output is:\n%s\n%!" (S.print_tree root_index parents children sega);
	(root_index, parents, children, sega, abbs_lcc, abbs_final_skel, list_good_tips) ;;

(*let (snFAT_root_index, snFAT_parents, snFAT_children, snFAT_sega, snFAT_abbs_lcc, snFAT_abbs_final_skel, snFAT_list_good_tips) = full_analysis_timed 0 7 "./small_network_test" 0.9 ;;*)

(* Used for real data
let (dcs_root_index, dcs_parents, dcs_children, dcs_sega, dcs_lcc, dcs_skel, dcs_tips) = full_analysis_timed 56 127 "./dicom_small_3b(15)_p3" 0.60 ;;
*)


(*let (x_ext, y_ext, _) = ABBS.get_ext_dim dcs_lcc in
Printf.printf "\n dcs_lcc with dcs_skel and tips:\n\t z projection:\n%s\n%!" (ABBS.ascii_slice_with_tips_and_skeleton 128 129 dcs_lcc dcs_tips dcs_skel (x_ext/1) (y_ext/1) 0. 0.2 "z") ;;
Printf.printf "\n min_dim = %s    max_dim = %s\n%!" (to_string_triplet (ABBS.get_min_dim dcs_lcc)) (to_string_triplet (ABBS.get_max_dim dcs_lcc)) ;;*)


(* !!!! need to think . . .
(*type vertex = int*)
type vertex = int * int * int
(*type weight = float*)
(*type neighbor = vertex * weight*)
(*module VertexSet = Set.Make(struct type t = weight * vertex let compare = compare end)*)
module VertexSet = Set.Make(struct type t = vertex let compare = compare end)
 
(*let dijkstra (src:vertex) (adj_list:neighbor list array) : weight array * vertex array =*)
let dijkstra (src:vertex) : weight array =
  let n = Array.length adj_list in
  let min_distance = Array.make n infinity in
  min_distance.(src) <- 0.;
  (*let previous = Array.make n (-1) in*)
  let rec aux vertex_queue =
    if not (VertexSet.is_empty vertex_queue) then
      let dist, u = VertexSet.min_elt vertex_queue in
      let vertex_queue' = VertexSet.remove (dist, u) vertex_queue in
      let edges = adj_list.(u) in
      let f vertex_queue (v, weight) =
        let dist_thru_u = dist +. weight in
        if dist_thru_u >= min_distance.(v) then
          vertex_queue
        else begin
          let vertex_queue' = VertexSet.remove (min_distance.(v), v) vertex_queue in
          min_distance.(v) <- dist_thru_u;
          previous.(v) <- u;
          VertexSet.add (min_distance.(v), v) vertex_queue'
        end
      in
      aux (List.fold_left f vertex_queue' edges)
  in
  aux (VertexSet.singleton (min_distance.(src), src));
  min_distance(*, previous*)
*)

(* SKELETON COMMENT *)

(*

(* skeleton withOUT tips *)

Printf.printf "\n Skeletonizing small_network_lcc without tips at %f \n%!" (Sys.time()) ;;
let small_network_skeleton_wo_tips = S.skeleton_without_tips small_network_lcc true ;;
Printf.printf "\n Produced small_network_skeleton_wo_tips at %f \n%!" (Sys.time()) ;;
Printf.printf "\n small_network_skeleton_wo_tips summary:\n%!" ;
ABBS.print_summary small_network_skeleton_wo_tips ;;
Printf.printf "\n small_network_lcc with small_network_skeleton_wo_tips:\n%s%!" (ABBS.ascii_slice_with_tips_and_skeleton 0 7 small_network_lcc [] small_network_skeleton_wo_tips 50 48 0. 1.0 "z") ;;

Printf.printf "\n small_network_skeleton:\n%s%!" (ABBS.ascii_slice_with_tips_and_skeleton 0 7 small_network_skeleton_wo_tips [] small_network_skeleton_wo_tips 50 48 0. 1.0 "z") ;;


*)

(* START mouse_lung_drobox comment


let (mlt_width, mlt_height, mlt_intens_min, mlt_intens_max, mouse_lung_thresh) = (60, 60, 0., 0.2, 0.9) ;;



(* Loading from raw images takes about 3 minutes on old laptop; 1.3 minutes on good ol' desktop {threshold = 0.9}


Printf.printf "\n\n Loading mouse_lung_dropbox... (two progress bars)\n%!" ;;
Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;
let mouse_lung_dropbox = P3.load 1 337 "./mouse_lung_dropbox_pnm" mouse_lung_thresh ".pnm" 1.0 ;;
Printf.printf "\n\n Loaded mouse_lung_dropbox.\n%!" ;;
Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;
Printf.printf "\n\n Saving mouse_lung_dropbox.\n%!" ;;
dump_obj "./mouse_lung_t900.abbs" mouse_lung_dropbox ;;
Printf.printf "\n\n Saved mouse_lung_dropbox.\n%!" ;;
Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;

*)

(* Takes about 4 seconds to draw all layers on good ol' desktop {threshold = 0.9} *)

Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;
let mouse_lung_dropbox = slurp_obj "./mouse_lung_t900.abbs" ;;
Printf.printf "\n mouse_lung_dropbox summary:\n" ; ABBS.print_summary mouse_lung_dropbox ;;
Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;
Printf.printf "\n mouse_lung_dropbox:\n%s%!" (ABBS.ascii_slice 0 366 mouse_lung_dropbox mlt_width mlt_height mlt_intens_min mlt_intens_max "z") ;;
Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;



(*let total_set_bits = ABBS.count mouse_lung_dropbox in
let running_total = ref 0 in
let start_position = ref (ABBS.next_set_position_after mouse_lung_dropbox (-1, -1, -1))  in
let lcc_start_position = ref !start_position in
let lcc_size = ref 0 in
let mouse_lung_copy = ABBS.create_exact_copy mouse_lung_dropbox in
let covered_everything = ref false in
while not !covered_everything do
	let (cc_size, next_start_position) = S.extract_connected_component_size !start_position mouse_lung_copy S.list_twenty_six_neighbors in
	running_total := !running_total + cc_size ;
	if !lcc_size < cc_size then
		lcc_size := cc_size; lcc_start_position := !start_position ;
	if cc_size > 100 then
		Printf.printf "\n Position %s's cc has size %d; %d remain unaccounted for \t\t time: %f\n%!" (to_string_triplet !start_position) cc_size (total_set_bits - !running_total) (Sys.time())
	else
		Printf.printf "(%d)%!" cc_size ;
	start_position := next_start_position ;
	let (x, _, _) = next_start_position in
	covered_everything := x < 0 || !lcc_size > total_set_bits - !running_total
done ;
Printf.printf "\n found lcc with size %d, which includes the point %s\n%!" !lcc_size (to_string_triplet !lcc_start_position) ;;*)



(* Finding the largest connected component takes about six minutes on old laptop; 3.2 minutes on good ol' desktop {threshold = 0.9}

Printf.printf "\n Finding largest connected component of mouse_lung_dropbox...\n%!" ;;
Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;
let mouse_lung_lcc = S.largest_connected_component mouse_lung_dropbox ;;
Printf.printf "\n Largest component found.\n%!" ;;
Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;
Printf.printf "\n\n Saving mouse_lung_lcc.\n%!" ;;
dump_obj "./mouse_lung_t900_lcc.abbs" mouse_lung_lcc ;;
Printf.printf "\n\n Saved mouse_lung_lcc.\n%!" ;;
Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;

*)


let mouse_lung_lcc = slurp_obj "./mouse_lung_t900_lcc.abbs" ;;
Printf.printf "\n lcc summary:\n" ; ABBS.print_summary mouse_lung_lcc ;;
Printf.printf "\n mouse_lung_lcc:\n%s%!" (ABBS.ascii_slice 0 366 mouse_lung_lcc mlt_width mlt_height mlt_intens_min mlt_intens_max "z") ;;



(* Finding (pruned) tips takes about 4 minutes on good ol' desktop {threshold = 0.9} *)
(* Finding rough skeleton takes about takes too long, even on good ol' desktop {threshold = 0.9} *)
(* Finding rough segments takes about takes about X minutes on good ol' desktop {threshold = 0.9} *)
(* Finding final skeleton takes about takes about X minutes on good ol' desktop {threshold = 0.9} *)
(* Finding final segments takes about takes about X minutes on good ol' desktop {threshold = 0.9} *)
(* Organizing the tree takes about takes about X minutes on good ol' desktop {threshold = 0.9} *)

Printf.printf "\n Finding tips...\n%!" ;;
Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;
(*let mouse_lung_tips_unpruned = S.tips_local_a mouse_lung_lcc true ;;
Printf.printf "\n Found %d tips (unpruned)\n%!" (List.length mouse_lung_tips_unpruned) ;;
Printf.printf "\n Pruning tips \t Processor time: %f s\n%!" (Sys.time()) ;;*)
let mouse_lung_tips = slurp_obj  "./mouse_lung_tips.list" ;;(*S.prune_tips mouse_lung_tips_unpruned mouse_lung_lcc ;;
dump_obj "./mouse_lung_tips.list" mouse_lung_tips ;;*)



let mouse_lung_rough_local_skel = slurp_obj "./mouse_lung_rough_local_skel.abbs" ;;
S.remove_globally mouse_lung_tips mouse_lung_rough_local_skel ;;

(* TEMP mouse_lung_dropbox comment *)

Printf.printf "\n Finding rough skeleton \t Processor time: %f s\n%!" (Sys.time()) ;;
let mouse_lung_skeleton = S.skeleton mouse_lung_lcc mouse_lung_tips true ;;
dump_obj "./mouse_lung_skeleton.abbs" mouse_lung_skeleton ;;



Printf.printf "\n Finding rough segments \t Processor time: %f s\n%!" (Sys.time()) ;;
let mouse_lung_test = S.analyze_skeleton mouse_lung_lcc mouse_lung_skeleton ;;
Printf.printf "\n Finding final skelton and tips \t Processor time: %f s\n%!" (Sys.time()) ;;
let (mouse_lung_skel_final, mouse_lung_good_tips) = S.skeleton_final mouse_lung_skeleton mouse_lung_test mouse_lung_tips ;;
dump_obj "./mouse_lung_t900_skel_final.abbs" mouse_lung_skel_final ;;
dump_obj "./mouse_lung_t900_good_tips.list" mouse_lung_good_tips ;;
Printf.printf "\n Finding final segments \t Processor time: %f s\n%!" (Sys.time()) ;;
let mouse_lung_final_test = S.analyze_skeleton mouse_lung_lcc mouse_lung_skel_final ;;
dump_obj "./mouse_lung_t900_final_test.segs" mouse_lung_final_test ;;
Printf.printf "\n Organizing tree \t Processor time: %f s\n%!" (Sys.time()) ;;
let (mouse_lung_sega_root, mouse_lung_sega, mouse_lung_parents, mouse_lung_children) = S.parents_and_children mouse_lung_final_test mouse_lung_skel_final mouse_lung_good_tips ;;
Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;



Printf.printf "\n Found %d segments:\n%!" (List.length mouse_lung_final_test) ;
print_segments_short_summaries mouse_lung_final_test ;
Printf.printf "\n sn_lcc.count = %d\n sum of volumes = %f%!" (ABBS.count mouse_lung_lcc) (sum_segments_vol 0. mouse_lung_final_test) ;
Printf.printf "\n tip separation and measured length:\n%!" ; compare_segment_lengths_with_tips mouse_lung_final_test ;
Printf.printf "\n The tree is:\n%!" ;
print_tree [mouse_lung_sega_root] mouse_lung_parents mouse_lung_children mouse_lung_sega ;;
Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;


(*Printf.printf "\n mouse_lung_dropbox with tips (0-111):\n%s%!" (ABBS.ascii_slice_with_tips 0 111 mouse_lung_lcc mouse_lung_tips mlt_width mlt_height mlt_intens_min mlt_intens_max "z") ;;
Printf.printf "\n mouse_lung_dropbox with tips (112-223):\n%s%!" (ABBS.ascii_slice_with_tips 112 223 mouse_lung_lcc mouse_lung_tips mlt_width mlt_height mlt_intens_min mlt_intens_max "z") ;;
Printf.printf "\n mouse_lung_dropbox with tips (224-336):\n%s%!" (ABBS.ascii_slice_with_tips 224 336 mouse_lung_lcc mouse_lung_tips mlt_width mlt_height mlt_intens_min mlt_intens_max "z") ;;
Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;*)

END mouse_lung_dropbox comment *)






Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;;

Printf.printf "\n\n TO DO:\n\t - Take into account asymmetries in voxel dimensions\n\t - Identify best tip in same group better\n\t - Use better digital curve length estimator\n\t - Before finding tips, fill in holes?\n\t - Skeletonize better?\n\t - Deal with loops (identify when switching from local to global connectivity?)\n\n\t - Use None instead of some other invalid output (like (-1, -1, -1) in ABBS)\n\t - Implement better organized lists (like Point.Set) for faster searching\n\n"



(* graphGL is not compatible with Windows

Printf.printf "\n\n Starting graphGL tests.\n%!" ;;

let test_edges = [] ;;
let test_vertex_colors = [] ;;
let test_vertices = ABBS.get_points fromP3;;
let test_vertex_colors = Mu.map (Mu.constant (0,0,0,255)) test_vertices ;;

GraphGL.display_loop
  ~edges:test_edges
  ~edge_colors:test_edge_colors
  ~vertices:test_vertices
  ~vertex_colors:test_vertex_colors ;;

Printf.printf "\n Processor time: %f s\n%!" (Sys.time()) ;; *)