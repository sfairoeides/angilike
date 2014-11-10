open BatBitSet ;;
open Printf ;;
open Sys ;;

type t = {
	min_dim : int*int*int ;
	max_dim : int*int*int ;
	ext_dim : int*int*int ;
	num_arrays : int ;
	split_dim : string ; (* "x" or "y" or "z"  *)
	abbs : BatBitSet.t array ;
}

let get_min_dim abbs = abbs.min_dim ;;

let get_max_dim abbs = abbs.max_dim ;;

let get_ext_dim abbs = abbs.ext_dim ;;

exception Will_not_fit_within_memory_constraints of string ;;

let to_string_triplet (x, y, z) = Printf.sprintf "(%d,%d,%d)" x y z

let print_summary a =
	Printf.printf "\n min_dim = %s\n max_dim = %s\n ext_dim = %s\n split_dim = %s\n%!" (to_string_triplet a.min_dim) (to_string_triplet a.max_dim) (to_string_triplet a.ext_dim) a.split_dim


let is_valid position abbs =
	let (x, y, z) = position in
	let (x_min, y_min, z_min) = abbs.min_dim in
	let (x_max, y_max, z_max) = abbs.max_dim in
		x_min <= x && x <= x_max && y_min <= y && y <= y_max && z_min <= z && z <= z_max

(* Use None instead of (-1, -1)!!!! *)
(* if modified, check position_raw for consistency *)
let array_and_index position abbs =
	if is_valid position abbs then begin
		let (x_raw, y_raw, z_raw) = position in
		let (x_ext, y_ext, z_ext) = abbs.ext_dim in
		let (x_min, y_min, z_min) = abbs.min_dim in
		let (x, y, z) = (x_raw - x_min, y_raw - y_min, z_raw - z_min) in
		if abbs.split_dim = "z" then (z, y*x_ext + x)
		else if abbs.split_dim = "y" then (y, z*x_ext + x)
		else (x, y*z_ext + z)
	end else (-1, -1)

(* Use None instead of (-1, -1, -1)!!!! *)
(* if modified, check array_and_index for consistency *)
let position_raw arr_and_ind abbs =
	let (array_index, index) = arr_and_ind in
	let (x_ext, y_ext, z_ext) = abbs.ext_dim in
	let (x_min, y_min, z_min) = abbs.min_dim in
	if array_index >= 0 && index >= 0 then begin
		if abbs.split_dim = "z" then ((index mod x_ext) + x_min, index/x_ext + y_min, array_index + z_min)
		else if abbs.split_dim = "y" then ((index mod x_ext) + x_min, array_index + y_min, index/x_ext + z_min)
		else (array_index + x_min, index/z_ext + y_min, (index mod z_ext) + z_min)
	end else (-1, -1, -1)


let is_set abbs position =
	let (array_index, index) = array_and_index position abbs in
		if(array_index >= 0 && index >= 0) then
			BatBitSet.mem abbs.abbs.(array_index) index
		else false

let is_not_set abbs position =
	let (array_index, index) = array_and_index position abbs in
		if(array_index >= 0 && index >= 0) then
			not (BatBitSet.mem abbs.abbs.(array_index) index)
		else true

let print_all_elements abbs =
	let (x_min, y_min, z_min) = abbs.min_dim in
	let (x_max, y_max, z_max) = abbs.max_dim in
	Printf.printf "\n(x, y, z)\t state%!" ;
	for x = x_min to x_max do
		for y = y_min to y_max do
			for z = z_min to z_max do
				Printf.printf "\n(%d, %d, %d)\t" x y z;
				if is_set abbs (x, y, z) then Printf.printf "set%!" else Printf.printf "unset%!"
			done
		done
	done ;
	Printf.printf "\n%!"

let max_allowed_array_size = 8*Sys.max_string_length;; (*int_of_float (2.**(float_of_int (Sys.word_size - 2)) -. 1.) ;;*)

(*let sort_triplet trip =
	let trip_ordered = ref trip in
	let (a, b, c) = !trip_ordered in
		if a < b then trip_ordered := (b, a, c) ;
	let (a, b, c) = !trip_ordered in
		if b < c then trip_ordered := (a, c, b) ;
	let (a, b, c) = !trip_ordered in
		if a < b then trip_ordered := (b, a, c) ;
	!trip_ordered*)

(*let num_BatBitSet_arrays_necessary dim_min dim_max =
	let (x_min, y_min, z_min) = dim_min in
	let (x_max, y_max, z_max) = dim_max in
	let x_ext = x_max - x_min in
	let y_ext = y_max - y_min in
	let z_ext = z_max - z_min in
	let (max_ext, mid_ext, min_ext) = sort_triplet (x_ext, y_ext, z_ext) in
	let single_array_size = mid_ext*min_ext in
	let num_slices_per_array = max_allowed_array_size/single_array_size in
		1 + max_ext/num_slices_per_array*)

let create max_size_in_GB min_dim max_dim =
	let (x_min, y_min, z_min) = min_dim in
	let (x_max, y_max, z_max) = max_dim in
	let x_ext = x_max - x_min + 1 in
	let y_ext = y_max - y_min + 1 in
	let z_ext = z_max - z_min + 1 in
	if (float_of_int (x_ext*y_ext*x_ext)) > (max_size_in_GB*.1024.*.1024.*.1024.*.8.) then begin
		raise (Will_not_fit_within_memory_constraints (Printf.sprintf "\n ArrayBatBitSet.create cannot accommodate %f GB of data within the desired %f GB limit.\n%!" ((float_of_int (x_ext*y_ext*z_ext))/.1024./.1024./.1024./.8.) max_size_in_GB))
	end else
		(* change <= to >= if there are problems allocating individual arrays *)
		let sdim = if (x_ext <= y_ext && x_ext <= z_ext) then "x"
		else if (y_ext <= x_ext && y_ext <= z_ext) then "y"
		else "z" in
		let array_size = if sdim = "x" then y_ext*z_ext
		else if sdim = "y" then x_ext*z_ext
		else x_ext*y_ext in
		let narrays = if sdim = "x" then x_ext
		else if sdim = "y" then y_ext
		else z_ext in
		let array_of_batBitSet num_arrays array_size =
			let result = Array.make num_arrays (BatBitSet.create array_size) in
			for i = 1 to num_arrays - 1 do
				result.(i) <- BatBitSet.create array_size
			done ;
			result
		in
		{	min_dim = min_dim ;
			max_dim = max_dim ;
			ext_dim = (x_ext, y_ext, z_ext) ;
			split_dim = sdim ;
			num_arrays = narrays ;
			abbs = array_of_batBitSet narrays array_size
		}

let set abbs position =
	if is_valid position abbs then begin
		let (array_index, index) = array_and_index position abbs in
			BatBitSet.set abbs.abbs.(array_index) index
	end

let unset abbs position =
	if is_valid position abbs then begin
		let (array_index, index) = array_and_index position abbs in
			BatBitSet.unset abbs.abbs.(array_index) index
	end

let next_set_bit a n =
	let next_bit = BatBitSet.next_set_bit a n in
	match next_bit with
		| Some b -> b
		| None -> -2 ;;

(* Use None instead of (-1, -1, -1) and (-1, -1)!!!! *)
let next_set_position_after abbs position =
	let (x_orig, y_orig, z_orig) = position in
	let (x_min, y_min, z_min) = abbs.min_dim in
	let (x_max, y_max, z_max) = abbs.max_dim in
	let (x, y, z) = (max x_orig x_min, max y_orig y_min, max z_orig z_min) in
	if (x <= x_max && y <= y_max && z <= z_max) then begin
		let (array_index, index) = array_and_index (x, y, z) abbs in
		let find_next_set_bit_after abbs array_index index =
			let next_array_index = ref array_index in
			let next_index = ref (index) in
			if x_orig >= x_min then next_index := index + 1 ;
			next_index := next_set_bit abbs.abbs.(!next_array_index) !next_index ;
			while (!next_index < 0 && !next_array_index < abbs.num_arrays - 1) do
				next_array_index := !next_array_index + 1 ;
				next_index := next_set_bit abbs.abbs.(!next_array_index) 0
			done;
			if !next_index >= 0 then (!next_array_index, !next_index) else (-1, -1)
		in
		let (next_array_index, next_index) = find_next_set_bit_after abbs array_index index in
		if (next_array_index >= 0 && next_index >= 0) then position_raw (next_array_index, next_index) abbs
		else (-1, -1, -1)
	end else (-1, -1, -1)

let rec set_list abbs positions = match positions with
	[] -> ()
	| front::rest -> set abbs front ; set_list abbs rest

let rec unset_list abbs positions = match positions with
	[] -> ()
	| front::rest -> unset abbs front ; unset_list abbs rest

(*let unset_all abbs =
	for i = 0 to abbs.num_arrays - 1 do
		let next_bit = ref (next_set_bit abbs.abbs.(i) 0) in
		let all_unset = ref (!next_bit < 0) in
		while not !all_unset do
			BatBitSet.unset abbs.abbs.(i) !next_bit ;
			next_bit := next_set_bit abbs.abbs.(i) (!next_bit + 1) ;
			all_unset := !next_bit < 0
		done
	done*)

(* A preferrable version of this method is found in skeleton.ml *)
let list_twenty_six_neighbors (x, y, z) =
 [(x - 1, y - 1, z - 1); (x + 0, y - 1, z - 1); (x + 1, y - 1, z - 1);
  (x - 1, y + 0, z - 1); (x + 0, y + 0, z - 1); (x + 1, y + 0, z - 1);
  (x - 1, y + 1, z - 1); (x + 0, y + 1, z - 1); (x + 1, y + 1, z - 1);
  (x - 1, y - 1, z + 0); (x + 0, y - 1, z + 0); (x + 1, y - 1, z + 0);
  (x - 1, y + 0, z + 0);                        (x + 1, y + 0, z + 0);
  (x - 1, y + 1, z + 0); (x + 0, y + 1, z + 0); (x + 1, y + 1, z + 0);
  (x - 1, y - 1, z + 1); (x + 0, y - 1, z + 1); (x + 1, y - 1, z + 1);
  (x - 1, y + 0, z + 1); (x + 0, y + 0, z + 1); (x + 1, y + 0, z + 1);
  (x - 1, y + 1, z + 1); (x + 0, y + 1, z + 1); (x + 1, y + 1, z + 1)]

let degree abbs position =
	let rec check_list_count ell = match ell with
		[] -> 0
		| front::rest -> if (is_set abbs front) then
				1 + (check_list_count rest)
			else
				check_list_count rest
	in check_list_count (list_twenty_six_neighbors position)
	

(* A million gigabytes should be enough *)
(* could be faster!!!! *)
let create_exact_copy abbs_orig =
	let (x_min, y_min, z_min) = abbs_orig.min_dim in
	let (x_max, y_max, z_max) = abbs_orig.max_dim in
	let abbs_copy = create 1000000. abbs_orig.min_dim abbs_orig.max_dim in
	for x = x_min to x_max do
		for y = y_min to y_max do
			for z = z_min to z_max do
				if is_set abbs_orig (x, y, z) then set abbs_copy (x, y, z)
			done
		done
	done ;
	abbs_copy

(* A million gigabytes should be enough *)
let create_blank_copy abbs_orig = create 1000000. abbs_orig.min_dim abbs_orig.max_dim

(* A million gigabytes should be enough *)
let create_tight_copy abbs_orig =
	let (x_orig_min, y_orig_min, z_orig_min) = abbs_orig.min_dim in
	let (x_orig_max, y_orig_max, z_orig_max) = abbs_orig.max_dim in
	let (x_min, y_min, z_min, x_max, y_max, z_max) = (ref x_orig_max, ref y_orig_max, ref z_orig_max, ref x_orig_min, ref y_orig_min, ref z_orig_min) in
	let next_bit = ref (next_set_position_after abbs_orig (-1, -1, -1)) in
	let (x_init, _, _) = !next_bit in
	let covered_everything = ref (x_init < 0) in
	while not !covered_everything do
		let (x, y, z) = !next_bit in
		x_min := min x !x_min ;
		x_max := max x !x_max ;
		y_min := min y !y_min ;
		y_max := max y !y_max ;
		z_min := min z !z_min ;
		z_max := max z !z_max ;
		next_bit := next_set_position_after abbs_orig !next_bit ;
		let (x_next, _, _) = !next_bit in
		covered_everything := x_next < 0
	done ;
	if !x_min > x_orig_min then x_min := !x_min - 1 ;
	if !x_max < x_orig_max then x_max := !x_max + 1 ;
	if !y_min > y_orig_min then y_min := !y_min - 1 ;
	if !y_max < y_orig_max then y_max := !y_max + 1 ;
	if !z_min > z_orig_min then z_min := !z_min - 1 ;
	if !z_max < z_orig_max then z_max := !z_max + 1 ;
	let abbs = create 1000000. (!x_min, !y_min, !z_min) (!x_max, !y_max, !z_max) in
	next_bit := next_set_position_after abbs_orig (-1, -1, -1) ;
	covered_everything := x_init < 0 ;
	while not !covered_everything do
		set abbs !next_bit ;
		next_bit := next_set_position_after abbs_orig !next_bit ;
		let (x_next, _, _) = !next_bit in
		covered_everything := x_next < 0
	done ;
	abbs

(*
(* A million gigabytes should be enough *)
let create_difference abbs_alpha abbs_beta =
	let (x_min_a, y_min_a, z_min_a) = abbs_alpha.min_dim in
	let (x_max_a, y_max_a, z_max_a) = abbs_alpha.max_dim in
	let (x_min_b, y_min_b, z_min_b) = abbs_beta.min_dim in
	let (x_max_b, y_max_b, z_max_b) = abbs_beta.max_dim in
	let (x_min, y_min, z_min) = (min x_min_a x_min_b, min y_min_a y_min_b, min z_min_a z_min_b) in
	let (x_max, y_max, z_max) = (max x_max_a x_max_b, max y_max_a y_max_b, max z_max_a z_max_b) in
	let abbs_diff = create 1000000. (x_min, y_min, z_min) (x_max, y_max, z_max) in
	for x = x_min to x_max do
		for y = y_min to y_max do
			for z = z_min to z_max do
				if (not ((is_set abbs_alpha (x, y, z)) = (is_set abbs_beta (x, y, z)))) then set (x, y, z) abbs_diff
			done
		done
	done ;
	abbs_diff
*)

(*let copy_from_to abbs_from abbs_to =
	let (x_min_from, y_min_from, z_min_from) = abbs_from.min_dim in
	let (x_min_to, y_min_to, z_min_to) = abbs_to.min_dim in
	let (x_max_from, y_max_from, z_max_from) = abbs_from.max_dim in
	let (x_max_to, y_max_to, z_max_to) = abbs_to.max_dim in
	if (x_min_from = x_min_to && x_max_from = x_max_to && y_min_from = y_min_to && y_max_from = y_max_to && z_min_from = z_min_to && z_max_from = z_max_to) then begin
		for x = x_min_from to x_max_from do
			for y = y_min_from to y_max_from do
				for z = z_min_from to z_max_from do
					if is_set abbs_from (x, y, z) then set (x, y, z) abbs_to
				done
			done
		done
	end else Printf.printf "\n Error arrayBatBitSet.copy_from_to: dimension boundary mismatch!\n%!"*)

(*let get_points abbs =
	let point_list = ref [] in
		let (x_ext, y_ext, z_ext) = abbs.ext_dim in
		let num_layers =
			if abbs.split_dim = "x" then x_ext
			else if abbs.split_dim = "y" then y_ext
			else z_ext in
		for layer = 0 to num_layers - 1 do
			let next_set = ref (next_set_bit abbs.abbs.(layer) 0) in
			while !next_set >= 0 do
				point_list := (position_raw (layer, !next_set) abbs) :: !point_list ;
				next_set := next_set_bit abbs.abbs.(layer) (!next_set + 1) ;
			done
		done ;
		!point_list*)

let ascii_intensity_set = Array.create 10 " " ;;
let ascii_count = ref 0 ;;
Array.set ascii_intensity_set !ascii_count " " ;; ascii_count := !ascii_count + 1 ;;
Array.set ascii_intensity_set !ascii_count "." ;; ascii_count := !ascii_count + 1 ;;
Array.set ascii_intensity_set !ascii_count "," ;; ascii_count := !ascii_count + 1 ;;
Array.set ascii_intensity_set !ascii_count ":" ;; ascii_count := !ascii_count + 1 ;;
Array.set ascii_intensity_set !ascii_count ";" ;; ascii_count := !ascii_count + 1 ;;
Array.set ascii_intensity_set !ascii_count "|" ;; ascii_count := !ascii_count + 1 ;;
Array.set ascii_intensity_set !ascii_count "o" ;; ascii_count := !ascii_count + 1 ;;
Array.set ascii_intensity_set !ascii_count "O" ;; ascii_count := !ascii_count + 1 ;;
Array.set ascii_intensity_set !ascii_count "8" ;; ascii_count := !ascii_count + 1 ;;
Array.set ascii_intensity_set !ascii_count "#" ;; ascii_count := !ascii_count + 1 ;;

(* slice_start and slice_finish are the absolute coordinates, not to be offset by min_dim *)
let ascii_slice slice_start slice_finish abbs width height min_intensity max_intensity slice_dim =
	let (x_ext, y_ext, z_ext) = abbs.ext_dim in
	let (x_min, y_min, z_min) = abbs.min_dim in
	(*Printf.printf "\next, min gives (%d,%d,%d), (%d,%d,%d)\n%!" x_ext y_ext z_ext x_min y_min z_min *)
	let intensity_inc = (max_intensity -. min_intensity)/.(float_of_int (Array.length ascii_intensity_set - 1)) in
	let average_intensity x_start x_finish y_start y_finish z_start z_finish =
		let intensity_count = ref 0 in
		let voxel_count = ref 0 in
		for x = x_start to x_finish do
			for y = y_start to y_finish do
				for z = z_start to z_finish do
					if is_valid (x, y, z) abbs then begin
						if is_set abbs (x, y, z) then intensity_count := !intensity_count + 1 ;
						voxel_count := !voxel_count + 1
					end
				done
			done
		done ;
		(*Printf.printf "\ni_count = %d\t v_count = %d for (%d,%d,%d) to [%d,%d,%d]" !intensity_count !voxel_count x_start y_start z_start x_finish y_finish z_finish ;*)
		if !voxel_count = 0 then 0.0
		else (float_of_int !intensity_count)/.(float_of_int !voxel_count)
	in
	let block_extremes block_min block_index block_size =
		let block_start = block_min + block_index*block_size in
		(block_start, max (block_min + (block_index + 1)*block_size - 1) block_start) in
	let ascii_intensity_index intensity =
		if intensity <= min_intensity then 0
		else if intensity >= max_intensity then (Array.length ascii_intensity_set) - 1
		else int_of_float (floor ((intensity -. min_intensity)/.intensity_inc))
	in
	let slice_string = ref "" in
	if slice_dim = "x" then begin
		let (x_start, x_finish) = (slice_start, slice_finish) in
		let (z_block_size, y_block_size) = (max 1 z_ext/(width - 1) , max 1 y_ext/(height - 1)) in
		for y_block = 0 to height - 1 do
			let (y_start, y_finish) = block_extremes y_min y_block y_block_size in
			for z_block = 0 to width - 1 do
				let (z_start, z_finish) = block_extremes z_min z_block z_block_size in
				let intens = average_intensity x_start x_finish y_start y_finish z_start z_finish in
				slice_string := !slice_string ^ ascii_intensity_set.(ascii_intensity_index intens)
			done ;
			slice_string := !slice_string ^ "\n"
		done
	end else if slice_dim = "y" then begin
		let (y_start, y_finish) = (slice_start, slice_finish) in
		let (x_block_size, z_block_size) = (max 1 x_ext/(width - 1), max 1 z_ext/(height - 1)) in
		for z_block = 0 to height - 1 do
			let (z_start, z_finish) = block_extremes z_min z_block z_block_size in
			for x_block = 0 to width - 1 do
				let (x_start, x_finish) = block_extremes x_min x_block x_block_size in
				let intens = average_intensity x_start x_finish y_start y_finish z_start z_finish in
				slice_string := !slice_string ^ ascii_intensity_set.(ascii_intensity_index intens)
			done ;
			slice_string := !slice_string ^ "\n"
		done
	end else begin
		let (z_start, z_finish) = (slice_start, slice_finish) in
		let (x_block_size, y_block_size) = (max 1 x_ext/(width - 1), max 1 y_ext/(height - 1)) in
		for y_block = 0 to height - 1 do
			let (y_start, y_finish) = block_extremes y_min y_block y_block_size in
			for x_block = 0 to width - 1 do
				let (x_start, x_finish) = block_extremes x_min x_block x_block_size in
				let intens = average_intensity x_start x_finish y_start y_finish z_start z_finish in
				slice_string := !slice_string ^ ascii_intensity_set.(ascii_intensity_index intens)
			done ;
			slice_string := !slice_string ^ "\n"
		done
	end ;
	!slice_string

(* slice_start and slice_finish are the absolute coordinates, not to be offset by min_dim *)
let ascii_slice_with_tips slice_start slice_finish abbs tips width height min_intensity max_intensity slice_dim =
	let (x_ext, y_ext, z_ext) = abbs.ext_dim in
	let (x_min, y_min, z_min) = abbs.min_dim in
	let intensity_inc = (max_intensity -. min_intensity)/.(float_of_int (Array.length ascii_intensity_set - 1)) in
	let average_intensity x_start x_finish y_start y_finish z_start z_finish =
		let intensity_count = ref 0 in
		let voxel_count = ref 0 in
		for x = x_start to x_finish do
			for y = y_start to y_finish do
				for z = z_start to z_finish do
					if is_valid (x, y, z) abbs then begin
						if is_set abbs (x, y, z) then intensity_count := !intensity_count + 1 ;
						voxel_count := !voxel_count + 1
					end
				done
			done
		done ;
		(*Printf.printf "\ni_count = %d\t v_count = %d for (%d,%d,%d) to [%d,%d,%d]" !intensity_count !voxel_count x_start y_start z_start x_finish y_finish z_finish ;*)
		if !voxel_count = 0 then 0.0
		else (float_of_int !intensity_count)/.(float_of_int !voxel_count)
	in
	let block_extremes block_min block_index block_size =
		let block_start = block_min + block_index*block_size in
		(block_start, max (block_min + (block_index + 1)*block_size - 1) block_start) in
	let ascii_intensity_index intensity =
		if intensity <= min_intensity then 0
		else if intensity >= max_intensity then (Array.length ascii_intensity_set) - 1
		else int_of_float (floor ((intensity -. min_intensity)/.intensity_inc))
	in
	let subvol_contains_tip x_start x_finish y_start y_finish z_start z_finish =
		let rec is_in_subvolume ell = match ell with
			[] -> false
			| (x, y, z)::rest -> if (x_start <= x && x <= x_finish && y_start <= y && y <= y_finish && z_start <= z && z <= z_finish) then true else is_in_subvolume rest
		in
		is_in_subvolume tips
	in
	let subvolume_string x_start x_finish y_start y_finish z_start z_finish =
		if subvol_contains_tip x_start x_finish y_start y_finish z_start z_finish then "+"
		else begin
			let intens = average_intensity x_start x_finish y_start y_finish z_start z_finish in
			ascii_intensity_set.(ascii_intensity_index intens)
		end
	in
	let slice_string = ref "" in
	if slice_dim = "x" then begin
		let (x_start, x_finish) = (slice_start, slice_finish) in
		let (z_block_size, y_block_size) = (max 1 z_ext/(width - 1) , max 1 y_ext/(height - 1)) in
		for y_block = 0 to height - 1 do
			let (y_start, y_finish) = block_extremes y_min y_block y_block_size in
			for z_block = 0 to width - 1 do
				let (z_start, z_finish) = block_extremes z_min z_block z_block_size in
				slice_string := !slice_string ^ (subvolume_string x_start x_finish y_start y_finish z_start z_finish)
			done ;
			slice_string := !slice_string ^ "\n"
		done
	end else if slice_dim = "y" then begin
		let (y_start, y_finish) = (slice_start, slice_finish) in
		let (x_block_size, z_block_size) = (max 1 x_ext/(width - 1), max 1 z_ext/(height - 1)) in
		for z_block = 0 to height - 1 do
			let (z_start, z_finish) = block_extremes z_min z_block z_block_size in
			(*Printf.printf "\nz_start = %d, z_finish = %d" z_start z_finish ;*)
			for x_block = 0 to width - 1 do
				let (x_start, x_finish) = block_extremes x_min x_block x_block_size in
				slice_string := !slice_string ^ (subvolume_string x_start x_finish y_start y_finish z_start z_finish)
			done ;
			slice_string := !slice_string ^ "\n"
		done
	end else begin
		let (z_start, z_finish) = (slice_start, slice_finish) in
		let (x_block_size, y_block_size) = (max 1 x_ext/(width - 1), max 1 y_ext/(height - 1)) in
		for y_block = 0 to height - 1 do
			let (y_start, y_finish) = block_extremes y_min y_block y_block_size in
			for x_block = 0 to width - 1 do
				let (x_start, x_finish) = block_extremes x_min x_block x_block_size in
				slice_string := !slice_string ^ (subvolume_string x_start x_finish y_start y_finish z_start z_finish)
			done ;
			slice_string := !slice_string ^ "\n"
		done
	end ;
	!slice_string

(* slice_start and slice_finish are the absolute coordinates, not to be offset by min_dim *)
let ascii_slice_with_tips_and_skeleton slice_start slice_finish abbs tips abbs_skel width height min_intensity max_intensity slice_dim =
	let (x_ext, y_ext, z_ext) = abbs.ext_dim in
	let (x_min, y_min, z_min) = abbs.min_dim in
	let intensity_inc = (max_intensity -. min_intensity)/.(float_of_int (Array.length ascii_intensity_set - 1)) in
	let average_intensity x_start x_finish y_start y_finish z_start z_finish =
		let intensity_count = ref 0 in
		let voxel_count = ref 0 in
		for x = x_start to x_finish do
			for y = y_start to y_finish do
				for z = z_start to z_finish do
					if is_valid (x, y, z) abbs then begin
						if is_set abbs (x, y, z) then intensity_count := !intensity_count + 1 ;
						voxel_count := !voxel_count + 1
					end
				done
			done
		done ;
		(*Printf.printf "\ni_count = %d\t v_count = %d for (%d,%d,%d) to [%d,%d,%d]" !intensity_count !voxel_count x_start y_start z_start x_finish y_finish z_finish ;*)
		if !voxel_count = 0 then 0.0
		else (float_of_int !intensity_count)/.(float_of_int !voxel_count)
	in
	let block_extremes block_min block_index block_size =
		let block_start = block_min + block_index*block_size in
		(block_start, max (block_min + (block_index + 1)*block_size - 1) block_start) in
	let ascii_intensity_index intensity =
		if intensity <= min_intensity then 0
		else if intensity >= max_intensity then (Array.length ascii_intensity_set) - 1
		else int_of_float (floor ((intensity -. min_intensity)/.intensity_inc))
	in
	let subvol_contains_tip x_start x_finish y_start y_finish z_start z_finish =
		let rec is_in_subvolume ell = match ell with
			[] -> false
			| (x, y, z)::rest -> if (x_start <= x && x <= x_finish && y_start <= y && y <= y_finish && z_start <= z && z <= z_finish) then true else is_in_subvolume rest
		in
		is_in_subvolume tips
	in
	let subvol_contains_skel x_start x_finish y_start y_finish z_start z_finish =
		let contains_skel = ref false in
		(* Can be FASTER!!!! *)
		for x = x_start to x_finish do
			for y = y_start to y_finish do
				for z = z_start to z_finish do
					if is_set abbs_skel (x, y, z) then contains_skel := true ;
				done
			done
		done ;
		!contains_skel
	in
	let subvolume_string x_start x_finish y_start y_finish z_start z_finish =
		if subvol_contains_tip x_start x_finish y_start y_finish z_start z_finish then "+"
		else if subvol_contains_skel  x_start x_finish y_start y_finish z_start z_finish then "X"
		else begin
			let intens = average_intensity x_start x_finish y_start y_finish z_start z_finish in
			ascii_intensity_set.(ascii_intensity_index intens)
		end
	in
	let slice_string = ref "" in
	if slice_dim = "x" then begin
		let (x_start, x_finish) = (slice_start, slice_finish) in
		let (z_block_size, y_block_size) = (max 1 z_ext/(width - 1) , max 1 y_ext/(height - 1)) in
		for y_block = 0 to height - 1 do
			let (y_start, y_finish) = block_extremes y_min y_block y_block_size in
			for z_block = 0 to width - 1 do
				let (z_start, z_finish) = block_extremes z_min z_block z_block_size in
				slice_string := !slice_string ^ (subvolume_string x_start x_finish y_start y_finish z_start z_finish)
			done ;
			slice_string := !slice_string ^ "\n"
		done
	end else if slice_dim = "y" then begin
		let (y_start, y_finish) = (slice_start, slice_finish) in
		let (x_block_size, z_block_size) = (max 1 x_ext/(width - 1), max 1 z_ext/(height - 1)) in
		for z_block = 0 to height - 1 do
			let (z_start, z_finish) = block_extremes z_min z_block z_block_size in
			(*Printf.printf "\nz_start = %d, z_finish = %d" z_start z_finish ;*)
			for x_block = 0 to width - 1 do
				let (x_start, x_finish) = block_extremes x_min x_block x_block_size in
				slice_string := !slice_string ^ (subvolume_string x_start x_finish y_start y_finish z_start z_finish)
			done ;
			slice_string := !slice_string ^ "\n"
		done
	end else begin
		let (z_start, z_finish) = (slice_start, slice_finish) in
		let (x_block_size, y_block_size) = (max 1 x_ext/(width - 1), max 1 y_ext/(height - 1)) in
		for y_block = 0 to height - 1 do
			let (y_start, y_finish) = block_extremes y_min y_block y_block_size in
			for x_block = 0 to width - 1 do
				let (x_start, x_finish) = block_extremes x_min x_block x_block_size in
				slice_string := !slice_string ^ (subvolume_string x_start x_finish y_start y_finish z_start z_finish)
			done ;
			slice_string := !slice_string ^ "\n"
		done
	end ;
	!slice_string

let count abbs =
	let count = ref 0 in
	for i = 0 to abbs.num_arrays - 1 do
		count := !count + (BatBitSet.count abbs.abbs.(i))
	done ;
	!count
	