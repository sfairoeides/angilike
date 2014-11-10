open BatBitSet ;;
open Printf ;;
module ABBS = ArrayBatBitSet ;;

open Sys ;;

let equal_triplets (ax, ay, az) (bx, by, bz) = (ax = bx && ay = by && az = bz)

let list_six_neighbors (x, y, z) =
	[						 						 
							 (x + 0, y + 0, z - 1); 						
							  												 
	  						 (x + 0, y - 1, z + 0); 						
	  (x - 1, y + 0, z + 0);                        (x + 1, y + 0, z + 0);
	   						 (x + 0, y + 1, z + 0); 						
							 												 
	   						 (x + 0, y + 0, z + 1) 					
							  											]

let list_eight_neighbors (x, y, z) =
	 [(x - 1, y - 1, z - 1);						(x + 1, y - 1, z - 1);
																			
	  (x - 1, y + 1, z - 1);						(x + 1, y + 1, z - 1);
																			
																			
																			
	  (x - 1, y - 1, z + 1);						(x + 1, y - 1, z + 1);
																			
	  (x - 1, y + 1, z + 1); 						(x + 1, y + 1, z + 1)]

let list_twelve_neighbors (x, y, z) =
	[						 (x + 0, y - 1, z - 1);						 
	  (x - 1, y + 0, z - 1); 						(x + 1, y + 0, z - 1);
							 (x + 0, y + 1, z - 1);						 
	  (x - 1, y - 1, z + 0); 						(x + 1, y - 1, z + 0);
																		
	  (x - 1, y + 1, z + 0);						(x + 1, y + 1, z + 0);
							 (x + 0, y - 1, z + 1);						 
	  (x - 1, y + 0, z + 1);						(x + 1, y + 0, z + 1);
							 (x + 0, y + 1, z + 1)						]

let list_eighteen_neighbors (x, y, z) =
	[						 (x + 0, y - 1, z - 1);						 
	  (x - 1, y + 0, z - 1); (x + 0, y + 0, z - 1); (x + 1, y + 0, z - 1);
							 (x + 0, y + 1, z - 1);						 
	  (x - 1, y - 1, z + 0); (x + 0, y - 1, z + 0); (x + 1, y - 1, z + 0);
	  (x - 1, y + 0, z + 0);                        (x + 1, y + 0, z + 0);
	  (x - 1, y + 1, z + 0); (x + 0, y + 1, z + 0); (x + 1, y + 1, z + 0);
							 (x + 0, y - 1, z + 1);						 
	  (x - 1, y + 0, z + 1); (x + 0, y + 0, z + 1); (x + 1, y + 0, z + 1);
							 (x + 0, y + 1, z + 1)						]

(* Concatenate previous lists so that the 26 neighbors are ordered by distance from center *)
let list_twenty_six_neighbors position = List.concat[list_six_neighbors position ; list_twelve_neighbors position ; list_eight_neighbors position]
	 (*[(x - 1, y - 1, z - 1); (x + 0, y - 1, z - 1); (x + 1, y - 1, z - 1);
	  (x - 1, y + 0, z - 1); (x + 0, y + 0, z - 1); (x + 1, y + 0, z - 1);
	  (x - 1, y + 1, z - 1); (x + 0, y + 1, z - 1); (x + 1, y + 1, z - 1);
	  (x - 1, y - 1, z + 0); (x + 0, y - 1, z + 0); (x + 1, y - 1, z + 0);
	  (x - 1, y + 0, z + 0);                        (x + 1, y + 0, z + 0);
	  (x - 1, y + 1, z + 0); (x + 0, y + 1, z + 0); (x + 1, y + 1, z + 0);
	  (x - 1, y - 1, z + 1); (x + 0, y - 1, z + 1); (x + 1, y - 1, z + 1);
	  (x - 1, y + 0, z + 1); (x + 0, y + 0, z + 1); (x + 1, y + 0, z + 1);
	  (x - 1, y + 1, z + 1); (x + 0, y + 1, z + 1); (x + 1, y + 1, z + 1)]*)

let list_twenty_six_neighbors_farthest_first position = List.concat[ list_eight_neighbors position ; list_twelve_neighbors position ; list_six_neighbors position ]

let list_one_hundred_twenty_four_neighbors (x, y, z) =
	let neighbor_list = ref [] in
	for i = -2 to 2 do
		for j = -2 to 2 do
			for k = -2 to 2 do
				if not (i = 0 && j = 0 && k = 0) then
				neighbor_list := (x + i, y + j, z + k)::!neighbor_list
			done
		done
	done ;
	!neighbor_list

(* The connected components should be small sets *)
let extract_first_cc neighbor_list_function b_abbs =
	let start_position = ABBS.next_set_position_after b_abbs (-1, -1, -1) in
    let (x_init, _, _) = start_position in
	if x_init < 0 then [] else begin
		ABBS.unset b_abbs start_position ;
		let cc_list = ref [start_position] in
		let to_check = ref [start_position] in
		while List.length !to_check > 0 do
			match !to_check with
				[] -> Printf.printf "\nBug in skeleton.tops.boundary_tips.extract_first_cc: to_check is empty, but has length greater than zero!\n%!" ;
				| check_position::rest ->
					let rec is_excluded_from ell start_position =
						match ell with
							[] -> true ;
							| front::remaining ->
								if (equal_triplets start_position front) then false else is_excluded_from remaining start_position
					in
					let rec unset_list ell =
						match ell with
							[] -> () ;
							| front::remaining -> ABBS.unset b_abbs front ; unset_list remaining
					in
					let set_neighbors = List.filter (ABBS.is_set b_abbs) (neighbor_list_function check_position) in
					let excluded_neighbors = List.filter ( is_excluded_from !to_check) set_neighbors in
					unset_list excluded_neighbors ;
					cc_list := List.concat [!cc_list; excluded_neighbors] ;
					to_check := List.concat [rest; excluded_neighbors] ;
		done ;
		!cc_list
	end

(* Returns a doublet with the count of the connected component's size and the position of a point in the next connected component *)
let extract_connected_component_size start_position abbs neighbor_list_function =
	if not (ABBS.is_set abbs start_position) then (0, (ABBS.next_set_position_after abbs start_position)) else begin
		ABBS.unset abbs start_position ;
		let cc_count = ref 1 in
		let to_check = ref [start_position] in
		while List.length !to_check > 0 do
			match !to_check with
				[] -> Printf.printf "\nBug in skeleton.tips.boundary_tips.extract_first_cc: to_check is empty, but has length greater than zero!\n%!" ;
				| front::rest ->
					let rec is_excluded_from ell pos =
						match ell with
							[] -> true ;
							| fr::remaining ->
								if (equal_triplets fr pos) then false else is_excluded_from remaining start_position
					in
					(*let rec unset_list ell =
						match ell with
							[] -> () ;
							| front::remaining -> ABBS.unset abbs front ; unset_list remaining
					in*)
					let set_neighbors = List.filter (ABBS.is_set abbs) (neighbor_list_function front) in
					let excluded_neighbors = List.filter ( is_excluded_from !to_check) set_neighbors in
					ABBS.unset_list abbs excluded_neighbors ;
					cc_count := !cc_count + (List.length excluded_neighbors) ;
					to_check := List.concat [rest; excluded_neighbors]
		done ;
		(!cc_count, (ABBS.next_set_position_after abbs start_position))
	end

(* Returns a doublet with the count of the connected component's size and the position of a point in the next connected component *)
let count_connected_component_size start_position abbs_orig neighbor_list_function =
	if not (ABBS.is_set abbs_orig start_position) then (0, (ABBS.next_set_position_after abbs_orig start_position)) else begin
		let abbs = ABBS.create_exact_copy abbs_orig in
		ABBS.unset abbs start_position ;
		let cc_count = ref 1 in
		let to_check = ref [start_position] in
		while List.length !to_check > 0 do
			match !to_check with
				[] -> Printf.printf "\nBug in skeleton.tops.boundary_tips.extract_first_cc: to_check is empty, but has length greater than zero!\n%!" ;
				| (cx, cy, cz)::rest ->
					let rec is_excluded_from ell (x, y, z) =
						match ell with
							[] -> true ;
							| (fx, fy, fz)::remaining ->
								if (x = fx && y = fy && z = fz) then false else is_excluded_from remaining (x, y, z)
					in
					(*let rec unset_list ell =
						match ell with
							[] -> () ;
							| front::remaining -> ABBS.unset abbs front ; unset_list remaining
					in*)
					let set_neighbors = List.filter (ABBS.is_set abbs) (neighbor_list_function (cx, cy, cz)) in
					let excluded_neighbors = List.filter ( is_excluded_from !to_check) set_neighbors in
					ABBS.unset_list abbs excluded_neighbors ;
					cc_count := !cc_count + (List.length excluded_neighbors) ;
					to_check := List.concat [rest; excluded_neighbors]
		done ;
		(!cc_count, (ABBS.next_set_position_after abbs start_position))
	end

(* Assumes 26 neighbors *)
let copy_connected_component abbs_orig start_position =
	if not (ABBS.is_set abbs_orig start_position) then (ABBS.create 1. (0, 0, 0) (0, 0, 0)) else begin
		let abbs = ABBS.create_exact_copy abbs_orig in
		let cc_abbs = ABBS.create 1000000. (ABBS.get_min_dim abbs) (ABBS.get_max_dim abbs) in
		ABBS.unset abbs start_position ;
		ABBS.set cc_abbs start_position ;
		let to_check = ref [start_position] in
		while List.length !to_check > 0 do
			match !to_check with
				[] -> Printf.printf "\nBug in skeleton.tops.boundary_tips.extract_first_cc: to_check is empty, but has length greater than zero!\n%!" ;
				| (cx, cy, cz)::rest ->
					let rec is_excluded_from ell (x, y, z) =
						match ell with
							[] -> true ;
							| (fx, fy, fz)::remaining ->
								if (x = fx && y = fy && z = fz) then false else is_excluded_from remaining (x, y, z)
					in
					(*let rec unset_list ell =
						match ell with
							[] -> () ;
							| front::remaining -> ABBS.unset abbs front ; unset_list remaining
					in*)
					(*let rec set_list ell =
						match ell with
							[] -> () ;
							| front::remaining -> ABBS.set front cc_abbs ; set_list remaining
					in*)
					let set_neighbors = List.filter (ABBS.is_set abbs) (list_twenty_six_neighbors (cx, cy, cz)) in
					let excluded_neighbors = List.filter ( is_excluded_from !to_check) set_neighbors in
					ABBS.unset_list abbs excluded_neighbors ;
					ABBS.set_list cc_abbs excluded_neighbors ;
					to_check := List.concat [rest; excluded_neighbors]
		done ;
		(*let (_, _, z_min) = ABBS.get_min_dim cc_abbs in
		let (_, _, z_max) = ABBS.get_max_dim cc_abbs in
		.printf "\n creating tight copy of cc_abbs:\n%s%!" (ABBS.ascii_slice z_min z_max cc_abbs 25 25 0. 1. "z") ;*)
		ABBS.create_tight_copy cc_abbs
	end

let largest_connected_component abbs =
	let total_set_bits = ABBS.count abbs in
	let running_total = ref 0 in
	let start_position = ref (ABBS.next_set_position_after abbs (-1, -1, -1))  in
	let lcc_start_position = ref !start_position in
	let lcc_size = ref 0 in
	let abbs_copy = ABBS.create_exact_copy abbs in
	let covered_everything = ref false in
	while not !covered_everything do
		let (cc_size, next_start_position) = extract_connected_component_size !start_position abbs_copy list_twenty_six_neighbors in
		running_total := !running_total + cc_size ;
		if !lcc_size < cc_size then
			lcc_size := cc_size; lcc_start_position := !start_position ;
		start_position := next_start_position ;
		let (x, _, _) = next_start_position in
		covered_everything := x < 0 || !lcc_size > total_set_bits - !running_total
	done ;
	copy_connected_component abbs !lcc_start_position

let center_of_mass_of_connected_component abbs_orig start_position neighbor_list_function =
	if not (ABBS.is_set abbs_orig start_position) then (-1., -1., -1.) else begin
		let abbs = ABBS.create_exact_copy abbs_orig in
        let (x, y, z) = start_position in
		let (x_sum, y_sum, z_sum) = (ref (float_of_int x), ref (float_of_int y), ref (float_of_int z)) in
		ABBS.unset abbs start_position ;
		let cc_count = ref 1 in
		let to_check = ref [start_position] in
		while List.length !to_check > 0 do
			match !to_check with
				[] -> Printf.printf "\nBug in skeleton.tops.boundary_tips.extract_first_cc: to_check is empty, but has length greater than zero!\n%!" ;
				| (cx, cy, cz)::rest ->
					let rec is_excluded_from ell pos =
						match ell with
							[] -> true ;
							| fr::remaining ->
								if (equal_triplets fr pos) then false else is_excluded_from remaining pos
					in
					let rec unset_list ell =
						match ell with
							[] -> () ;
							| front::remaining -> ABBS.unset abbs front ; unset_list remaining
					in
					let rec add_list ell =
						match ell with
							[] -> () ;
							| (fx, fy, fz)::remaining ->
								x_sum := !x_sum +. (float_of_int fx) ;
								y_sum := !y_sum +. (float_of_int fy) ;
								z_sum := !z_sum +. (float_of_int fz) ;
								add_list remaining
					in
					let set_neighbors = List.filter (ABBS.is_set abbs) (neighbor_list_function (cx, cy, cz)) in
					let excluded_neighbors = List.filter ( is_excluded_from !to_check) set_neighbors in
					unset_list excluded_neighbors ;
					add_list excluded_neighbors ;
					cc_count := !cc_count + (List.length excluded_neighbors) ;
					to_check := List.concat [rest; excluded_neighbors]
		done ;
		(!x_sum/.(float_of_int !cc_count), !y_sum/.(float_of_int !cc_count), !z_sum/.(float_of_int !cc_count))
	end

let separationSq (x_float, y_float, z_float) (x_int, y_int, z_int) =
	let (dx, dy, dz) = (x_float -. float_of_int x_int, y_float -. float_of_int y_int, z_float -. float_of_int z_int) in
	dx*.dx +. dy*.dy +. dz*.dz

let center_of_mass_point abbs_orig start_position neighbor_list_function =
	if not (ABBS.is_set abbs_orig start_position) then (-1, -1, -1) else begin
		let com = center_of_mass_of_connected_component abbs_orig start_position neighbor_list_function in
		let abbs = ABBS.create_exact_copy abbs_orig in
		let best_position = ref start_position in
		ABBS.unset abbs start_position ;
		let best_sepSq = ref (separationSq com start_position) in
		let to_check = ref [start_position] in
		while List.length !to_check > 0 do
			match !to_check with
				[] -> Printf.printf "\nBug in skeleton.tops.boundary_tips.extract_first_cc: to_check is empty, but has length greater than zero!\n%!" ;
				| check_position::rest ->
					let rec is_excluded_from ell (x, y, z) =
						match ell with
							[] -> true ;
							| (fx, fy, fz)::remaining ->
								if (x = fx && y = fy && z = fz) then false else is_excluded_from remaining (x, y, z)
					in
					(*let rec unset_list ell =
						match ell with
							[] -> () ;
							| front::remaining -> ABBS.unset abbs front ; unset_list remaining
					in*)
					let rec check_list ell =
						match ell with
							[] -> () ;
							| front::remaining ->
								let sepSq = separationSq com front in
								if sepSq < !best_sepSq then begin
									best_position := front ;
									best_sepSq := sepSq
								end
					in
					let set_neighbors = List.filter (ABBS.is_set abbs) (neighbor_list_function check_position) in
					let excluded_neighbors = List.filter ( is_excluded_from !to_check) set_neighbors in
					ABBS.unset_list abbs excluded_neighbors ;
					check_list excluded_neighbors ;
					to_check := List.concat [rest; excluded_neighbors]
		done ;
		!best_position
	end

let is_boundary_position abbs position =
	let (x_min, y_min, z_min) = ABBS.get_min_dim abbs in
	let (x_max, y_max, z_max) = ABBS.get_max_dim abbs in
	let (x, y, z) = position in
	x = x_min || x = x_max || y = y_min || y = y_max || z = z_min || z = z_max

let closest_point_to_center com point_list = match point_list with
	[] -> (-1, -1, -1)
	| front::rest ->
		let rec find_closest closest_so_far distanceSq_to_closest_so_far to_be_checked = match to_be_checked with
			[] -> closest_so_far
			| checking::remainder ->
				let distanceSq_to_checking = separationSq com checking in
				if distanceSq_to_checking < distanceSq_to_closest_so_far then
					find_closest checking distanceSq_to_checking remainder
				else
					find_closest closest_so_far distanceSq_to_closest_so_far remainder
		in
		find_closest front (separationSq com front) point_list

let center_of_mass_of_list ell =
	let (x_sum, y_sum, z_sum, count) = (ref 0, ref 0, ref 0, ref 0) in
	let rec sum_all listing = match listing with
		[] -> ()
		| (x, y, z)::rest ->
			x_sum := !x_sum + x ;
			y_sum := !y_sum + y ;
			z_sum := !z_sum + z ;
			count := !count + 1 ;
			sum_all rest
	in
	sum_all ell ;
	((float_of_int !x_sum)/.(float_of_int !count), (float_of_int !y_sum)/.(float_of_int !count), (float_of_int !z_sum)/.(float_of_int !count))


(* connected components should be small *)
let centers_of_connected_components neighbor_list_function sparse_abbs =
	let centers_list = ref [] in
	let cc = ref (extract_first_cc neighbor_list_function sparse_abbs) in
	while List.length !cc > 0 do
		let com = center_of_mass_of_list !cc in
		centers_list := (closest_point_to_center com !cc)::!centers_list ;
		cc := extract_first_cc neighbor_list_function sparse_abbs
	done ;
	!centers_list

let boundary_tips_only abbs =
	let boundary_abbs = ABBS.create_exact_copy abbs in
	let next_set_pos = ref (ABBS.next_set_position_after boundary_abbs (-1, -1, -1)) in
	let (x_init, _, _) = !next_set_pos in
	let at_end = ref (x_init < 0) in
	while (not !at_end) do
		(*let (x, y, z) = !next_set_pos in*)
		if not (is_boundary_position abbs !next_set_pos) then ABBS.unset boundary_abbs !next_set_pos ;
		next_set_pos := ABBS.next_set_position_after boundary_abbs !next_set_pos ;
		let (x_after, _, _) = !next_set_pos in
		at_end := x_after < 0
	done ;
	centers_of_connected_components list_eighteen_neighbors boundary_abbs

let separation (p1x, p1y, p1z) (p2x, p2y, p2z) =
	let sq x = x*x in
	sqrt (float_of_int (sq (p1x - p2x) + sq (p1y - p2y) + sq (p1z - p2z)))  

let critical_graph_distance len = 1.1 *. len +. 2.

let rec does_not_include triplet_list (x, y, z) = match triplet_list with
	[] -> true
	| (fx, fy, fz)::rest ->
		if (fx = x && fy = y && fz = z) then false
		else does_not_include rest (x, y, z)

let rec is_contained_in position_list (x, y, z) = match position_list with
	[] -> false ;
	| (fx, fy, fz)::remaining ->
		if (x = fx && y = fy && z = fz) then true else is_contained_in remaining (x, y, z)

let to_string_triplet (x, y, z) =
	Printf.sprintf "-%d-%d-%d-" x y z

let rec print_triplet_list = function
  [] -> ()
  | front::rest -> print_string (to_string_triplet front); print_string "\n" ; print_triplet_list rest ;;

let prune_tips tips abbs_lcc =
	(* !!!! Might be faster using the ordered set structure
			Will be slightly more accurate if Dijkstra distances are used !!!! *)
	let closeness_radius_Euclid = 7.0 in (* was 7.0 *)
	let elliptical_neighborhood_contains p1 p2 =
		let sep = (separation p1 p2) in
		if sep > closeness_radius_Euclid then false else begin
			let len = critical_graph_distance sep in
			let sum_sep p3 = (separation p1 p3) +. (separation p2 p3) in
			let rec grow_neighborhood in_nh check_neighbors =
				match check_neighbors with
				[] -> false
				| (x, y, z)::rest ->
					let set_neighbors = List.filter (ABBS.is_set abbs_lcc) (list_twenty_six_neighbors (x, y, z)) in
					let excluded_neighbors = List.filter (does_not_include in_nh) set_neighbors in
					let appropriate_neighbors = List.filter (fun x -> sum_sep x <= len) excluded_neighbors in
					if is_contained_in appropriate_neighbors p2 then true
					else grow_neighborhood (List.concat [in_nh; appropriate_neighbors]) (List.concat [rest; appropriate_neighbors])
			in
			grow_neighborhood [p1] [p1]
		end
	in
	let rec prune pruned_tips to_prune = match to_prune with
		[] -> pruned_tips
		| seed_tip::other_tips ->
			if (List.length other_tips) < 1 then seed_tip::pruned_tips else begin
				let rec all_close_tips checked_near_tips near_tips_to_check remaining_far_tips = match near_tips_to_check with
					[] -> (checked_near_tips, remaining_far_tips)
					| front::rest ->
						let (same_tips, far_tips) =
							let rec find_same_tips found_same_tips found_far_tips tips_to_check = 
							match tips_to_check with
								[] -> (found_same_tips, found_far_tips)
								| checking::rest ->
									if (elliptical_neighborhood_contains front checking) then
										find_same_tips (checking::found_same_tips) found_far_tips rest
									else
										find_same_tips found_same_tips (checking::found_far_tips) rest
							in
							find_same_tips [] [] remaining_far_tips
						in
						all_close_tips (front::checked_near_tips) (List.concat [rest; same_tips]) far_tips
				in
				let (tip_bundle, unbundled_tips) = all_close_tips [seed_tip] [seed_tip] other_tips in
				(* Need to choose a more appropriate tip *)
				let chosen_tip = seed_tip in
				prune (chosen_tip::pruned_tips) unbundled_tips
			end
	in
	prune [] tips
(*
let tips_frontier abbs_orig is_lcc_already =
	(* MUST be the largest connected component *)
	let abbs = if is_lcc_already then
		ABBS.create_exact_copy abbs_orig
	else largest_connected_component abbs_orig in
	let center_point = center_of_mass_point abbs (ABBS.next_set_position_after abbs (-1, -1, -1)) list_twenty_six_neighbors in 
	ABBS.unset abbs center_point ;
	if (ABBS.degree abbs center_point) = 0 then List.concat [[center_point]; (boundary_tips_only abbs_orig)] else begin
		(* Could definitely used a more efficient updateable ordered list *)
		let known_shortest_paths = ref [(0., center_point)] in
		let tips_internal = ref [] in
		let rec remove_unneeded_shortest_paths needed_paths known_paths = match known_paths with
			[] -> needed_paths
			| (front_dist, front_pos)::rest ->
				if ABBS.degree abbs front_pos = 0 then remove_unneeded_shortest_paths needed_paths rest
				else remove_unneeded_shortest_paths ((front_dist, front_pos)::needed_paths) rest
		in
		let find_nearest_frontier known_shortest_paths =
			let error_best = ((-1, -1, -1), -1.) in
			let find_nearest_frontier_neighbor position =
				let (x, y, z) = position in
				let set_neighbors = ref (List.filter (ABBS.is_set abbs) (list_six_neighbors (x, y, z))) in
				let additional_dist = ref 1. in
				if List.length !set_neighbors = 0 then begin
					set_neighbors := List.filter (ABBS.is_set abbs) (list_twelve_neighbors (x, y, z)) ;
					additional_dist := sqrt 2.
				end ;
				if List.length !set_neighbors = 0 then begin
					set_neighbors := List.filter (ABBS.is_set abbs) (list_eight_neighbors (x, y, z)) ;
					additional_dist := sqrt 3.
				end ;
				match !set_neighbors with
					[] -> error_best
					| lucky_front::unlucky_rest -> (lucky_front, !additional_dist)
			in
			let rec searcher (best_frontier_pos, best_frontier_dist) remaining = match remaining with
				[] -> (best_frontier_pos, best_frontier_dist)
				| (front_dist, front_pos)::rest ->
					let (nn, nn_dist) = find_nearest_frontier_neighbor front_pos in
					if front_dist +. nn_dist < best_frontier_dist then
						searcher (nn, front_dist +. nn_dist) rest
					else
						searcher (best_frontier_pos, best_frontier_dist) rest
			in
			match known_shortest_paths with
				[] -> error_best
				| (very_first_dist, very_first_position)::all_others ->
					let (nn, nn_dist) = find_nearest_frontier_neighbor very_first_position in
					if nn_dist < 0. then error_best
					else searcher (nn, nn_dist +. very_first_dist) all_others
		in
		let counter = ref 0 in
		while List.length !known_shortest_paths > 0 do
			let (best_front_pos, best_front_dist) = find_nearest_frontier !known_shortest_paths in
			if best_front_dist >= 0. then begin
				if (ABBS.degree abbs best_front_pos = 0  && not (is_boundary_position abbs best_front_pos)) then tips_internal := best_front_pos::!tips_internal ;
				ABBS.unset abbs best_front_pos ;
				known_shortest_paths := remove_unneeded_shortest_paths [] ((best_front_dist, best_front_pos)::!known_shortest_paths)
			end else Printf.printf "\n Error in skeleton.tips_frontier!\n%!" ;
			if (!counter mod 200 = 0) then Printf.printf "\n skeleton.tips_frontier progress tips.length = %d, abbs.count = %d :\n%s\n%!" (List.length !tips_internal) (ABBS.count abbs) (ABBS.ascii_slice 0 7 abbs 10 10 0. 0.04 "z") ;
			counter := !counter + 1
		done ;
		List.concat [!tips_internal; (boundary_tips_only abbs_orig)]
	end
*)

let rec is_away_from_by position away_list len = match away_list with
	[] -> true
	| front::rest ->
		if separation position front > len then
			is_away_from_by position rest len
		else false

let tips_local_a abbs_orig is_lcc_already =
	(* MUST be the largest connected component *)
	let abbs = if is_lcc_already then
		ABBS.create_exact_copy abbs_orig
	else largest_connected_component abbs_orig in
	let center_point = center_of_mass_point abbs (ABBS.next_set_position_after abbs (-1, -1, -1)) list_twenty_six_neighbors in 
	ABBS.unset abbs center_point ;
	let all_tips = ref [] in
	(*let counter = ref 0 in*)
	let rec erode_to_tips removed_points = match removed_points with
		[] -> ()
		| (x, y, z)::rest ->
			let set_neighbors = List.filter (ABBS.is_set abbs) (list_twenty_six_neighbors (x, y, z)) in
			let rec unset_and_identify set_nh = match set_nh with
				[] -> ()
				| position::remaining ->
					if (ABBS.degree abbs position = 0 && (not (is_boundary_position abbs position)) && (is_away_from_by position !all_tips 2.1)) then all_tips := position::!all_tips ;
					ABBS.unset abbs position ;
					unset_and_identify remaining
			in
			unset_and_identify set_neighbors ;
			(*if (!counter mod 200 = 0) then Printf.printf "\n skeleton.tips_frontier progress tips.length = %d, abbs.count = %d :\n%s\n%!" (List.length !all_tips) (ABBS.count abbs) (ABBS.ascii_slice 0 7 abbs 10 10 0. 0.04 "z") ;
			counter := !counter + 1 ;*)
			erode_to_tips (List.concat [rest; set_neighbors])
	in
	erode_to_tips [center_point] ;
	List.concat [!all_tips; (boundary_tips_only abbs_orig)]
	(*prune_tips (List.concat [!all_tips; (boundary_tips_only abbs_orig)]) abbs_orig*)
	(*1. erode from central point: remove points next to points that were removed last iteration
	2. when a single point is reached, set it as a tip
	3. prune maxima that are in the same tip!*)


(*
let tips_local abbs_orig is_lcc_already =
	let abbs = if is_lcc_already then
		ABBS.create_exact_copy abbs_orig
	else largest_connected_component abbs_orig in
	let (x_min, y_min, z_min) = ABBS.get_min_dim abbs in
	let (x_max, y_max, z_max) = ABBS.get_max_dim abbs in
	(*let closest_point_to_center com point_list = match point_list with
		[] -> (-1, -1, -1)
		| front::rest ->
			let rec find_closest closest_so_far distanceSq_to_closest_so_far to_be_checked = match to_be_checked with
				[] -> closest_so_far
				| checking::remainder ->
					let distanceSq_to_checking = separationSq com checking in
					if distanceSq_to_checking < distanceSq_to_closest_so_far then
						find_closest checking distanceSq_to_checking remainder
					else
						find_closest closest_so_far distanceSq_to_closest_so_far remainder
			in
			find_closest front (separationSq com front) point_list
	in*)
	(* Find the boundary tip points *)
	let boundary_tips =
		let boundary_abbs = ABBS.create_exact_copy abbs in
		let next_set_pos = ref (ABBS.next_set_position_after boundary_abbs (-1, -1, -1)) in
		let (x_init, _, _) = !next_set_pos in
		let at_end = ref (x_init < 0) in
		while (not !at_end) do
			let (x, y, z) = !next_set_pos in
			if (x_min < x && x < x_max && y_min < y && y < y_max && z_min < z && z < z_max) then begin ABBS.unset boundary_abbs !next_set_pos
			end ;
			next_set_pos := ABBS.next_set_position_after boundary_abbs !next_set_pos ;
			let (x_after, _, _) = !next_set_pos in
			at_end := x_after < 0
		done ;
		centers_of_connected_components list_eighteen_neighbors boundary_abbs
	in
	(* Find the internal tip points *)
	let internal_tips =
		let internal_abbs = ABBS.create_exact_copy abbs in
		let next_set_pos = ref (ABBS.next_set_position_after internal_abbs (-1, -1, -1)) in
		let (x_init, _, _) = !next_set_pos in
		let at_end = ref (x_init < 0) in
		while (not !at_end) do
			let (x, y, z) = !next_set_pos in
			(* Exclude the boundary points *)
			if (x_min = x || x = x_max || y_min = y || y = y_max || z_min = z || z = z_max) then ABBS.unset internal_abbs !next_set_pos ;
			(* Exclude points that do not have the desired degree *)
			let k = ABBS.degree abbs (x, y, z) in
			if (k < 1 || 8 < k) then ABBS.unset internal_abbs !next_set_pos ;
			next_set_pos := ABBS.next_set_position_after internal_abbs !next_set_pos ;
			let (x_after, _, _) = !next_set_pos in
			at_end := x_after < 0
		done ;
		centers_of_connected_components list_twenty_six_neighbors internal_abbs
	in
	List.concat [boundary_tips; internal_tips]
*)

(*
let tips abbs_orig is_lcc_already =
	(* MUST be the largest connected component *)
	let abbs = if is_lcc_already then
		ABBS.create_exact_copy abbs_orig
	else largest_connected_component abbs_orig in
	let (x_min, y_min, z_min) = ABBS.get_min_dim abbs in
	let (x_max, y_max, z_max) = ABBS.get_max_dim abbs in
	let centers_of_connected_components neighbor_list_function sparse_abbs =
		let centers_list = ref [] in
		let cc = ref (extract_first_cc neighbor_list_function sparse_abbs) in
		while List.length !cc > 0 do
			(*Printf.printf "\n length of cc = %d\n%!" (List.length !cc) ;*)
			let centermost_position ell =
				let sum_positions =
					let rec helper (x_sum, y_sum, z_sum) positions = match positions with
						[] -> (x_sum, y_sum, z_sum)
						| (x, y, z)::rest -> helper (x_sum + x, y_sum + y, z_sum + z) rest
					in helper (0, 0, 0) ell
				in
				let (sx, sy, sz) = sum_positions in
				let num_positions = float_of_int (List.length ell) in
				let center_of_mass = ((float_of_int sx)/.num_positions, (float_of_int sy)/.num_positions, (float_of_int sz)/.num_positions) in
				closest_point_to_center center_of_mass !cc
			in
			centers_list := (centermost_position !cc)::!centers_list ;
			cc := extract_first_cc neighbor_list_function sparse_abbs
		done ;
		!centers_list
	in
	(* Find the boundary tip points *)
	let boundary_tips =
		let boundary_abbs = ABBS.create_exact_copy abbs in
		let next_set_pos = ref (ABBS.next_set_position_after boundary_abbs (-1, -1, -1)) in
		let (x_init, _, _) = !next_set_pos in
		let at_end = ref (x_init < 0) in
		while (not !at_end) do
			let (x, y, z) = !next_set_pos in
			if (x_min < x && x < x_max && y_min < y && y < y_max && z_min < z && z < z_max) then begin ABBS.unset boundary_abbs !next_set_pos
			end ;
			next_set_pos := ABBS.next_set_position_after boundary_abbs !next_set_pos ;
			let (x_after, _, _) = !next_set_pos in
			at_end := x_after < 0
		done ;
		centers_of_connected_components list_eighteen_neighbors boundary_abbs
	in
	(* Find the internal tip points *)
	let internal_tips =
		let internal_abbs = ABBS.create_exact_copy abbs in
		let next_set_pos = ref (ABBS.next_set_position_after internal_abbs (-1, -1, -1)) in
		let (x_init, _, _) = !next_set_pos in
		let at_end = ref (x_init < 0) in
		while (not !at_end) do
			let (x, y, z) = !next_set_pos in
			(* Exclude the boundary points *)
			if (x_min = x || x = x_max || y_min = y || y = y_max || z_min = z || z = z_max) then ABBS.unset internal_abbs !next_set_pos ;
			(* Exclude points that do not have the desired degree *)
			let k = ABBS.degree abbs (x, y, z) in
			if (k < 1 || 4 < k) then ABBS.unset internal_abbs !next_set_pos ;
			next_set_pos := ABBS.next_set_position_after internal_abbs !next_set_pos ;
			let (x_after, _, _) = !next_set_pos in
			at_end := x_after < 0
		done ;
		centers_of_connected_components list_twenty_six_neighbors internal_abbs
	in
	List.concat [boundary_tips; internal_tips]
*)

let list_is_single_connected_component ell abbs_skel = match ell with
	[] -> true
	| seed::unexplored ->
		let rec find_neighbors nh found_n found_outsiders to_check = match to_check with
			[] -> (found_n, found_outsiders)
			| front::rest ->
				if (is_contained_in nh front) then
					find_neighbors nh (front::found_n) found_outsiders rest
				else
					find_neighbors nh found_n (front::found_outsiders) rest
		in
		let rec explore to_check outsiders = match to_check with
			[] -> List.length outsiders = 0
			| (cx, cy, cz)::rest ->
				let nh = list_twenty_six_neighbors (cx, cy, cz) in
				let (in_set, still_missing) = find_neighbors nh [] [] outsiders in
				explore (List.concat [rest; in_set]) still_missing
		in
		explore [seed] unexplored
(*let list_is_single_connected_component ell abbs_skel = match ell with
	[] -> true
	| front::rest ->
		let rec are_overlapping list_one list_two = match list_one with
				[] -> false
				| first::remaining ->
					if (is_contained_in list_two first) then true
					else are_overlapping remaining list_two
		in
		let rec find_point_in_that_connects_with unconnected_points unknown_connectivity known_cc = match unknown_connectivity with
			[] -> ((-1, -1, -1), unconnected_points)
			| (fx, fy, fz)::remaining ->
				let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors fx fy fz) in
				if are_overlapping set_neighbors known_cc then
					((fx, fy, fz), List.concat [unconnected_points; remaining])
				else
					find_point_in_that_connects_with ((fx, fy, fz)::unconnected_points) remaining known_cc
		in
		let rec grow_cc unknown_connectivity known_cc =
			if (List.length unknown_connectivity) < 1 then true
			else begin
				let ((mx, my, mz), unconnected_points) = find_point_in_that_connects_with [] unknown_connectivity known_cc in
				if (mx < 0) then false
				else
					grow_cc unconnected_points ((mx, my, mz)::known_cc)
			end
		in
		grow_cc rest [front]*)

(*
(* small_points_list assumed to be SMALL, not much more than four points *)
let segregated_connected_neighborhood components small_points_list =
	let rec screen_candidates check_against to_check good_ones bad_ones = match to_check with
		[] -> (good_ones, bad_ones)
		| front::rest ->
			if is_contained_in check_against front then
				screen_candidates check_against rest (front::good_ones) bad_ones
			else
				screen_candidates check_against rest good_ones (front::bad_ones)
	in
	let rec expand_cc cc to_check candidates = match to_check with
		[] -> (cc, candidates)
		| front::rest ->
			if is_contained_in cc front then
				expand_cc cc rest candidates
			else begin
				(to_include, still_to_check) = screen_candidates (list_twenty_six_neighbors front) candidates [] [] in
				expand_cc (front:cc) (List.concat [to_check; to_include]) still_to_check
			end
	in	
	let rec form_ccs ccs points_remaining = match points_remaining with
		[] -> ccs
		| front::rest ->
			let (cc, leftovers) = expand_cc front rest in
			form_ccs (cc::ccs) leftovers
	in
	form_ccs (ref []) small_points_list
	

let is_removable_global abbs_skel tips suspect_point =
	if is_contained_in tips suspect_point then false
	else if ABBS.degree abbs_skel suspect_point < 2 then true
	else
	let checked_abbs = ABBS.create_blank_copy abbs_skel in
	let rec expand_cc frontier = match frontier with
		[] -> ()
		| front::rest ->
			let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors front) in
			let new_frontier = List.filter (not ABBS.is_set 
	in
	expand_cc (List.hd (List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors suspect_point))) ;
	let rec count_included_tips to_check count = match to_check with
		[] -> count
		| front::rest ->
			if (ABBS.is_set checked_abbs
	in
	let inluded_tips_count = count_included_tips tips 0 in
	IF THERE ARE EXACTLY TWO CCs: included_tips_count = 0 || included_tips_count = List.length tips*)

(* criticallity should propagate: fix by visitation order (frontier, not by label) *)
let remove_globally tips abbs_skel =
	let is_critical_abbs = ABBS.create_blank_copy abbs_skel in
	let is_extraneous_abbs = ABBS.create_blank_copy abbs_skel in
	let abbs_skel_orig = ABBS.create_exact_copy abbs_skel in
	let next_skel_position = ref (ABBS.next_set_position_after abbs_skel (-1, -1, -1)) in
	let (x_init, _, _) = !next_skel_position in
	let covered_everything = ref (x_init < 0) in
	
	while not !covered_everything do
		let global_criticality_search suspect = (* modifies either is_critical_abbs or is_extraneous_abbs *)
			let tip_count = List.length tips in
			let tip_count_in_cc seed =
				let abbs_cc = ABBS.create_blank_copy abbs_skel in
				let rec grow_cc to_check = match to_check with
					[] -> ()
					| front::rest ->
						if ABBS.is_set abbs_cc front then grow_cc rest
						else begin
							ABBS.set abbs_cc front ;
							let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors front) in
							grow_cc (List.concat [rest; set_neighbors])
						end
				in
				grow_cc [seed] ;
				let rec count_tips to_check tip_count = match to_check with
					[] -> tip_count
					| front::rest ->
						if ABBS.is_set abbs_cc front then
							count_tips rest (tip_count + 1)
						else
							count_tips rest tip_count
				in
				count_tips tips 0
			in
			let rec is_critical_by_cc_tips to_check = match to_check with
				[] -> false (* no tips in any neighboring component -- it's an island *)
				| front::rest ->
					let tip_count_cc = tip_count_in_cc front in
					if tip_count_cc = tip_count then false
					else if tip_count_cc > 0 then true
					else is_critical_by_cc_tips rest
			in
			let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors suspect) in
			ABBS.unset abbs_skel suspect ;
			if is_critical_by_cc_tips set_neighbors then
				ABBS.set is_critical_abbs suspect
			else
				ABBS.set is_extraneous_abbs suspect ;
			ABBS.set abbs_skel suspect ;
		in
		let check_degree_two abbs_x =
			(* see if either neighbor in abbs has degree two; if one does, inherit criticality, otherwise vet for removal, globally *)
			let set_neighbors = List.filter (ABBS.is_set abbs_x) (list_twenty_six_neighbors !next_skel_position) in
			let neighbor1 = (List.hd set_neighbors) in
			let neighbor2 = (List.hd (List.tl set_neighbors)) in
			if ABBS.degree abbs_x neighbor1 = 2 then begin
				if ABBS.is_set is_critical_abbs neighbor1 then ABBS.set is_critical_abbs !next_skel_position
				else if ABBS.is_set is_extraneous_abbs neighbor1 then ABBS.set is_extraneous_abbs !next_skel_position
			end ;
			if ABBS.degree abbs_x neighbor2 = 2 then begin
				if ABBS.is_set is_critical_abbs neighbor2 then ABBS.set is_critical_abbs !next_skel_position
				else if ABBS.is_set is_extraneous_abbs neighbor2 then ABBS.set is_extraneous_abbs !next_skel_position
			end; 
			if not (ABBS.is_set is_critical_abbs !next_skel_position) && not (ABBS.is_set is_extraneous_abbs !next_skel_position) then
				global_criticality_search !next_skel_position
		in
		if ABBS.degree abbs_skel !next_skel_position = 2 then
			check_degree_two abbs_skel
		else if ABBS.degree abbs_skel_orig !next_skel_position = 2 then
			check_degree_two abbs_skel_orig
		else
			global_criticality_search !next_skel_position ;
		if ABBS.is_set is_extraneous_abbs !next_skel_position then
			ABBS.unset abbs_skel !next_skel_position ;
		next_skel_position := ABBS.next_set_position_after abbs_skel !next_skel_position ;
		let (x, _, _) = !next_skel_position in
		covered_everything := x < 0
	done

(* set tips as critical; criticallity propogates quickly along strands; check points with new algorithm*)


(* start at tips and work in so that the cc that needs to be searched always shrinks *)
(* what if there is STILL a pesky wayward strand? *)
let remove_globally_old2 tips abbs_skel =
	let abbs_to_check = ABBS.create_exact_copy abbs_skel in
	let rec determine_tip_seeds tips_to_check tip_seeds = match tips_to_check with
		[] -> tip_seeds
		| front::rest ->
			ABBS.unset abbs_to_check front ;
			if (ABBS.degree abbs_skel front = 1) then
				determine_tip_seeds rest (front::tip_seeds)
			else
				determine_tip_seeds rest tip_seeds
	in
	let remaining_to_check_count = ref (ABBS.count abbs_to_check) in
	let original_rough_skel_count = ABBS.count abbs_skel in
	let running_skel_count = ref original_rough_skel_count in
	(* check all set_neighbors to see if each if critical:
				those of degree 1 in abbs_to_check inherit criticallity from seed
				those of higher degree must be checked:
					start at seed and see if full cc (less a point) is returned
						{starting with seed might help keep the search space low since it goes out toward tips, so that if there is no longer a single cc then that will be discovered quickly; this is helpful because there are probably few points that are left to remove, so there will be few points that provoke a full coverage of the skeleton}
				return the list of neighbors that will be seeds*)
	let rec check_neighborhood seed nh = match nh with
		[] -> ()
		| front::rest ->
			(*Printf.printf "\n check_neighborhood: fsd = %d, fcd = %d ssd = %d, scd = %d ss? =  %B\n\t\t time: %f%!" (ABBS.degree abbs_skel front) (ABBS.degree abbs_to_check front) (ABBS.degree abbs_skel seed) (ABBS.degree abbs_to_check seed) (ABBS.is_set abbs_skel seed) (Sys.time());*)
			if (ABBS.degree abbs_skel front = 1) then begin (* tips will never be checked here *)
				ABBS.unset abbs_skel front ;
				running_skel_count := !running_skel_count - 1 ;
				Printf.printf "0%!"
			end else if (ABBS.degree abbs_skel front = 2 && ABBS.degree abbs_skel seed < 3) then begin
				if not (ABBS.is_set abbs_skel seed) then begin
					ABBS.unset abbs_skel front ;
					running_skel_count := !running_skel_count - 1 ;
					Printf.printf ".%!"
				end
			end	else begin
				ABBS.unset abbs_skel front ;
				let (cc_size, next_cc_point) = count_connected_component_size seed abbs_skel list_twenty_six_neighbors in
				if (cc_size = (!running_skel_count - 1)) then begin
					running_skel_count := !running_skel_count - 1 ;
					Printf.printf "*%!"
				end else
					ABBS.set abbs_skel front
			end ;
			ABBS.unset abbs_to_check front ;
			remaining_to_check_count := !remaining_to_check_count - 1
	in
	let rec erode_skeleton seeds = match seeds with
		[] -> ()
		| front::rest ->
			(* removed bits may need to be checked... if not (ABBS.is_set abbs_to_check front) then
				erode_skeleton rest ;*)
			let set_neighbors = List.filter (ABBS.is_set abbs_to_check) (list_twenty_six_neighbors_farthest_first front) in
			check_neighborhood front set_neighbors ;
			(*if ((original_rough_skel_count - !remaining_to_check_count) mod 10 = 0) then begin
				Printf.printf "{%4.3f%%}%!" (100.*.(float_of_int (original_rough_skel_count - !remaining_to_check_count))/.(float_of_int original_rough_skel_count)) ;
				if ((original_rough_skel_count - !remaining_to_check_count) mod 1000 = 0) then
					Printf.printf "[%f]%!" (Sys.time())
			end ;*)
			erode_skeleton (List.concat [rest; set_neighbors])
	in
	erode_skeleton (determine_tip_seeds tips []) ;
	Printf.printf "\n remove_globally removed %d points\n%!" (original_rough_skel_count - !running_skel_count) 
(*
let remove_globally_old tips abbs_skel =
	(*criticality should propagate along segments*)
	(* remove_if_does_not_disconnect_global needs to be more efficient!!!! *)
	let cannot_be_removed_abbs = ABBS.create_blank_copy abbs_skel in
	let remove_if_does_not_disconnect_global (x, y, z) original_size =
		if not (ABBS.is_set abbs_skel (x, y, z)) then false
		else if is_contained_in tips (x, y, z) then false
		else if (ABBS.is_set cannot_be_removed_abbs (x, y, z)) then false
		else begin
			let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors (x, y, z)) in
			let is_critical = ref false in
			if (List.length set_neighbors) = 2 then begin (* if not critical, then will check globally *)
				let (neighbor1, neighbor2) = (List.hd set_neighbors, List.hd (List.tl set_neighbors) ) in
				let degree1 = ABBS.degree abbs_skel neighbor1 in
				let degree2 = ABBS.degree abbs_skel neighbor2 in
				let is_critical1 = ABBS.is_set cannot_be_removed_abbs neighbor1 in
				let is_critical2 = ABBS.is_set cannot_be_removed_abbs neighbor2 in
				(* not quite right: if ((is_critical1 && (degree2 = 2)) || (is_critical2 && (degree1 = 2))) then*)
				if((degree1 = 2) && (degree2 = 2) && (is_critical1 || is_critical2)) then begin
					(*doesn't save much time
					ABBS.set cannot_be_removed_abbs (x, y, z) ;
					ABBS.set cannot_be_removed_abbs neighbor1 ;
					ABBS.set cannot_be_removed_abbs neighbor2 ;*)
					is_critical := true
				end
			end ;
			if !is_critical then false else begin
				ABBS.unset abbs_skel (x, y, z) ;
				let count_some_connected_component_with_tips =
					let cc = ABBS.create_blank_copy abbs_skel in
					let check_neighbors to_check = match to_check with
						[] -> 0
						| front::rest ->
					in
					check_neighbors set_neighbors
				in
				if count_some_connected_component_with_tips = (List.length tips) then true

				else begin
					ABBS.set abbs_skel (x, y, z) ;
					false
				end
				(*match set_neighbors with
					[] -> false
					| test_position::rest ->*)
						(*let (original_size, next_component_start) = (count_connected_component_size test_position abbs_skel list_twenty_six_neighbors) in*)
						
						(*let (modified_size, modified_component_start) = (count_connected_component_size test_position abbs_skel list_twenty_six_neighbors) in
						if modified_size = original_size - 1 then true*)
			end
		end
	in
	let skel_count = ABBS.count abbs_skel in
	Printf.printf "\n Removing globally (%d remaining points)...\n%!" skel_count ;
	let removed_count = ref 1 in
	let covered_count = ref 0 in
	ABBS.set_list cannot_be_removed_abbs tips ;
	(* Should only take one pass where removed_count exceeds zero... NO! might take more than one...*)
	while !removed_count > 0 do
		let next_skel_position = ref (ABBS.next_set_position_after abbs_skel (-1, -1, -1)) in
		let (x_init, _, _) = !next_skel_position in
		let covered_everything = ref (x_init < 0) in
		removed_count := 0 ;
		while not !covered_everything do
			if (remove_if_does_not_disconnect_global !next_skel_position (skel_count - !removed_count)) then
				removed_count := !removed_count + 1
			else
				ABBS.set cannot_be_removed_abbs !next_skel_position ;
			covered_count := !covered_count + 1 ;
			(*Printf.printf "{%4.3f%%}%!" (100.*.(float_of_int !covered_count)/.(float_of_int skel_count)) ;*)
			next_skel_position := ABBS.next_set_position_after abbs_skel !next_skel_position ;
			let (x, _, _) = !next_skel_position in
			covered_everything := x < 0
		done ;
	done ;
	Printf.printf "\n Globally skeletonized; removed %d points.\n%!" !removed_count
*)
let dump_obj filename obj = let ch = open_out_bin filename in 
  output_value ch obj ; close_out ch ;;

let skeleton abbs_orig tips is_lcc_already =
	(* MUST be the largest connected component *)
	let abbs = if is_lcc_already then
		ABBS.create_exact_copy abbs_orig
	else largest_connected_component abbs_orig in
	let abbs_skel = ABBS.create_exact_copy abbs in
	(* Fill in holes? *)
	let removing_degree = ref 9 in
	let lcc_count = (ABBS.count abbs) in
	let left_to_remove = ref lcc_count in
	while !removing_degree < 25 do
		let prev_skel = ABBS.create_exact_copy abbs_skel in
		let removed_this_pass = ref 0 in
		let next_bit = ref (ABBS.next_set_position_after abbs_skel (-1, -1, -1)) in
		let (x_init, _, _) = !next_bit in
		let covered_everything = ref (x_init < 0) in
		while not !covered_everything do
			if ((ABBS.degree prev_skel !next_bit <= !removing_degree) && not (is_contained_in tips !next_bit)) then begin
				let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_one_hundred_twenty_four_neighbors !next_bit) in
				if (list_is_single_connected_component set_neighbors abbs_skel) then begin
					ABBS.unset abbs_skel !next_bit ;
					removed_this_pass := !removed_this_pass + 1
				end
			end ;
			next_bit := ABBS.next_set_position_after abbs_skel !next_bit ;
			let (x, _, _) = !next_bit in
			covered_everything := x < 0
		done ;
		left_to_remove := !left_to_remove - !removed_this_pass ;
		(*
		Printf.printf "\n removing_degree = %d, removed_this_pass = %d, %4.1f%%\n%!" !removing_degree !removed_this_pass (100.*.(float_of_int (lcc_count - !left_to_remove))/.(float_of_int lcc_count)) ;
		*)
		let critical_remaining_fraction_to_keep_things_moving = 0.01*.2./.27. in (* decreasing takes longer but	should give cleaner skeletons; doing the opposite should result in the opposite *)
		let minimal_overall_fraction_to_keep_things_moving = 0.0001 in
		if (float_of_int !removed_this_pass > critical_remaining_fraction_to_keep_things_moving*.(float_of_int !left_to_remove) && float_of_int !removed_this_pass > minimal_overall_fraction_to_keep_things_moving*.(float_of_int lcc_count)) then(*0 then*)
			removing_degree := max 9 (!removing_degree - 2)
		else
			removing_degree := !removing_degree + 2 ;
		if !removed_this_pass > 0 then
			removing_degree := min !removing_degree 23
	done ;
	(* Remove pesky strands *)
	let removed_this_pass = ref 1 in
	while !removed_this_pass > 0 do
		removed_this_pass := 0 ;
		let next_bit = ref (ABBS.next_set_position_after abbs_skel (-1, -1, -1)) in
		let (x_init, _, _) = !next_bit in
		let covered_everything = ref (x_init < 0) in
		while not !covered_everything do
			if (ABBS.degree abbs_skel !next_bit < 2 && does_not_include tips !next_bit) then begin
				ABBS.unset abbs_skel !next_bit ;
				removed_this_pass := !removed_this_pass + 1
			end ;
			next_bit := ABBS.next_set_position_after abbs_skel !next_bit ;
			let (x, _, _) = !next_bit in
			covered_everything := x < 0
		done
	done ;
	(*dump_obj "./rough_local_skel.abbs" abbs_skel ;*)
	remove_globally tips abbs_skel ;
	(*remove_globally tips abbs_skel ;*)
	abbs_skel

(*let skeleton_sheath abbs_orig tips is_lcc_already =
	(* MUST be the largest connected component *)
	let abbs = if is_lcc_already then
		ABBS.create_exact_copy abbs_orig
	else largest_connected_component abbs_orig in
	let abbs_skel = ABBS.create_exact_copy abbs in
	let abbs_sheath = ABBS.create 1000000. (ABBS.get_min_dim abbs) (ABBS.get_max_dim abbs) in
	let set_sheath =
		ABBS.unset_all abbs_sheath ;
		let next_bit = ref (ABBS.next_set_position_after abbs_skel (-1, -1, -1)) in
		let (x_init, _, _) = !next_bit in
		let covered_everything = ref (x_init < 0) in
		while not !covered_everything do
			if (ABBS.degree abbs_skel !next_bit) < 18 then ABBS.set abbs_sheath !next_bit ;
			next_bit := ABBS.next_set_position_after abbs_skel !next_bit ;
			let (x, _, _) = !next_bit in
			covered_everything := x < 0
		done
	in
	let remove_if_does_not_disconnect_local (x, y, z) =
		if not (ABBS.is_set abbs_skel (x, y, z)) then false
		else if is_contained_in tips (x, y, z) then false
		else begin
			let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_one_hundred_twenty_four_neighbors (x, y, z)) in
			if (list_is_single_connected_component set_neighbors abbs_skel) then begin
				ABBS.unset abbs_sheath (x, y, z) ;
				ABBS.unset abbs_skel (x, y, z) ;
				true
			end else false
		end
	in
	let remove_if_does_not_disconnect_global (x, y, z) =
		if not (ABBS.is_set abbs_skel (x, y, z)) then false
		else if is_contained_in tips (x, y, z) then false
		else begin
			let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors (x, y, z)) in
			match set_neighbors with
				[] -> false
				| test_position::rest ->
					let (original_size, next_component_start) = (count_connected_component_size test_position abbs_skel list_twenty_six_neighbors) in
					ABBS.unset abbs_skel (x, y, z) ;
					let (modified_size, modified_component_start) = (count_connected_component_size test_position abbs_skel list_twenty_six_neighbors) in
					if modified_size = original_size - 1 then true
					else begin
						ABBS.set abbs_skel (x, y, z) ;
						false
					end
		end
		
	in
	Printf.printf "\n Removing locally...\n%!" ;
	let removed_count = ref 1 in
	while !removed_count > 0 do
		set_sheath ;
		let next_sheath_position = ref (ABBS.next_set_position_after abbs_sheath (-1, -1, -1)) in
		let (x_init, _, _) = !next_sheath_position in
		let covered_everything = ref (x_init < 0) in
		removed_count := 0 ;
		while not !covered_everything do
			if (remove_if_does_not_disconnect_local !next_sheath_position) then removed_count := !removed_count + 1 ;
			next_sheath_position := ABBS.next_set_position_after abbs_sheath !next_sheath_position ;
			let (x, _, _) = !next_sheath_position in
			covered_everything := x < 0
		done
	done ;
	Printf.printf "\n After removing locally:\n%s\n%!" (ABBS.ascii_slice_with_tips 0 7 abbs_skel tips 25 25 0. 1. "z") ;
	Printf.printf "\n Removing globally...\n%!" ;
	removed_count := 1 ;
	while !removed_count > 0 do
		let next_skel_position = ref (ABBS.next_set_position_after abbs_skel (-1, -1, -1)) in
		let (x_init, _, _) = !next_skel_position in
		let covered_everything = ref (x_init < 0) in
		removed_count := 0 ;
		while not !covered_everything do
			if (remove_if_does_not_disconnect_global !next_skel_position) then removed_count := !removed_count + 1 ;
			next_skel_position := ABBS.next_set_position_after abbs_skel !next_skel_position ;
			let (x, _, _) = !next_skel_position in
			covered_everything := x < 0
		done
	done ;
	Printf.printf "\n Finished removing.\n%!" ;
	(*Not written yet: smooth_skeleton abbs_orig abbs_skel*)
	abbs_skel
*)
(*
(* DOES NOT WORK --v *)
let skeleton_without_tips abbs_orig is_lcc_already =
	(* MUST be the largest connected component *)
	let abbs = if is_lcc_already then
		ABBS.create_exact_copy abbs_orig
	else largest_connected_component abbs_orig in
	let abbs_skel = ABBS.create_exact_copy abbs in
	let abbs_sheath = ABBS.create 1000000. (ABBS.get_min_dim abbs) (ABBS.get_max_dim abbs) in
	let set_sheath =
		ABBS.unset_all abbs_sheath ;
		let next_bit = ref (ABBS.next_set_position_after abbs_skel (-1, -1, -1)) in
		let (x_init, _, _) = !next_bit in
		let covered_everything = ref (x_init < 0) in
		while not !covered_everything do
			let k = ABBS.degree abbs_skel !next_bit in
			if 0 < k && k < 26 then ABBS.set abbs_sheath !next_bit ;
			next_bit := ABBS.next_set_position_after abbs_skel !next_bit ;
			let (x, _, _) = !next_bit in
			covered_everything := x < 0
		done
	in
	let remove_if_does_not_disconnect_local (x, y, z) =
		if not (ABBS.is_set abbs_skel (x, y, z)) then false
		(*else if is_contained_in tips (x, y, z) then false*)
		else begin
			let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_one_hundred_twenty_four_neighbors (x, y, z)) in
			if (list_is_single_connected_component set_neighbors abbs_skel) then begin
				ABBS.unset abbs_sheath (x, y, z) ;
				ABBS.unset abbs_skel (x, y, z) ;
				true
			end else false
		end
	in
	let remove_if_does_not_disconnect_global (x, y, z) =
		if not (ABBS.is_set abbs_skel (x, y, z)) then false
		(*else if is_contained_in tips (x, y, z) then false*)
		else if ABBS.degree abbs_skel (x, y, z) < 3 then false
		else begin
			let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors (x, y, z)) in
			match set_neighbors with
				[] -> false
				| test_position::rest ->
					let (original_size, next_component_start) = (count_connected_component_size test_position abbs_skel list_twenty_six_neighbors) in
					ABBS.unset abbs_skel (x, y, z) ;
					let (modified_size, modified_component_start) = (count_connected_component_size test_position abbs_skel list_twenty_six_neighbors) in
					if modified_size = original_size - 1 then true
					else begin
						ABBS.set abbs_skel (x, y, z) ;
						false
					end
		end
		
	in
	let removed_count = ref 1 in
	while !removed_count > 0 do
		set_sheath ;
		let next_sheath_position = ref (ABBS.next_set_position_after abbs_sheath (-1, -1, -1)) in
		let (x_init, _, _) = !next_sheath_position in
		let covered_everything = ref (x_init < 0) in
		removed_count := 0 ;
		while not !covered_everything do
			if (remove_if_does_not_disconnect_local !next_sheath_position) then removed_count := !removed_count + 1 ;
			next_sheath_position := ABBS.next_set_position_after abbs_sheath !next_sheath_position ;
			let (x, _, _) = !next_sheath_position in
			covered_everything := x < 0
		done ;
		Printf.printf "\n removed %d locally, left with:\n%s%!" !removed_count (ABBS.ascii_slice 0 7 abbs_skel  50 48 0. 1. "z")
	done ;
	removed_count := 1 ;
	while !removed_count > 0 do
		let next_skel_position = ref (ABBS.next_set_position_after abbs_skel (-1, -1, -1)) in
		let (x_init, _, _) = !next_skel_position in
		let covered_everything = ref (x_init < 0) in
		removed_count := 0 ;
		while not !covered_everything do
			if (remove_if_does_not_disconnect_global !next_skel_position) then removed_count := !removed_count + 1 ;
			next_skel_position := ABBS.next_set_position_after abbs_skel !next_skel_position ;
			let (x, _, _) = !next_skel_position in
			covered_everything := x < 0
		done ;
		Printf.printf "\n removed %d globally, left with:\n%s%!" !removed_count (ABBS.ascii_slice 0 7 abbs_skel  50 48 0. 1. "z")
	done ;
	abbs_skel
*)
(*
let is_local_skeleton_degree_extremum comparison (x, y, z) abbs_skel =
	if not (ABBS.is_set abbs_skel (x, y, z)) then false else begin
		let center_degree = ABBS.degree (x, y, z) abbs_skel in
		let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors x y z) in
		let rec is_loc_deg_ex nh = match nh with
			[] -> true
			| front::rest -> if comparison (ABBS.degree front abbs_skel) center_degree then false
				else is_loc_deg_ex rest
		in
		is_loc_deg_ex set_neighbors
	end


let is_unique_localo_skeleton_degree_max position abbs_skel =
	is_local_skeleton_degree_extremum (>=) psition abbs_skel

(* May not be the unique minimum *)
let is_local_skeleton_degree_min position abbs_skel =
	is_local_skeleton_degree_extremum (<) position abbs_skel
*)
let has_neighbor_of_same_degree (x, y, z) abbs_skel =
	if not (ABBS.is_set abbs_skel (x, y, z)) then false else begin
		let center_degree = ABBS.degree abbs_skel (x, y, z) in
		if center_degree = 1 then false else begin (* to address tips and a cc of two nodes*)
			let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors (x, y, z)) in
			let rec is_same_deg nh = match nh with
				[] -> false
				| front::rest -> if (ABBS.degree abbs_skel front) = center_degree then true
					else is_same_deg rest
			in
			is_same_deg set_neighbors
		end
	end

(* seg_seed must have a degree of exactly 2 *)
let get_vertebrae seg_seed abbs_skel =
	let vertebrae = ref [seg_seed] in
	let (x, y, z) = seg_seed in
	let seed_neighbors = List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors (x, y, z)) in
	let rec add_vertebra ultimate penultimate to_front =
		vertebrae := if to_front then (ultimate::!vertebrae) else List.concat [!vertebrae;[ultimate]] ;
		let (ux, uy, uz) = ultimate in
		let (px, py, pz) = penultimate in
		if (ABBS.degree abbs_skel ultimate) = 2 then begin
			let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors (ux, uy, uz)) in
			let (ax, ay, az) = List.hd set_neighbors in
			let (bx, by, bz) = List.nth set_neighbors 1 in
			let next_position = if (ax = px && ay = py && az == pz) then (bx, by, bz) else (ax, ay, az) in
			add_vertebra next_position ultimate to_front
		end
	in
	add_vertebra (List.hd seed_neighbors) seg_seed true ;
	add_vertebra (List.nth seed_neighbors 1) seg_seed false ;
	!vertebrae

let rec get_adjacent_segments_seeds seg_verts abbs_skel =
	let rec explore_branch_points seeds to_check already_checked = match to_check with
		[] -> seeds
		| front::rest -> if is_contained_in already_checked front then
				explore_branch_points seeds rest already_checked
			else if (ABBS.degree abbs_skel front) = 2 then
				explore_branch_points (front::seeds) rest (front::already_checked)
			else begin
				let (x, y, z) = front in
				let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors (x, y, z)) in
				let unexplored_neighbors = List.filter (does_not_include already_checked) set_neighbors in
				explore_branch_points seeds (List.concat [rest; unexplored_neighbors]) (front::already_checked)
			end
	in
	let first_two_last_two ell =
		if (List.length ell) < 5 then ell else begin
			let len = List.length ell in
			[(List.hd ell); (List.nth ell 1); (List.nth ell (len - 2)); (List.nth ell (len - 1))]
		end
	in
	explore_branch_points [] [(List.hd seg_verts); (List.nth seg_verts ((List.length seg_verts) - 1))] (first_two_last_two seg_verts)

(* Since the branch points can be included in the list of vertebrae for a single segment, there may be repetition in the list of all vertebrae that will never exceed listing a single branch point (of which there are at most two) more than its branching order. *)
let get_adjacent_segments_vertebrae seg_verts abbs_skel =
	(*Printf.printf "gasv%!" ;*)
	let rec all_vertebrae_from_seeds verts remaining_seeds = match remaining_seeds with
		[] -> verts
		| front::rest ->
			(*Printf.printf "(%d)%!" (List.length remaining_seeds) ;*)
			all_vertebrae_from_seeds (List.concat [verts; (get_vertebrae front abbs_skel)]) rest
	in
	let adjacent_segment_seeds = get_adjacent_segments_seeds seg_verts abbs_skel in
	(*Printf.printf "ass(%d) " (List.length adjacent_segment_seeds) ; *)
	all_vertebrae_from_seeds [] adjacent_segment_seeds

class seg = fun t1 t2 v ell r b -> object
	val tip1:int*int*int = t1
	val tip2:int*int*int = t2
	val vol:float = v
	val len:float = ell
	val rad:float = r
	val backbone:(int*int*int) list = b
	method get_tip1 = tip1
	method get_tip2 = tip2
	method get_vol = vol
	method get_len = len
	method get_rad = rad
	method get_backbone = backbone
	method short_summary = let (ax, ay, az) = tip1 in
							let (bx, by, bz) = tip2 in
							Printf.sprintf "(%d,%d,%d)-(%d,%d,%d);vol=%f;len=%f;rad=%f" ax ay az bx by bz vol len rad
end

let print_tree root_index parents children (sega:seg array) =
	let visited_indices = BatBitSet.create (Array.length sega) in
	let rec print_subtree queued_indices previous_prints = match queued_indices with
	[] -> previous_prints
	| front::rest ->
		if BatBitSet.mem visited_indices front then begin
			Printf.printf "\n skeleton.print_tree().print_subtree():\n\t Already visited seg %d in the tree!\n%!" front ;
			print_subtree rest previous_prints
		end else begin
			BatBitSet.set visited_indices front ;
			let (ax, ay, az) = sega.(front)#get_tip1 in
			let (bx, by, bz) = sega.(front)#get_tip2 in
			let seg_name = Printf.sprintf "(%d, %d, %d)-(%d, %d, %d)" ax ay az bx by bz in
			let par_name = if parents.(front) >= 0 then begin
				let (cx, cy, cz) =  sega.(parents.(front))#get_tip1 in
				let (dx, dy, dz) =  sega.(parents.(front))#get_tip2 in
				Printf.sprintf "(%d, %d, %d)-(%d, %d, %d)" cx cy cz dx dy dz
			end else "NA" in
			let beta = if parents.(front) >= 0 then (Printf.sprintf "%f" (sega.(front)#get_rad/.sega.(parents.(front))#get_rad)) else "NA" in
			let gamma = if parents.(front) >= 0 then (Printf.sprintf "%f" (sega.(front)#get_len/.sega.(parents.(front))#get_len)) else "NA" in
			let this_print = Printf.sprintf "%d\t%s\t%f\t%f\t%f\t<voxc>\t<defc>\t <col>\t<tips>\t%s\t%s\t%s\t%d\t<q>\t<s>\n" front seg_name sega.(front)#get_len sega.(front)#get_vol sega.(front)#get_rad par_name beta gamma (List.length children.(front))
			in
			print_subtree (List.concat [rest; children.(front)]) (sprintf "%s%s" previous_prints this_print)
		end
	in
	Printf.sprintf "tag\tname\tlen\tvol\trad\tvoxc\tdefc\tcol\ttips\tparent\tbeta\tgamma\tnchild\tq\ts\n%s" (print_subtree [root_index] "")

let segment_length vertebrae abbs_skel =
	let rec walk_length distance_walked left_to_walk = match left_to_walk with
		[] -> distance_walked
		| front::rest ->
			if List.length rest < 1 then distance_walked else begin
				let deg = ABBS.degree abbs_skel front in
				let sep = if deg > 2 && not (has_neighbor_of_same_degree front abbs_skel) then 1./.(float_of_int deg) else separation front (List.hd rest) in
				walk_length (distance_walked +. sep) rest
			end
	in
	walk_length 0. vertebrae

let segment_volume vertebrae adjacent_vertebrae abbs abbs_skel abbs_explored =
	let rec vertebrae_volume vol to_check = match to_check with
		[] -> vol
		| front::rest ->
			let deg = ABBS.degree abbs_skel front in
			let vert_vox_vol = if (deg < 3 || has_neighbor_of_same_degree front abbs_skel) then 1. else 1./.(float_of_int deg) in
			vertebrae_volume (vol +. vert_vox_vol) rest
	in
	let rec find_shortest_distance_to_in shortest_so_far position to_check = match to_check with
		[] -> shortest_so_far
		| front::rest ->
			let dist = separation front position in
			if (dist < shortest_so_far || shortest_so_far < 0.) then find_shortest_distance_to_in dist position rest
			else find_shortest_distance_to_in shortest_so_far position rest
	in
	let rec check_voxels vol to_check = match to_check with
		[] -> vol
		| front::rest ->
			let dist_in = find_shortest_distance_to_in (-1.) front vertebrae in
			let dist_out = find_shortest_distance_to_in (-1.) front adjacent_vertebrae in
			if dist_in <= dist_out (*+. 0.5*) then begin
				(*Printf.printf "\n(%d,%d,%d) has dist_in=%f \t dist_out=%f%!" x y z dist_in dist_out ;*)
				let set_neighbors = List.filter (ABBS.is_set abbs) (list_twenty_six_neighbors front) in
				let unexplored_neighbors = List.filter (does_not_include to_check) (List.filter (ABBS.is_not_set abbs_explored) set_neighbors) in
				let vert_vox_vol = if ABBS.is_set abbs_explored front then 0.
					else begin ABBS.set abbs_explored front; 1. end in
				check_voxels (vol +. vert_vox_vol) (List.concat [rest; unexplored_neighbors])
			end else
				check_voxels vol rest
	in
	(*Printf.printf "\n skeleton.segment_volume.vertebrae_volume = %f has %d voxels\n%!" (vertebrae_volume 0. vertebrae) (List.length vertebrae);*)
	check_voxels (vertebrae_volume 0. vertebrae) vertebrae

let analyze_segment seg_seed abbs_lcc abbs_skel abbs_skel_unexplored abbs_explored =
	let vertebrae = get_vertebrae seg_seed abbs_skel in
	let a = List.hd vertebrae in
	let b = List.nth vertebrae ((List.length vertebrae) - 1) in
	(*Printf.printf "5%!" ;*)
	ABBS.unset_list abbs_skel_unexplored vertebrae ;
	ABBS.set_list abbs_explored vertebrae ;
	let adjacent_vertebrae = get_adjacent_segments_vertebrae vertebrae abbs_skel in
	(*Printf.printf "9%!" ;*)
	let v = segment_volume vertebrae adjacent_vertebrae abbs_lcc abbs_skel abbs_explored in
	let ell = segment_length vertebrae abbs_skel in
	let rad = sqrt (v/.ell/.acos(-1.)) in
	new seg a b v ell rad vertebrae
	(* analyze segment: analyze volume and radius and points within radius of vertebrae to determine if there is a deformity or if a tip was missed !!!! *)

let analyze_skeleton abbs_lcc abbs_skel =
	let segmented_skel = ref [] in
	let abbs_skel_unexplored = ABBS.create_exact_copy abbs_skel in
	let abbs_explored = ABBS.create_blank_copy abbs_skel in
	(*let abbs_explored = ABBS.create_exact_copy abbs_skel in
	ABBS.unset_all abbs_explored ;*)
	let next_skel = ref (ABBS.next_set_position_after abbs_skel_unexplored (-1, -1, -1)) in
	let (x_init, _, _) = !next_skel in
	let covered_skel = ref (x_init < 0) in
	while not !covered_skel do
		if (ABBS.degree abbs_skel !next_skel) = 2 then begin
			(*let (x, y, z) = !next_skel in
			Printf.printf "\nnext_skel=%d %d %d\n%!" x y z ;*)
			let s = analyze_segment !next_skel abbs_lcc abbs_skel abbs_skel_unexplored abbs_explored in
			segmented_skel := s::!segmented_skel
		end ;
		next_skel := ABBS.next_set_position_after abbs_skel_unexplored !next_skel ;
		let (x, _, _) = !next_skel in
		covered_skel := x < 0
	done ;
	!segmented_skel

let identify_bad_tips segments tips =
	if List.length segments < 2 then [] else begin
		let rec id_bad_tips bad_tips (segs:(seg list)) = match segs with
			[] -> bad_tips
			| front::rest ->
				(* A valid segment will have a length that is (at least nearly) greater than its "diameter" *)
				if front#get_len < 1.5*.front#get_rad then begin
					if is_contained_in tips front#get_tip1 then
						id_bad_tips (front#get_tip1::bad_tips) rest
					else if is_contained_in tips front#get_tip2 then
						id_bad_tips (front#get_tip2::bad_tips) rest
					else
						id_bad_tips bad_tips rest
				end else
					id_bad_tips bad_tips rest
		in
		id_bad_tips [] segments
	end

let skeleton_final abbs_skel segments tips =
	let bad_tips = identify_bad_tips segments tips in
	let good_tips = List.filter (does_not_include bad_tips) tips in
	let abbs_skel_final = skeleton abbs_skel good_tips true in
	(abbs_skel_final, good_tips)

let create_seg_array segs =
	let sega = Array.make (List.length segs) (List.hd segs) in
	let rec populate_sega index remaining_segs = match remaining_segs with
		[] -> sega
		| front::rest -> Array.set sega index front ; populate_sega (index + 1) rest
	in
	populate_sega 0 segs

let choose_root sega tips =
	let best_index = ref (-1) in
	for i = 0 to (Array.length sega) - 1 do
		if is_contained_in tips sega.(i)#get_tip1 || is_contained_in tips sega.(i)#get_tip2 then begin
			if !best_index < 0 then
				best_index := i
			else if sega.(!best_index)#get_rad < sega.(i)#get_rad then
				best_index := i ;
		end
	done ;
	!best_index

let parents_and_children (segs:seg list) abbs_skel tips = 
	let sega = create_seg_array segs in
	let parents = Array.make (Array.length sega) (-1) in
	let children = Array.make (Array.length sega) [] in
	let rec find_branching_point_set branching_points to_check = match to_check with
		[] -> branching_points
		| front::rest ->
			let set_neighbors = List.filter (ABBS.is_set abbs_skel) (list_twenty_six_neighbors front) in
			let branch_neighbors = List.filter (fun x -> ABBS.degree abbs_skel x > 2) set_neighbors in
			let unexplored_neighbors = List.filter (does_not_include branching_points) branch_neighbors in
			find_branching_point_set (List.concat [branching_points; unexplored_neighbors]) (List.concat [rest; unexplored_neighbors])
	in
	let downstream_end parent_index downstream_index =
		if parent_index < 0 then if is_contained_in tips sega.(downstream_index)#get_tip1 then sega.(downstream_index)#get_tip2 else sega.(downstream_index)#get_tip1
		else begin
			(*let min_sep_one = min (separation sega.(parent_index)#get_tip1 sega.(downstream_index)#get_tip1) (separation sega.(parent_index)#get_tip2 sega.(downstream_index)#get_tip1) in
			let min_sep_two = min (separation sega.(parent_index)#get_tip1 sega.(downstream_index)#get_tip2) (separation sega.(parent_index)#get_tip2 sega.(downstream_index)#get_tip2) in
			if min_sep_one < min_sep_two then sega.(downstream_index)#get_tip2 else sega.(downstream_index)#get_tip1*)
			let parent_branching_point_sets = List.concat [(find_branching_point_set [sega.(parent_index)#get_tip1] [sega.(parent_index)#get_tip1]);(find_branching_point_set [sega.(parent_index)#get_tip2] [sega.(parent_index)#get_tip2])] in
			if is_contained_in parent_branching_point_sets sega.(downstream_index)#get_tip1 then sega.(downstream_index)#get_tip2 else sega.(downstream_index)#get_tip1
		end
	in
	let visited_parents = BatBitSet.create (List.length segs) in
	let rec add_children childless_parent_indices = match childless_parent_indices with
		[] -> ()
		| front::rest ->
			if BatBitSet.mem visited_parents front then begin
				Printf.printf "\n Skeleton.parents_and_children().add_children():\n\t Already visited %d as a parent!\n%!" front ;
				add_children rest
			end else begin
				BatBitSet.set visited_parents front ;
				let downstream_point = downstream_end parents.(front) front in
				let branching_point_set = find_branching_point_set [downstream_point] [downstream_point] in
				let added_children = ref [] in
				if does_not_include tips downstream_point then begin
					for i = 0 to (Array.length sega) - 1 do
						if not (i = front) && (is_contained_in branching_point_set sega.(i)#get_tip1 || is_contained_in branching_point_set sega.(i)#get_tip2) then begin
							Printf.printf "Adding %d %s-%s (%d%s, %d%s) as child to %d at %s\n%!" i (to_string_triplet sega.(i)#get_tip1) (to_string_triplet sega.(i)#get_tip2) (ABBS.degree abbs_skel sega.(i)#get_tip1) (if is_contained_in tips sega.(i)#get_tip1 then "t" else "b") (ABBS.degree abbs_skel sega.(i)#get_tip2) (if is_contained_in tips sega.(i)#get_tip2 then "t" else "b") front (to_string_triplet downstream_point) ;
							added_children := List.concat [!added_children; [i]] ; (* Adding to end to keep a little order *)
							Array.set parents i front
						end
					done ;
				end ;
				Array.set children front !added_children ;
				add_children (List.concat [rest; !added_children])
			end
	in
	let root_index = choose_root sega tips in
	add_children [root_index] ;
	(root_index, sega, parents, children)
