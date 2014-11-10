open Scanf ;;
open Printf ;;
module ABBS = ArrayBatBitSet ;;

(* modified from http://rosettacode.org/wiki/Bitmap/Read_a_PPM_file#OCaml *)
let read_p3 ~filename =
	(*Printf.printf "\nReading in %s\n" filename ;*)
  let ic = open_in filename in
  let line = input_line ic in
  if line <> "P3" then invalid_arg (Printf.sprintf "%s is not a P3 ppm file" filename) ;
  let line = input_line ic in
  let line =
    try if line.[0] = '#'  (* skip comments *)
    then input_line ic
    else line
    with _ -> line
  in
  let width, height =
    Scanf.sscanf line "%d %d" (fun w h -> (w, h))
  in
  let line = input_line ic in
  if line <> "255" then invalid_arg "not a 8 bit depth image";
  let r_channel = Array.make_matrix width height 0
  and g_channel = Array.make_matrix width height 0
  and b_channel = Array.make_matrix width height 0
  in
  for y = 0 to pred height do
    for x = 0 to pred width do
      r_channel.(x).(y) <- int_of_string (input_line ic);
      g_channel.(x).(y) <- int_of_string (input_line ic);
      b_channel.(x).(y) <- int_of_string (input_line ic);
    done;
  done;
  close_in ic;
   (width,
   height,
   r_channel,
   g_channel,
   b_channel)

let rgb2intensity r g b =
	((float_of_int r) +. (float_of_int g) +. (float_of_int b))/.3.

let set_by_threshold abbs im z thresh =
	(*Printf.printf "[%d]" z ;*)
	(*Printf.printf "\n p32abbs.set_by_threshold(): setting for z = %d\n" z ;*)
	let (width, height, r_channel, g_channel, b_channel) = im in
	for x = 0 to width - 1 do
		for y = 0 to height - 1 do
			let scaled_thresh = thresh*.255. in
			if scaled_thresh <= (rgb2intensity r_channel.(x).(y) g_channel.(x).(y) b_channel.(x).(y)) then ABBS.set abbs (x, y, z)
		done
	done

let progress_bar cur goal incs =
	let filler x =
		let filler_str = ref "" in
		for i = 0 to x - 1 do filler_str := !filler_str ^ "-" done ;
		!filler_str
	in
	let proper_incs = if incs > goal then goal else incs in
	if cur = 0 then Printf.printf "\n|%s|\n %!" (filler proper_incs) ;
	let bin_size = (float_of_int goal)/.(float_of_int proper_incs) in
	if floor( (float_of_int cur)/.bin_size ) > floor ((float_of_int (cur - 1))/.bin_size ) then Printf.printf "|%!" ;
	if cur = goal then Printf.printf "\n%!"

let observed_max start finish path image_extension =
	let filename index = sprintf "%s/%05d%s" path index image_extension in
	let get_im index = read_p3 (filename index) in
	let observed_slice_max im =
		let (width, height, r_channel, g_channel, b_channel) = im in
		let max_intensity = ref 0.0 in
		for x = 0 to width - 1 do
			for y = 0 to height - 1 do
				let intens = rgb2intensity r_channel.(x).(y) g_channel.(x).(y) b_channel.(x).(y) in
				if !max_intensity < intens then max_intensity := intens
			done
		done ;
		!max_intensity
	in
	let max_intensity = ref 0.0 in
	for i = start to finish do
		let im = get_im i in
		let slice_intensity = observed_slice_max im in
		if !max_intensity < slice_intensity then max_intensity := slice_intensity;
		progress_bar (i - start) (finish - start + 1) 64
	done ;
	!max_intensity
	
let load start finish path thresh image_extension max_size_in_GB =
	let filename index = sprintf "%s/%05d%s" path index image_extension in
	let get_im index = read_p3 (filename index) in
	let init_im = get_im start in
	let (width, height, r, g, b) = init_im in
	let abbs = ABBS.create max_size_in_GB (0, 0, 0) (width - 1, height - 1, finish - start)  in
	let scaled_thresh = thresh*.(observed_max start finish path image_extension)/.255. in
	set_by_threshold abbs init_im 0 scaled_thresh ;
	progress_bar 0 (finish - start + 1) 64 ;
	for i = start + 1 to finish do
		let im = get_im i in
			set_by_threshold abbs im (i - start) scaled_thresh ;
			progress_bar (i - start) (finish - start + 1) 64
	done ;
	ABBS.create_tight_copy abbs

