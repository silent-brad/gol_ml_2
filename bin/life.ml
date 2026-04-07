(*
 * Conway's Game of Life in OCaml
 * Rules:
 *   1. Any live cell with fewer than two Alive neighbors dies (underpopulation)
 *   2. Any live cell with two or three Alive neighbors Alives on
 *   3. Any live cell with more than three Alive neighbors dies (overpopulation)
 *   4. Any dead cell with exactly three Alive neighbors becomes Alive (reproduction)
 *)

open Graphics

let cell_size = 10

type cell = Alive | Dead

let count_live_neighbors grid row col =
  let get_cell grid row col =
    try List.nth (List.nth grid row) col
    with Failure _ | Invalid_argument _ -> Dead
  in
  let offsets = [ -1; 0; 1 ] in
  List.fold_left
    (fun acc dr ->
      List.fold_left
        (fun acc dc ->
          if dr = 0 && dc = 0 then acc
          else if get_cell grid (row + dr) (col + dc) = Alive then acc + 1
          else acc)
        acc offsets)
    0 offsets

let next_grid grid =
  List.mapi
    (fun row_i row ->
      List.mapi
        (fun col_i cell ->
          let n = count_live_neighbors grid row_i col_i in
          match cell with
          | Alive -> if n = 2 || n = 3 then Alive else Dead
          | Dead -> if n = 3 then Alive else Dead)
        row)
    grid

let random_grid ~width ~height =
  List.init height (fun _ ->
      List.init width (fun _ -> if Random.int 2 = 1 then Alive else Dead))

let grid_height grid = List.length grid

let draw_grid grid =
  let height = grid_height grid in
  let win_h = size_y () in
  let bar_y = height * cell_size in
  let random_color () =
    let colors = [| red; green; blue; yellow; cyan; magenta |] in
    colors.(Random.int (Array.length colors))
  in
  clear_graph ();
  List.iteri
    (fun row_i row ->
      List.iteri
        (fun col_i cell ->
          let x = col_i * cell_size in
          let y = (height - 1 - row_i) * cell_size in
          match cell with
          | Alive ->
              set_color (random_color ());
              fill_rect x y cell_size cell_size
          | Dead ->
              set_color black;
              fill_rect x y cell_size cell_size)
        row)
    grid;
  let alive_count =
    List.length (List.filter (fun c -> c = Alive) (List.concat grid))
  in
  set_color black;
  fill_rect 0 bar_y (size_x ()) (win_h - bar_y);
  set_color white;
  moveto 5 (bar_y + 5);
  draw_string (Printf.sprintf "Alive Cells: %d" alive_count);
  synchronize ()

let dims () =
  let w = size_x () / cell_size in
  let h = (size_y () - 30) / cell_size in
  (w, h)

let resize_grid grid ~width ~height =
  let old_h = grid_height grid in
  let old_w = match grid with [] -> 0 | r :: _ -> List.length r in
  if width = old_w && height = old_h then grid
  else
    List.init height (fun r ->
        List.init width (fun c ->
            if r < old_h && c < old_w then List.nth (List.nth grid r) c
            else if Random.int 2 = 1 then Alive
            else Dead))

let rec loop grid =
  draw_grid grid;
  Unix.sleepf 0.1;
  if key_pressed () then
    let _ = read_key () in
    ()
  else
    let width, height = dims () in
    let grid = resize_grid grid ~width ~height in
    loop (next_grid grid)

let () =
  Random.self_init ();
  open_graph "";
  set_window_title "Conway's Game of Life";
  auto_synchronize false;
  set_font "serif-14";
  let width, height = dims () in
  set_color black;
  fill_rect 0 0 (size_x ()) (size_y ());
  loop (random_grid ~width ~height);
  close_graph ()
