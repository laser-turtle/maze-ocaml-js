open! Js_of_ocaml
open Js_utils
open Core_kernel
module DL = Doubly_linked

type direction =
    | North
    | East
    | South
    | West

type possible_node = {
    coord : int * int;
    elt : (int * int) DL.Elt.t;
}

type node = {
    mutable missing_walls : direction list;
    mutable visited : bool;
    mutable active : bool;
}

type maze = {
    nodes : node array;
    width : int;
    height : int;
}

let make_maze width height =
    let nodes = Array.init (width*height) ~f:(fun _ -> {
        visited = false;
        active = false;
        missing_walls = [];
    }) in
    { nodes; width; height }
;;

let in_bounds maze (x, y) =
    x >= 0 && y >= 0 &&
    x < maze.width && y < maze.height
;;

let get_node maze (x, y as coord) =
    if in_bounds maze coord then
        Some maze.nodes.(y*maze.width + x)
    else
        None
;;

let get_node_exn maze coord =
    Option.value_exn (get_node maze coord)
;;

let offset_of_direction = function
    | North -> ( 0,  1)
    | East  -> ( 1,  0)
    | South -> ( 0, -1)
    | West  -> (-1,  0)
;;

let opposite_direction = function
    | North -> South
    | East -> West
    | South -> North
    | West -> East
;;

let get_coord_offset (x, y) dir =
    let dx, dy = offset_of_direction dir in
    x+dx, y+dy
;;

let neighbor maze coord dir =
    let coord = get_coord_offset coord dir in
    get_node maze coord
;;

let nth dl_list index =
    DL.findi_elt dl_list ~f:(fun idx _ -> idx = index)
    |> function
       | None -> failwith "nth out of range"
       | Some (_, elt) -> elt
;;

let has_been_visited =
    Option.value_map ~default:true ~f:(fun node -> node.visited)
;;

let all_directions = [North; East; South; West]

let (%) = Fn.compose

let get_valid_neighbors maze coord =
    all_directions
    |> List.map ~f:(fun dir -> neighbor maze coord dir, dir)
    |> List.filter ~f:(Option.is_some % fst)
    |> List.filter ~f:(not % has_been_visited % fst)
    |> List.map ~f:(fun (n, d) -> Option.value_exn n, d)
;;

type pick_result = No_Neighbors
                 | Picked of (int * int)

let pick_neighbor maze coord =
    let this_node = get_node_exn maze coord in
    let neighbors = get_valid_neighbors maze coord in
    match List.random_element neighbors with
    | None -> No_Neighbors
    | Some (neighbor, dir) ->
        this_node.visited <- true;
        neighbor.visited <- true;
        this_node.missing_walls <- dir :: this_node.missing_walls;
        neighbor.missing_walls <- (opposite_direction dir) :: neighbor.missing_walls;
        Picked (get_coord_offset coord dir)
;;

type algorithm = BreadthFirstSearch
               | DepthFirstSearch
               | Randomized

type algorithm_rec = {
    insert : (int * int) -> unit;
    pick   : unit -> (int * int) DL.Elt.t;
}

let get_algorithm lst = function
    | BreadthFirstSearch -> {
        insert = (fun coord -> DL.insert_first lst coord |> ignore);
        pick = (fun () -> Option.value_exn DL.(last_elt lst));
    }
    | DepthFirstSearch ->  {
        insert = (fun coord -> DL.insert_first lst coord |> ignore);
        pick = (fun () -> Option.value_exn DL.(first_elt lst));
    }
    | Randomized -> {
        insert = (fun coord -> DL.insert_first lst coord |> ignore);
        pick = (fun () ->
            if Random.bool() then (
                nth lst Random.(int DL.(length lst))
            ) else (
                Option.value_exn DL.(first_elt lst)
            )
        )
    }
;;

type maze_bundle = {
    maze : maze;
    one_step : unit -> bool;
}

let make_maze_path algorithm w h =
    let maze = make_maze w h in
    let active = DL.create () in
    let start_coord = Random.int w, Random.int h in

    let algorithm = get_algorithm active algorithm in

    let insert_active coord =
        let node = get_node_exn maze coord in
        node.active <- true;
        algorithm.insert coord;
    in

    let remove_active elt =
        let node = get_node_exn maze elt.coord in
        node.active <- false;
        DL.remove active elt.elt
    in

    let pick_random () =
        let elt = algorithm.pick() in
        { coord=DL.Elt.value elt; elt }
    in

    insert_active start_coord;

    let one_step () =
        if not DL.(is_empty active) then (
            let elt = pick_random() in
            begin match pick_neighbor maze elt.coord with
            | No_Neighbors -> remove_active elt
            | Picked coord -> insert_active coord
            end;
            DL.is_empty active
        ) else true
    in
    { maze; one_step }
;;

let draw_maze (module G : Dpi_graphics.Make_sig) maze =
    let dx = Int.of_float (float (G.size_x()-2) /. float maze.width) in
    let dy = Int.of_float (float (G.size_y()-2) /. float maze.height) in
    G.clear_graph();
    let draw_wall x y =
        let x = x*dx + 1
        and y = y*dy + 1 in
        G.set_color G.black;
        function
        | North -> G.draw_line x (y+dy) dx 0
        | South -> G.draw_line x y dx 0
        | East  -> G.draw_line (x+dx) y 0 dy
        | West  -> G.draw_line x y 0 dy
    in
    let set_node_color node : unit =
        let set_color r g b = G.set_color G.(rgb r g b) in
        match node.visited, node.active with
        | false, true  -> set_color 255 255 128
        | false, false -> set_color 80  80  80
        | true, true   -> set_color 255 200 200
        | true, false  -> set_color 255 255 255
    in
    for x=0 to maze.width-1 do
        for y=0 to maze.height-1 do
            let node = get_node_exn maze (x, y) in
            set_node_color node;
            G.fill_rect (x*dx+1) (y*dy+1) dx dy;
            let get_opposite_walls walls =
                let dir_exists = List.mem walls ~equal:(=) in
                List.filter ~f:(not % dir_exists) all_directions
            in
            node.missing_walls
            |> get_opposite_walls
            |> List.iter ~f:(draw_wall x y)
        done;
    done;
;;

let setup_canvas w h =
    let module Html = Dom_html in
    let elem = getElementById_exn "maze" in
    let canvas = Dom_html.createCanvas Html.window##.document in
    Dom.appendChild elem canvas;
    let dpi = set_canvas_size canvas w h in
    let module G = Dpi_graphics.Make(struct let dpi = dpi end) in
    G.open_canvas canvas;
    (module G : Dpi_graphics.Make_sig)
;;

let _ =
    let module Html = Dom_html in
    Html.window##.onload := Html.handler (fun _ ->
        (* For some reason jsoo_runtime is not added globally
         * in debug mode uncomment this if building in debug *)
        (*expose_jsoo_runtime();*)

        let start_btn = getElementById_exn "start-btn" in
        let reset_btn = getElementById_exn "reset-btn" in
        let select_algorithm =
            "select-algorithm"
            |> getElementById_exn
            |> Html.tagged
            |> function
               | Select sel -> sel
               | _ -> failwith "Expected select element"
        in

        let select_items = [
            (DepthFirstSearch, "Depth First Search");
            (BreadthFirstSearch, "Breadth First Search");
            (Randomized, "Randomized");
        ] in

        List.iteri ~f:(fun index (_, name) ->
            let opt = Html.createOption Html.document in
            opt##setAttribute Js.(string "value") Js.(string Int.(to_string index));
            setTextContent opt name;
            Dom.appendChild select_algorithm opt;
        ) select_items;

        let w, h = 402, 402 in
        let (module G) = setup_canvas w h in
        let maze_w, maze_h = 20, 20 in
        let running = ref false in
        let default_algorithm = List.(hd_exn select_items |> fst) in
        let bundle = ref (make_maze_path default_algorithm maze_w maze_h) in
        let timeout_id = ref None in
        let rec loop () =
            let res = !bundle.one_step() in
            draw_maze (module G) !bundle.maze;
            if not res && !running then (
                timeout_id := Some (Html.setTimeout loop 16.);
            ) else if res then (
                start_btn##click;
            );
        in

        draw_maze (module G) !bundle.maze;

        let clear_timeout() =
            match !timeout_id with
            | None -> ()
            | Some id -> Html.clearTimeout id
        in

        start_btn##.onclick := Html.handler (fun _ ->
            if !running then (
                running := false;
                clear_timeout();
                setTextContent start_btn "Start";
            ) else (
                running := true;
                setTextContent start_btn "Stop";
                Html.setTimeout loop 0. |> ignore;
            );
            Js._false
        );

        reset_btn##.onclick := Html.handler (fun _ ->
            running := false;
            clear_timeout();
            setTextContent start_btn "Start";
            let algorithm =
                select_algorithm##.selectedIndex
                |> List.nth_exn select_items
                |> fst
            in
            bundle := make_maze_path algorithm maze_w maze_h;
            draw_maze (module G) !bundle.maze;
            Js._false
        );

        Js._false
    );
