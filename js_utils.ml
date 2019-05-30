open Js_of_ocaml

let some = function
    | Some s -> s
    | None -> failwith "Expected some"
;;

let setTextContent element str =
    element##.textContent := Js.(string str) |> Js.Opt.return;
;;

let getElementById_exn id =
    Dom_html.document##getElementById Js.(string id)
    |> Js.Opt.to_option
    |> some
;;

let set_canvas_size canvas width height =
    let dpi : float = Js.Unsafe.js_expr "window.devicePixelRatio || 1" in
    canvas##.width  := Float.(of_int width *. dpi |> to_int);
    canvas##.height := Float.(of_int height *. dpi |> to_int);
    canvas##.style##.width := Js.string (string_of_int width ^ "px");
    canvas##.style##.height := Js.string (string_of_int height ^ "px");
    dpi
;;

let expose_jsoo_runtime () =
    let var = Js.Unsafe.variable "jsoo_runtime" in
    let keys = Js.(object_keys var) |> Js.to_array in
    Array.iter (fun key ->
        let value = Js.Unsafe.get var key in
        Js.Unsafe.set Js.Unsafe.global key value
    ) keys
;;

