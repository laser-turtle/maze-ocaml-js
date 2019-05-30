module type Dpi = sig
    val dpi : float
end

module Make(Dpi : Dpi) = struct
    open Core_kernel

    module G = Graphics_js

    include G

    let scale v = Int.of_float (float v *. Dpi.dpi)

    let scale_f f x y w h =
        f (scale x) (scale y) (scale w) (scale h)

    let draw_rect = scale_f G.draw_rect

    let fill_rect = scale_f G.fill_rect

    let draw_line x y dx dy =
        let draw_line x y dx dy =
            G.moveto x y;
            G.rlineto dx dy;
        in
        scale_f draw_line x y dx dy
    ;;

    let size_x () = Int.of_float (float (G.size_x()) /. Dpi.dpi)
    let size_y () = Int.of_float (float (G.size_y()) /. Dpi.dpi)
end

module type Make_sig = module type of Make(struct let dpi=0. end)
