open Pyth

[@@@warning "-26"]

let (%) f g = fun x -> f (g x)

let print fn =
    print_string "[";
    List.iter (fun (x, y, z) ->
        Printf.sprintf "(%d, %d, %d), " x y z
        |> print_string) (fn 1000);
    print_endline "]"

let exec fn = ignore (fn 1000)

let pt = Landmark.register "pyth"
let pm = Landmark.register "pyth_monad"
let pl = Landmark.register "pyth_lazymonad"
let ps = Landmark.register "pyth_seq"
let pi = Landmark.register "pyth_iterator"
let pf = Landmark.register "pyth_fusionstream"

let runbench wrapper f lm =
    Landmark.enter lm;
        wrapper f;
    Landmark.exit lm

let () =
    if not (Landmark.profiling()) then begin
        Landmark.start_profiling () ~profiling_options:
            { Landmark.default_options with 
              allocated_bytes = true
            ; sys_time = true }
    end

[@@@warning ""]

let () =
    runbench exec pyth pt;
    runbench exec pyth_monad pm;
    runbench exec pyth_lazymonad pl;
    runbench exec pyth_seq ps;
    runbench exec pyth_iterator pi;
    runbench exec pyth_fusionstream pf;
