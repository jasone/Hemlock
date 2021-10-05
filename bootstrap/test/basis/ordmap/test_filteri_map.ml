open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[<h>";
  let test arr = begin
    let ordmap = of_karray arr in
    let ordmap' = filteri_map ordmap ~f:(fun i (_, v) ->
      match i % 2L = 0L with
      | true -> Some (Uns.to_string v)
      | false -> None
    ) in
    let arr' = to_array ordmap' in
    printf "%a -> %a@\n"
      (Array.pp Uns.pp) arr
      (Array.pp (pp_kv String.pp)) arr'
  end in
  iter_oc 0L 7L (fun n ->
    let arr = Array.init n ~f:(fun i -> i * 10L) in
    test arr
  );
  printf "@]"

let _ = test ()