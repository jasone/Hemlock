open! Basis.Rudiments
open! Basis
open Codepoint
open Format

let pp_x ppf cp =
  Format.fprintf ppf "%a" Uns.pp_x (to_uns cp)

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let i = x in
        let t = of_uns (Uns.of_sint i) in
        let u = to_uns t in
        let t' = of_uns u in
        printf "of_uns %a -> to_uns %a -> of_uns %a -> %a\n"
          Uns.pp_x x pp_x t Uns.pp_x u pp_x t';
        fn xs'
      end
  in
  fn [Uns.max_value; 0L; 42L; 0xd800L; 0xdfffL; 0x10_ffffL; 0x11_0000L; 0x20_0000L; 0x20_0001L]

let _ = test ()