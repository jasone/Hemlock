open! Basis.Rudiments
open! Basis
open Q
open Format

let test () =
  let ppt = (pp Uns.pp) in
  printf "@[<h>";
  let rec fn i n t = begin
    match i <= n with
    | false -> ()
    | true -> begin
        let elm, t' = pop t in
        printf "pop %a = %a %a\n" ppt t Uns.pp elm ppt t';
        fn (succ i) n (push_back i t)
      end
  end in
  (* halts if we start with empty *)
  fn 1L 4L (push_back 0L empty);
  printf "@]"

let _ = test ()