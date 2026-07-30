open! Basis.Rudiments
open! Basis
open Ordset

let test () =
  let test_min_max ordset = begin
    File.Fmt.stdout
    |> Fmt.fmt "  min_opt -> "
    |> Option.pp Uns.pp (min_opt ordset)
    |> Fmt.fmt "\n"
    |> (fun formatter ->
      match is_empty ordset with
      | false -> begin
          formatter
          |> Fmt.fmt "  min -> "
          |> Uns.pp (min ordset)
          |> Fmt.fmt "\n"
        end
      | true -> formatter
    )
    |> Fmt.fmt "  max_opt -> "
    |> Option.pp Uns.pp (max_opt ordset)
    |> Fmt.fmt "\n"
    |> (fun formatter ->
      match is_empty ordset with
      | false -> begin
          formatter
          |> Fmt.fmt "  max -> "
          |> Uns.pp (max ordset)
          |> Fmt.fmt "\n"
        end
      | true -> formatter
    )
    |> ignore
  end in
  let test_search ordset key_max = begin
    Range.Uns.iter (0L =:= key_max) ~f:(fun probe ->
      File.Fmt.stdout
      |> Fmt.fmt "  "
      |> Uns.pp probe
      |> Fmt.fmt " -> "
      |> (fun formatter ->
        match psearch probe ordset with
        | None -> formatter |> Fmt.fmt "<"
        | Some (Lt, i) ->
          formatter |> Fmt.fmt "<[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (nth i ordset)
        | Some (Eq, i) ->
          formatter |> Fmt.fmt "=[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (nth i ordset)
        | Some (Gt, i) ->
          formatter |> Fmt.fmt ">[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (nth i ordset)
      )
      |> Fmt.fmt ", "
      |> (fun formatter ->
        (match search probe ordset with
          | None -> formatter |> Fmt.fmt "<>"
          | Some i -> formatter |> Fmt.fmt "=" |> Uns.pp (nth i ordset)
        )
      )
      |> Fmt.fmt ", "
      |> (fun formatter ->
        match nsearch probe ordset with
        | Some (Lt, i) ->
          formatter |> Fmt.fmt "<[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (nth i ordset)
        | Some (Eq, i) ->
          formatter |> Fmt.fmt "=[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (nth i ordset)
        | Some (Gt, i) ->
          formatter |> Fmt.fmt ">[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (nth i ordset)
        | None -> formatter |> Fmt.fmt ">"
      )
      |> Fmt.fmt "\n"
      |> ignore
    )
  end in
  Range.Uns.iter (0L =:< 4L) ~f:(fun len ->
    let ordset = of_array (module Uns)
      (Array.init (0L =:< len) ~f:(fun i -> i * 2L + 1L)) in
    let key_max = len * 2L in
    File.Fmt.stdout
    |> fmt ordset
    |> Fmt.fmt "\n"
    |> ignore;
    let () = test_min_max ordset in
    test_search ordset key_max
  )

let _ = test ()
