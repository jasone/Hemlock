open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  let test_min_max bitset = begin
    File.Fmt.stdout
    |> Fmt.fmt "  min_opt -> "
    |> Option.pp Uns.pp (min_opt bitset)
    |> Fmt.fmt "\n"
    |> (fun formatter ->
      match is_empty bitset with
      | false -> begin
          formatter
          |> Fmt.fmt "  min -> "
          |> Uns.pp (min bitset)
          |> Fmt.fmt "\n"
        end
      | true -> formatter
    )
    |> Fmt.fmt "  max_opt -> "
    |> Option.pp Uns.pp (max_opt bitset)
    |> Fmt.fmt "\n"
    |> (fun formatter ->
      match is_empty bitset with
      | false -> begin
          formatter
          |> Fmt.fmt "  max -> "
          |> Uns.pp (max bitset)
          |> Fmt.fmt "\n"
        end
      | true -> formatter
    )
    |> ignore
  end in
  let test_search bitset key_max = begin
    Range.Uns.iter (0L =:= key_max) ~f:(fun probe ->
      File.Fmt.stdout
      |> Fmt.fmt "  "
      |> Uns.pp probe
      |> Fmt.fmt " -> "
      |> (fun formatter ->
        match psearch probe bitset with
        | None -> formatter |> Fmt.fmt "<"
        | Some (Lt, i) ->
          formatter |> Fmt.fmt "<[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (nth i bitset)
        | Some (Eq, i) ->
          formatter |> Fmt.fmt "=[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (nth i bitset)
        | Some (Gt, i) ->
          formatter |> Fmt.fmt ">[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (nth i bitset)
      )
      |> Fmt.fmt ", "
      |> (fun formatter ->
        (match search probe bitset with
          | None -> formatter |> Fmt.fmt "<>"
          | Some i -> formatter |> Fmt.fmt "=" |> Uns.pp (nth i bitset)
        )
      )
      |> Fmt.fmt ", "
      |> (fun formatter ->
        match nsearch probe bitset with
        | Some (Lt, i) ->
          formatter |> Fmt.fmt "<[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (nth i bitset)
        | Some (Eq, i) ->
          formatter |> Fmt.fmt "=[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (nth i bitset)
        | Some (Gt, i) ->
          formatter |> Fmt.fmt ">[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (nth i bitset)
        | None -> formatter |> Fmt.fmt ">"
      )
      |> Fmt.fmt "\n"
      |> ignore
    )
  end in
  Range.Uns.iter (0L =:< 4L) ~f:(fun len ->
    let bitset = of_array
        (Array.init (0L =:< len) ~f:(fun i -> i * 2L + 1L)) in
    let key_max = len * 2L in
    File.Fmt.stdout
    |> fmt bitset
    |> Fmt.fmt "\n"
    |> ignore;
    let () = test_min_max bitset in
    test_search bitset key_max
  )

let _ = test ()
