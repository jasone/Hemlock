open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test_min_max ordmap = begin
    File.Fmt.stdout
    |> Fmt.fmt "  min_opt -> "
    |> Option.pp (pp_kv_pair Uns.pp) (min_opt ordmap)
    |> Fmt.fmt "\n"
    |> (fun formatter ->
      match is_empty ordmap with
      | false -> begin
          formatter
          |> Fmt.fmt "  min -> "
          |> (pp_kv_pair Uns.pp) (min ordmap)
          |> Fmt.fmt "\n"
        end
      | true -> formatter
    )
    |> Fmt.fmt "  max_opt -> "
    |> Option.pp (pp_kv_pair Uns.pp) (max_opt ordmap)
    |> Fmt.fmt "\n"
    |> (fun formatter ->
      match is_empty ordmap with
      | false -> begin
          formatter
          |> Fmt.fmt "  max -> "
          |> (pp_kv_pair Uns.pp) (max ordmap)
          |> Fmt.fmt "\n"
        end
      | true -> formatter
    )
    |> ignore
  end in
  let test_search ordmap (key_max:uns) = begin
    Range.Uns.iter (0L =:= key_max) ~f:(fun probe ->
      let open Cmp in
      File.Fmt.stdout
      |> Fmt.fmt "  "
      |> Uns.pp probe
      |> Fmt.fmt " -> "
      |> (fun formatter ->
        match psearch probe ordmap with
        | None -> formatter |> Fmt.fmt "<"
        | Some (Lt, i) ->
          formatter |> Fmt.fmt "<[" |> Uns.pp i |> Fmt.fmt "]="
          |> (pp_kv_pair Uns.pp) (nth i ordmap)
        | Some (Eq, i) ->
          formatter |> Fmt.fmt "=[" |> Uns.pp i |> Fmt.fmt "]="
          |> (pp_kv_pair Uns.pp) (nth i ordmap)
        | Some (Gt, i) ->
          formatter |> Fmt.fmt ">[" |> Uns.pp i |> Fmt.fmt "]="
          |> (pp_kv_pair Uns.pp) (nth i ordmap)
      )
      |> Fmt.fmt ", "
      |> (fun formatter ->
        (match search probe ordmap with
          | None -> formatter |> Fmt.fmt "<>"
          | Some i -> formatter |> Fmt.fmt "=" |> (pp_kv_pair Uns.pp) (nth i ordmap)
        )
      )
      |> Fmt.fmt ", "
      |> (fun formatter ->
        match nsearch probe ordmap with
        | Some (Lt, i) ->
          formatter |> Fmt.fmt "<[" |> Uns.pp i |> Fmt.fmt "]="
          |> (pp_kv_pair Uns.pp) (nth i ordmap)
        | Some (Eq, i) ->
          formatter |> Fmt.fmt "=[" |> Uns.pp i |> Fmt.fmt "]="
          |> (pp_kv_pair Uns.pp) (nth i ordmap)
        | Some (Gt, i) ->
          formatter |> Fmt.fmt ">[" |> Uns.pp i |> Fmt.fmt "]="
          |> (pp_kv_pair Uns.pp) (nth i ordmap)
        | None -> formatter |> Fmt.fmt ">"
      )
      |> Fmt.fmt "\n"
      |> ignore
    );
  end in
  Range.Uns.iter (0L =:< 4L) ~f:(fun len ->
    let ordmap = of_array (module Uns)
      (Array.init (0L =:< len) ~f:(fun i -> let k = (i * 2L + 1L) in k, k * 10L)) in
    let key_max = len * 2L in
    File.Fmt.stdout
    |> (fmt_internals Uns.pp) ordmap
    |> Fmt.fmt "\n"
    |> ignore;
    let () = test_min_max ordmap in
    test_search ordmap key_max
  )

let _ = test ()
