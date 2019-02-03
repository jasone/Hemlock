type 'a t [@@deriving sexp, compare]

val empty: 'a t
val init: int -> f:(int -> 'a) -> 'a t
val of_list: 'a list -> 'a t
val to_list: 'a t -> 'a list

val length: 'a t -> int
val is_empty: 'a t -> bool

val get: 'a t -> int -> 'a

val mutate: 'a t -> int -> 'a -> unit

val copy: 'a t -> 'a t
val slice: 'a t -> int -> int -> 'a t
val set: 'a t -> int -> 'a -> 'a t
val concat: 'a t list -> 'a t
val append: 'a t -> 'a t -> 'a t
val append_one: 'a t -> 'a -> 'a t
val insert: 'a t -> int -> 'a -> 'a t
val remove: 'a t -> int -> 'a t

val iter: 'a t -> f:('a -> unit) -> unit
val iteri: 'a t -> f:(int -> 'a -> unit) -> unit
val fold: 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
val fold_right: 'a t -> init:'accum -> f:('a -> 'accum -> 'accum) -> 'accum
val foldi: 'a t -> init:'accum -> f:(int -> 'accum -> 'a -> 'accum) -> 'accum
val fold_until: 'a t -> init:'accum -> f:('accum -> 'a -> 'accum * bool)
  -> 'accum
val foldi_until: 'a t -> init:'accum -> f:(int -> 'accum -> 'a -> 'accum * bool)
  -> 'accum

(* XXX Missing?
 * create
 * binary_search ?
 * mem
 * exists[i]
 * for_all[i]
 * count[i]
 * sum
 * find
 * find_map
 * to_vector, of_vector
 * min_elt, max_elt
 * max_length
 * make_matrix, init_matrix
 * fill
 * map, mapi
 * folding_map, folding_mapi
 * fold_map, fold_mapi
 * sort
 * is_sorted, is_sorted_strictly
 * concat_map[i]
 * partition[i]_tf
 * cartesian_product
 * transpose
 * filter_{opt,map[i]}
 * iter2, map2, fold2, for_all2, exists2
 * filter[i]
 * swap
 * rev[_inplace]
 * of_list[_rev][_map]
 * replace[_all]
 * map_inplace
 * find, find_map, findi, find_mapi
 * reduce
 * permute
 * zip
 * last
 * equal
 * normalize
 * slice
 * nget
 * nset
 *)