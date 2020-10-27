type module_type_411 =
  | Mty_ident of Path.t
  | Mty_signature of Types.signature
  | Mty_functor of Ident.t * Types.module_type option * Types.module_type
  | Mty_alias of unit * Path.t

let migrate_module_type : Types.module_type -> module_type_411 = function
  | Mty_ident p -> Mty_ident p
  | Mty_signature s -> Mty_signature s
  | Mty_functor (fp, mt) ->
    (match fp with
     | Unit -> Mty_functor(Ident.create_local "_", None, mt)
     | Named(i,mt) ->
       let i = (match i with None -> Ident.create_local "_" | Some i -> i) in
       Mty_functor (i, Some mt, mt))
  | Mty_alias p -> Mty_alias ((), p)
