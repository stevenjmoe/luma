open Ppxlib
open Ast_builder.Default

let component_rule () =
  let extracter () =
    let open Ast_pattern in
    ptyp __
  in
  let expander ~ctxt type_expr =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    let module_c =
      [%stri
        module C = Component.Make (struct
          type inner = t
        end)]
    in
    {
      pmod_desc = Pmod_structure [ [%stri type t = [%t type_expr]]; module_c ];
      pmod_loc = loc;
      pmod_attributes = [];
    }
  in

  let extender =
    let context = Extension.Context.module_expr in
    Extension.V3.declare "component" context (extracter ()) expander
  in
  Context_free.Rule.extension extender

let module_struct_rule () =
  let extracter () =
    let open Ast_pattern in
    pstr __
  in

  let expander ~ctxt payload =
    let open Parsetree in
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    match payload with
    | [
     {
       pstr_desc =
         Pstr_module
           {
             pmb_name = { txt = Some module_name };
             pmb_expr = { pmod_desc = Pmod_structure structure_items; _ };
             _;
           };
     };
    ] -> (
        let type_t =
          List.find_map
            (function
              | {
                  pstr_desc =
                    Pstr_type
                      (_, [ { ptype_name = { txt = "t" }; ptype_manifest = _; ptype_kind = _; _ } ]);
                  _;
                } ->
                  Some [%type: t]
              | _ -> None)
            structure_items
        in
        match type_t with
        | None ->
            pstr_extension ~loc
              (Location.error_extensionf ~loc "The module must define a type t")
              []
        | Some type_t ->
            let module_c =
              [%stri
                module C = Component.Make (struct
                  type inner = [%t type_t]
                end)]
            in
            let new_structure_items = structure_items @ [ module_c ] in
            let new_module_expr =
              {
                pmod_desc = Pmod_structure new_structure_items;
                pmod_loc = loc;
                pmod_attributes = [];
              }
            in
            let m =
              module_binding ~loc ~name:(Loc.make ~loc (Some module_name)) ~expr:new_module_expr
            in
            { pstr_desc = Pstr_module m; pstr_loc = loc })
    | _ -> Location.raise_errorf "Expected a single module declaration"
  in

  let extender =
    let context = Extension.Context.structure_item in
    Extension.V3.declare "component" context (extracter ()) expander
  in
  Context_free.Rule.extension extender

let () =
  Driver.register_transformation ~rules:[ component_rule (); module_struct_rule () ] "ppx_component"
