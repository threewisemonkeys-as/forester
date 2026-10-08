(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

open struct
  module T = Types
  module String_map = Value.String_map
  module Symbol_map = Value.Symbol_map
  type located = Value.t Range.located
end

let extract_content (node : located) =
  match node.value with
  | Value.Content content -> content
  | v -> Reporter.fatal ?loc: node.loc (Type_error {expected = [Content]; got = Some v})

let extract_text_loc (node : located) : string Range.located =
  let content = extract_content node in
  let rec loop acc = function
    | [] -> Option.some @@ String.concat "" @@ Bwd.prepend acc []
    | (T.Text txt | T.CDATA txt) :: content -> loop (Bwd.snoc acc txt) content
    | T.Uri uri :: content -> loop (Bwd.snoc acc (URI.to_string uri)) content
    | _ -> None
  in
  match loop Emp (T.extract_content content) with
  | Some txt -> {value = String.trim txt; loc = node.loc}
  | None -> Reporter.fatal ?loc: node.loc (Type_error {expected = [Text]; got = None})

let extract_text (node : located) : string = (extract_text_loc node).value

let extract_obj_ptr (x : located) =
  match x.value with
  | Obj sym -> sym
  (* TODO: Rephrase, should be something like "this is a thing of type foo, cannot access method bar"*)
  | other -> Reporter.fatal ?loc: x.loc (Type_error {expected = [Obj]; got = Some other})

let extract_sym (x : located) =
  match x.value with
  | Sym sym -> sym
  | other -> Reporter.fatal ?loc: x.loc (Type_error {expected = [Sym]; got = Some other})

let extract_bool (x : located) =
  match x.value with
  | Content (T.Content [Text "true"]) -> true
  | Content (T.Content [Text "false"]) -> false
  | other -> Reporter.fatal ?loc: x.loc (Type_error {expected = [Bool]; got = Some other})

let default_backmatter ~(uri : URI.t) : T.content =
  let vtx = T.Uri_vertex uri in
  let make_section title query =
    let section =
      let frontmatter = T.default_frontmatter ~title: (T.Content [T.Text title]) () in
      let mainmatter = T.Content [T.Results_of_datalog_query query] in
      let flags = {T.default_section_flags with hidden_when_empty = Some true} in
      T.{frontmatter; mainmatter; flags}
    in
    T.Section section
  in
  T.Content
    [
      make_section "References" @@ Builtin_queries.references_datalog vtx;
      make_section "Context" @@ Builtin_queries.context_datalog vtx;
      make_section "Backlinks" @@ Builtin_queries.backlinks_datalog vtx;
      make_section "Related" @@ Builtin_queries.related_datalog vtx;
      make_section "Contributions" @@ Builtin_queries.contributions_datalog vtx
    ]

type result = {articles: T.content T.article list; jobs: Job.job Range.located list} [@@deriving show]

module Tape = Tape_effect.Make ()
module Lex_env = Algaeff.Reader.Make(struct type t = Value.t String_map.t end)
module Dyn_env = Algaeff.Reader.Make(struct type t = Value.t Symbol_map.t end)
module Config_env = Algaeff.Reader.Make(struct type t = Config.t end)
module Heap = Algaeff.State.Make(struct type t = Value.obj Symbol_map.t end)
module Emitted_trees = Algaeff.State.Make(struct type t = T.content T.article list end)
module Jobs = Algaeff.State.Make(struct type t = Job.job Range.located list end)
module Frontmatter = Algaeff.State.Make(struct type t = T.content T.frontmatter end)
module Mode_env = Algaeff.Reader.Make(struct type t = eval_mode end)

let get_current_uri ~loc =
  match (Frontmatter.get ()).uri with
  | Some uri -> uri
  | None -> Reporter.fatal ?loc Internal_error ~extra_remarks: [Asai.Diagnostic.loctext "No uri for tree"]

let get_transclusion_flags ~loc =
  let dynenv = Dyn_env.read () in
  let get_bool key =
    let@ value = Option.map @~ Symbol_map.find_opt key dynenv in
    extract_bool @@ Range.locate_opt loc value
  in
  let module S = Expand.Builtins.Transclude in
  let open Option_util in
  let flags = T.default_section_flags in
  {flags with
    expanded = override (get_bool S.expanded_sym) flags.expanded;
    header_shown = override (get_bool S.show_heading_sym) flags.header_shown;
    included_in_toc = override (get_bool S.toc_sym) flags.included_in_toc;
    numbered = override (get_bool S.numbered_sym) flags.numbered;
    metadata_shown = override (get_bool S.show_metadata_sym) flags.metadata_shown;
  }

let resolve_uri ~loc: _ str =
  match URI.of_string_exn str with
  | uri ->
    (* If the URI is just a single component without anything else, we should treat it as a link to a local tree. *)
    match URI.scheme uri, URI.host uri, URI.path_components uri with
    | None, None, ([] | [_]) ->
      let config = Config_env.read () in
      let uri = URI_scheme.named_uri ~base: config.url str in
      Result.ok uri
    | _ -> Ok uri
    | exception _ -> Error "Invalid URI"

let extract_uri (node : located) =
  let text = extract_text node in
  resolve_uri ~loc: node.loc text

let extract_dx_term (node : located) =
  match node.value with
  | Dx_var name -> Datalog_expr.Var name
  | Dx_const vtx -> Datalog_expr.Const vtx
  (* | other -> Reporter.fatalf Type_error "Expected datalog term" *)
  | other -> Reporter.fatal ?loc: node.loc (Type_error {expected = [Datalog_term]; got = Some other})

let extract_dx_prop (node : located) =
  match node.value with
  | Dx_prop prop -> prop
  (* | _ -> Reporter.fatalf Type_error "Expected datalog proposition" *)
  | other -> Reporter.fatal ?loc: node.loc (Type_error {expected = [Dx_prop]; got = Some other})

let extract_dx_sequent (node : located) =
  match node.value with
  | Dx_sequent sequent -> sequent
  | other -> Reporter.fatal ?loc: node.loc (Type_error {expected = [Dx_sequent]; got = Some other})

let extract_vertex ~type_ (node : located) =
  match type_ with
  | `Content ->
    Ok (T.Content_vertex (extract_content node))
  | `Uri ->
    let@ uri = Result.map @~ extract_uri node in
    T.Uri_vertex uri

let pp_tex_cs fmt = function
  | TeX_cs.Symbol x -> Format.fprintf fmt "\\%c" x
  | TeX_cs.Word x -> Format.fprintf fmt "\\%s " x

let rec process_tape () =
  match Tape.pop_node_opt () with
  | None -> Value.Content (T.Content [])
  | Some node -> eval_node node

and eval_tape tape =
  Tape.run ~tape process_tape

and eval_pop_arg ~loc =
  Tape.pop_arg ~loc
  |> Range.map eval_tape

and pop_content_arg ~loc =
  eval_pop_arg ~loc
  |> extract_content

and pop_text_arg ~loc =
  eval_pop_arg ~loc
  |> extract_text

and pop_text_arg_loc ~loc =
  eval_pop_arg ~loc
  |> extract_text_loc

and eval_node node : Value.t =
  let loc = node.loc in
  match node.value with
  | Var x ->
    eval_var ~loc x
  | Text str ->
    emit_content_node ~loc @@ T.Text str
  | Prim p ->
    let content = pop_content_arg ~loc |> T.extract_content |> T.trim_whitespace in
    emit_content_node ~loc @@ T.prim p @@ T.Content content
  | Fun (xs, body) ->
    let env = Lex_env.read () in
    focus_clo ?loc env (List.map (fun (info, x) -> info, Some x) xs) body
  | Ref ->
    begin
      match eval_pop_arg ~loc |> extract_uri with
      | Ok href ->
        let content =
          T.Content
            [
              T.Transclude {href; target = T.Taxon};
              T.Text " ";
              T.Contextual_number href
            ]
        in
        emit_content_node ~loc @@ Link {href; content}
      | Error _ ->
        Reporter.fatal
          ?loc
          (Type_error {got = None; expected = [URI]})
          ~extra_remarks: [Asai.Diagnostic.loctextf "Expected valid URI in ref"]
    end
  | Link {title; dest} ->
    let dest = {node with value = dest} |> Range.map eval_tape in
    let href =
      match extract_uri dest with
      | Ok uri -> uri
      | Error error ->
        Reporter.fatal
          ?loc
          (Type_error {expected = [URI]; got = None})
          ~extra_remarks: [Asai.Diagnostic.loctext error]
    (* "Expected valid URI in link") *)
    in
    let content =
      match title with
      | None -> T.Content [T.Transclude {href; target = T.Title {empty_when_untitled = false}}]
      | Some title -> {node with value = eval_tape title} |> extract_content
    in
    emit_content_node ~loc @@ Link {href; content}
  | Math (mode, body) ->
    let content =
      let@ () = Mode_env.run ~env: TeX_mode in
      {node with value = eval_tape body} |> extract_content
    in
    emit_content_node ~loc @@ KaTeX (mode, content)
  | Xml_tag (name, attrs, body) ->
    let rec process : _ list -> _ T.xml_attr list = function
      | [] -> []
      | (key, v) :: attrs ->
        {T.key; value = extract_content {node with value = eval_tape v}} :: process attrs
    in
    let name = T.{prefix = name.prefix; uname = name.uname; xmlns = name.xmlns} in
    let content = {node with value = eval_tape body} |> extract_content in
    emit_content_node ~loc @@ T.Xml_elt {name; attrs = process attrs; content}
  | TeX_cs cs ->
    emit_content_node ~loc @@ T.Text (Format.asprintf "%a" pp_tex_cs cs)
  | Unresolved_ident (visible, path) ->
    let tex_cs_opt =
      match path with
      | [name] -> TeX_cs.parse name
      | _ -> None
    in
    begin
      match Mode_env.read (), tex_cs_opt with
      | TeX_mode, Some (cs, rest) ->
        emit_content_node ~loc @@ T.Text (Format.asprintf "%a%s" pp_tex_cs cs rest)
      | _, _ ->
        let extra_remarks = Suggestions.create_suggestions ~visible path in
        Reporter.emit ?loc ~extra_remarks (Unresolved_identifier (visible, path));
        emit_content_node ~loc @@ T.Text (Format.asprintf "\\%a" Resolver.Scope.pp_path path)
    end
  | Transclude ->
    let flags = get_transclusion_flags ~loc in
    let href_arg = eval_pop_arg ~loc in
    let href =
      match extract_uri href_arg with
      | Ok uri -> uri
      | Error _ ->
        Reporter.fatal ?loc (Type_error {got = None; expected = [URI]}) ~extra_remarks: [Asai.Diagnostic.loctext "Expected valid URI in transclusion"]
    in
    emit_content_node ~loc @@ T.Transclude {href; target = Full flags}
  | Subtree (addr_opt, nodes) ->
    let flags = get_transclusion_flags ~loc in
    let config = Config_env.read () in
    let uri =
      match addr_opt with
      | Some addr -> Some (URI_scheme.named_uri ~base: config.url addr)
      | None -> None
    in
    let subtree = eval_tree_inner ?uri nodes in
    let frontmatter = Frontmatter.get () in
    let source_line = Option.map Range.begin_line_num loc in
    let subtree = {subtree with frontmatter = {subtree.frontmatter with uri; designated_parent = frontmatter.uri; source_line}} in
    begin
      match uri with
      | Some uri ->
        Emitted_trees.modify @@ List.cons subtree;
        let transclusion = T.{href = uri; target = Full flags} in
        emit_content_node ~loc @@ Transclude transclusion
      | None ->
        emit_content_node ~loc @@ T.Section (T.article_to_section ~flags subtree)
    end
  | Results_of_query ->
    let arg = eval_pop_arg ~loc in
    begin
      match arg.value with
      | Value.Dx_query query ->
        emit_content_node ~loc @@ Results_of_datalog_query query
      | other -> Reporter.fatal ?loc: arg.loc (Type_error {expected = [Dx_query]; got = Some other})
    end
  | Syndicate_query_as_json_blob ->
    let name = pop_text_arg ~loc in
    let config = Config_env.read () in
    let blob_uri = URI_scheme.named_uri ~base: config.url @@ name ^ ".json" in
    let query_arg = eval_pop_arg ~loc in
    begin
      match query_arg.value with
      | Dx_query query ->
        let job = Job.Syndicate (Json_blob {blob_uri; query}) in
        Jobs.modify @@ List.cons @@ Range.locate_opt loc job;
        process_tape ()
      | other -> Reporter.fatal ?loc: query_arg.loc (Type_error {expected = [Dx_query]; got = Some other})
    end
  | Syndicate_current_tree_as_atom_feed ->
    let source_uri = get_current_uri ~loc: node.loc in
    let feed_uri =
      let components = URI.append_path_component (URI.path_components source_uri) "atom.xml" in
      URI.with_path_components components source_uri
    in
    let job = Job.Syndicate (Atom_feed {source_uri; feed_uri}) in
    Jobs.modify @@ List.cons @@ Range.locate_opt loc job;
    process_tape ()
  | Embed_tex ->
    let config = Config_env.read () in
    let preamble, body =
      let@ () = Mode_env.run ~env: TeX_mode in
      let preamble = pop_content_arg ~loc |> TeX_like.string_of_content in
      let body = pop_content_arg ~loc |> TeX_like.string_of_content in
      preamble, body
    in
    let source = LaTeX_template.to_string ~preamble ~body in
    let hash = Digest.to_hex @@ Digest.string source in
    let job = Job.{hash; source} in
    let uri = Job.uri_for_latex_to_svg_job ~base: config.url job in
    let content =
      T.Content
        [
          T.Xml_elt
            {
              content = T.Content [];
              name = {uname = "img"; prefix = "html"; xmlns = Some "http://www.w3.org/1999/xhtml"};
              attrs = [
                {
                  key = {uname = "src"; prefix = ""; xmlns = None};
                  value = T.Content [T.Route_of_uri uri]
                }
              ]
            }
        ]
    in
    let sources = [
      T.{type_ = "latex"; part = "preamble"; source = preamble};
      T.{type_ = "latex"; part = "body"; source = body}
    ]
    in
    let artefact = T.{hash; content; sources} in
    Jobs.modify (List.cons (Range.locate_opt loc (Job.LaTeX_to_svg job)));
    emit_content_node ~loc @@ T.Artefact artefact
  | Route_asset ->
    let Range.{value = source_path; loc = path_loc} = pop_text_arg_loc ~loc in
    let uri = Asset_router.uri_of_asset ?loc: path_loc ~source_path () in
    emit_content_nodes ~loc @@ [T.Route_of_uri uri]
  | Object {self; methods} ->
    let table =
      let env = Lex_env.read () in
      let add (name, body) =
        Value.Method_table.add name Value.{body; self; super = None; env}
      in
      List.fold_right add methods Value.Method_table.empty
    in
    let sym = Symbol.named ["obj"] in
    Heap.modify @@ Symbol_map.add sym Value.{prototype = None; methods = table};
    focus ?loc: node.loc @@ Value.Obj sym
  | Patch {obj; self; super; methods} ->
    let obj_ptr = {node with value = obj} |> Range.map eval_tape |> extract_obj_ptr in
    let table =
      let env = Lex_env.read () in
      let add (name, body) =
        Value.Method_table.add
          name
          Value.{body; self; super; env}
      in
      List.fold_right add methods Value.Method_table.empty
    in
    let sym = Symbol.named ["obj"] in
    Heap.modify @@ Symbol_map.add sym Value.{prototype = Some obj_ptr; methods = table};
    focus ?loc: node.loc @@ Value.Obj sym
  | Group (d, body) ->
    let l, r = delim_to_strings d in
    let content =
      let body = extract_content {node with value = eval_tape body} in
      T.Content (T.Text l :: T.extract_content body @ [T.Text r])
    in
    focus ?loc: node.loc @@ Value.Content (T.compress_content content)
  | Call (obj, method_name) ->
    let sym = {node with value = obj} |> Range.map eval_tape |> extract_obj_ptr in
    let rec call_method (obj : Value.obj) =
      let proto_val = obj.prototype |> Option.map @@ fun ptr -> Value.Obj ptr in
      match Value.Method_table.find_opt method_name obj.methods with
      | Some mthd ->
        let env =
          let env =
            match mthd.self with
            | None -> mthd.env
            | Some self -> String_map.add self (Value.Obj sym) mthd.env
          in
          match proto_val with
          | None -> env
          | Some proto_val ->
            match mthd.super with
            | None -> env
            | Some super -> String_map.add super proto_val env
        in
        let@ () = Lex_env.run ~env in
        eval_tape mthd.body
      | None ->
        match obj.prototype with
        | Some proto ->
          call_method @@ Symbol_map.find proto @@ Heap.get ()
        | None ->
          Reporter.fatal
            ?loc: node.loc
            (Unbound_method (method_name, obj))
    in
    let result = call_method @@ Symbol_map.find sym @@ Heap.get () in
    focus ?loc: node.loc result
  | Put (k, v, body) ->
    let k = {node with value = k} |> Range.map eval_tape |> extract_sym in
    let body =
      let@ () = Dyn_env.scope (Symbol_map.add k (eval_tape v)) in
      eval_tape body
    in
    focus ?loc: node.loc body
  | Default (k, v, body) ->
    let k = {node with value = k} |> Range.map eval_tape |> extract_sym in
    let body =
      let upd flenv = if Symbol_map.mem k flenv then flenv else Symbol_map.add k (eval_tape v) flenv in
      let@ () = Dyn_env.scope upd in
      eval_tape body
    in
    focus ?loc: node.loc body
  | Get k ->
    let k = {node with value = k} |> Range.map eval_tape |> extract_sym in
    let env = Dyn_env.read () in
    begin
      match Symbol_map.find_opt k env with
      | None ->
        Reporter.fatal
          ?loc: node.loc
          (Unbound_fluid_symbol k)
      | Some v -> focus ?loc: node.loc v
    end
  | Verbatim str ->
    emit_content_node ~loc @@ CDATA str
  | Title ->
    let title = pop_content_arg ~loc in
    Frontmatter.modify (fun fm -> {fm with title = Some title});
    process_tape ()
  | Parent ->
    let parent_arg = eval_pop_arg ~loc in
    let parent =
      match extract_uri parent_arg with
      | Ok uri -> uri
      | Error _ -> Reporter.fatal ?loc Invalid_URI ~extra_remarks: [Asai.Diagnostic.loctext "Expected valid URI in parent declaration"]
    in
    Frontmatter.modify (fun fm -> {fm with designated_parent = Some parent});
    process_tape ()
  | Meta ->
    let k = pop_text_arg ~loc in
    let v = pop_content_arg ~loc in
    Frontmatter.modify (fun fm -> {fm with metas = fm.metas @ [k, v]});
    process_tape ()
  | Attribution (role, type_) ->
    let arg = eval_pop_arg ~loc in
    let vertex =
      match extract_vertex ~type_ arg with
      | Ok vtx -> vtx
      | Error _ ->
        let corrected_attribution_code =
          match role with
          | Author -> "\\author/literal"
          | Contributor -> "\\contributor/literal"
        in
        Reporter.emit
          ?loc
          Type_warning
          ~extra_remarks: [Asai.Diagnostic.loctextf "Expected valid URI in attribution. Use `%s` instead if you intend an unlinked attribution." corrected_attribution_code];
        T.Content_vertex (extract_content arg)
    in
    let attribution = T.{role; vertex} in
    Frontmatter.modify (fun fm -> {fm with attributions = fm.attributions @ [attribution]});
    process_tape ()
  | Tag type_ ->
    let arg = eval_pop_arg ~loc in
    let vertex =
      match extract_vertex ~type_ arg with
      | Ok vtx -> vtx
      | Error _ ->
        let corrected = "\\tag/content" in
        Reporter.emit ?loc Type_warning ~extra_remarks: [Asai.Diagnostic.loctextf "Expected valid URI in tag. Use `%s` instead if you intend an unlinked attribution." corrected];
        T.Content_vertex (extract_content arg)
    in
    Frontmatter.modify (fun fm -> {fm with tags = fm.tags @ [vertex]});
    process_tape ()
  | Date ->
    let date_str = pop_text_arg ~loc in
    begin
      match Human_datetime.parse_string date_str with
      | None ->
        Reporter.fatal ?loc: node.loc Parse_error ~extra_remarks: [Asai.Diagnostic.loctextf "Invalid date string `%s`" date_str]
      | Some date ->
        Frontmatter.modify (fun fm -> {fm with dates = fm.dates @ [date]});
        process_tape ()
    end
  | Number ->
    let num = pop_text_arg ~loc in
    Frontmatter.modify (fun fm -> {fm with number = Some num});
    process_tape ()
  | Taxon ->
    let taxon = Some (pop_content_arg ~loc) in
    Frontmatter.modify (fun fm -> {fm with taxon});
    process_tape ()
  | Sym sym ->
    focus ?loc: node.loc @@ Value.Sym sym
  | Dx_prop (rel, args) ->
    let rel = {node with value = eval_tape rel} |> extract_text in
    let args =
      let@ arg = List.map @~ args in
      {node with value = eval_tape arg} |> extract_dx_term
    in
    focus ?loc: node.loc @@ Dx_prop {rel; args}
  | Dx_sequent (conclusion, premises) ->
    let conclusion = {node with value = eval_tape conclusion} |> extract_dx_prop in
    let premises =
      let@ premise = List.map @~ premises in
      {node with value = eval_tape premise} |> extract_dx_prop
    in
    focus ?loc: node.loc @@ Dx_sequent {conclusion; premises}
  | Dx_query (var, positives, negatives) ->
    let positives =
      let@ premise = List.map @~ positives in
      {node with value = eval_tape premise} |> extract_dx_prop
    in
    let negatives =
      let@ premise = List.map @~ negatives in
      {node with value = eval_tape premise} |> extract_dx_prop
    in
    focus ?loc: node.loc @@ Dx_query {var; positives; negatives}
  | Dx_var name ->
    focus ?loc: node.loc @@ Dx_var name
  | Dx_const (type_, arg) ->
    let arg = {node with value = eval_tape arg} in
    let const =
      match type_ with
      | `Content -> T.Content_vertex (extract_content arg)
      | `Uri ->
        begin
          match extract_uri arg with
          | Ok uri -> T.Uri_vertex uri
          | Error _ ->
            Reporter.fatal ?loc: node.loc Invalid_URI ~extra_remarks: [Asai.Diagnostic.loctext "Expected valid URI in datalog constant expression."]
        end
    in
    focus ?loc: node.loc @@ Dx_const const
  | Dx_execute ->
    let script = eval_pop_arg ~loc: node.loc |> extract_dx_sequent in
    emit_content_node ~loc: node.loc @@ T.Datalog_script [script]
  | Current_tree ->
    emit_content_node ~loc: node.loc @@ T.Uri (get_current_uri ~loc: node.loc)

and eval_var ~loc (x : string) =
  let env = Lex_env.read () in
  match String_map.find_opt x env with
  | Some v -> focus ?loc v
  | None ->
    Reporter.fatal
      ?loc
      (Unbound_variable x)

and focus ?loc = function
  | Clo (rho, xs, body) ->
    focus_clo ?loc rho xs body
  | Content content ->
    begin
      match process_tape () with
      | Content content' -> Value.Content (T.concat_compressed_content content content')
      | value -> value
    end
  | Sym _ | Obj _ | Dx_prop _ | Dx_sequent _ | Dx_query _ | Dx_var _ | Dx_const _ as v ->
    begin
      match process_tape () with
      | Content content when T.strip_whitespace content = T.Content [] -> v
      | v' ->
        Reporter.fatal
          ?loc
          (Type_error {expected = []; got = None})
          ~extra_remarks: [Asai.Diagnostic.loctextf "Expected solitary node but got %a / %a" Value.pp v Value.pp v']
    end

and focus_clo ?loc rho (xs : string option binding list) body =
  match xs with
  | [] ->
    focus ?loc @@
      let@ () = Lex_env.run ~env: rho in
      eval_tape body
  | (info, y) :: ys ->
    match Tape.pop_arg_opt () with
    | Some arg ->
      let yval =
        match info with
        | Strict -> eval_tape arg.value
        | Lazy -> Clo (Lex_env.read (), [(Strict, None)], arg.value)
      in
      let rhoy = match y with Some y -> String_map.add y yval rho | None -> rho in
      focus_clo ?loc rhoy ys body
    | None ->
      begin
        match process_tape () with
        | Content nodes when T.strip_whitespace nodes = T.Content [] -> Clo (rho, xs, body)
        | _ -> Reporter.fatal ?loc Missing_argument ~extra_remarks: [Asai.Diagnostic.loctextf "Expected %i additional arguments" (List.length xs)]
      end

and emit_content_nodes ~loc content =
  focus ?loc @@ Content (T.Content (T.compress_nodes content))

and emit_content_node ~loc content =
  emit_content_nodes ~loc [content]

and eval_tree_inner ?(uri : URI.t option) (syn : Syn.t) : T.content T.article =
  let attribution_is_author attr =
    match T.(attr.role) with
    | T.Author -> true
    | _ -> false
  in
  let outer_frontmatter = Frontmatter.get () in
  let attributions = List.filter attribution_is_author outer_frontmatter.attributions in
  let frontmatter =
    T.default_frontmatter
      ?uri
      ~attributions
      ?source_path: outer_frontmatter.source_path
      ~dates: outer_frontmatter.dates
      ()
  in
  let@ () = Frontmatter.run ~init: frontmatter in
  let mainmatter = {value = eval_tape syn; loc = None} |> extract_content in
  let frontmatter = Frontmatter.get () in
  let backmatter = match uri with Some uri -> default_backmatter ~uri | None -> Content [] in
  T.{frontmatter; mainmatter; backmatter = backmatter}

let empty_result = {
  articles = [];
  jobs = []
}

let eval_tree
    ~(config : Config.t)
    ~(uri : URI.t)
    ~(source_path : string option)
    (tree : Syn.t)
    : result * Reporter.diagnostic list
  =
  let diagnostics = ref [] in
  let push d = diagnostics := d :: !diagnostics in
  let res =
    Reporter.run
      ~fatal: (fun d -> push d; empty_result)
      ~emit: push
      @@ fun () ->
      let fm = T.default_frontmatter ~uri ?source_path () in
      let@ () = Mode_env.run ~env: Text_mode in
      let@ () = Frontmatter.run ~init: fm in
      let@ () = Emitted_trees.run ~init: [] in
      let@ () = Jobs.run ~init: [] in
      let@ () = Heap.run ~init: Symbol_map.empty in
      let@ () = Lex_env.run ~env: String_map.empty in
      let@ () = Dyn_env.run ~env: Symbol_map.empty in
      let@ () = Config_env.run ~env: config in
      let main = eval_tree_inner ~uri tree in
      let side = Emitted_trees.get () in
      let jobs = Jobs.get () in
      {articles = main :: side; jobs}
  in
  res, !diagnostics
