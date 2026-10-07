(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_xml_names
open Forester_core
open Forester_compiler

open struct module T = Types end
open Pure_html
open HTML

type query = {
  query: (string, T.content T.vertex) Forester_core.Datalog_expr.query;
}
[@@deriving repr]

module Xmlns = Xmlns_effect.Make ()

let local_path_components (uri : URI.t) =
  let host =
    match URI.host uri with
    | Some host -> host
    | None -> assert false (* TODO*)
  in
  host :: URI.path_components uri

let route (forest : State.t) uri : URI.t =
  let open State.Syntax in
  match forest.={uri} with
  | None -> uri
  | Some _ ->
    let path = "" :: local_path_components uri in
    URI.make ~path ()

let xhtml_ns = "http://www.w3.org/1999/xhtml"

(* We are emitting HTML rather than XML, so elements in the XHTML namespace
   (e.g. [html:p], produced by [\p]) must be rendered without their prefix;
   otherwise the browser treats them as unknown elements. *)
let render_xml_qname = function
  | {xmlns = Some xmlns; uname; _} when xmlns = xhtml_ns -> uname
  | {prefix = "html"; xmlns = None; uname} -> uname
  | {prefix = ""; uname; _} -> uname
  | {prefix; uname; _} -> Format.sprintf "%s:%s" prefix uname

let render_xml_attr (forest : State.t) : T.content T.xml_attr -> _ =
  fun T.{key; value} ->
  let value =
    Plain_text_client.string_of_content
      ~forest
      ~router: (Legacy_xml_client.route forest)
      value
  in
  string_attr (render_xml_qname key) "%s" value

let render_xmlns_prefix ({prefix; xmlns}: xmlns_attr) =
  let attr = match prefix with "" -> "xmlns" | _ -> "xmlns:" ^ prefix in
  string_attr attr "%s" xmlns

let render_date (date : Human_datetime.t) =
  let year = txt "%i" (Human_datetime.year date) in
  let month =
    match Human_datetime.month date with
    | None -> None
    | Some i ->
      match i with
      | 1 -> Some (txt "January")
      | 2 -> Some (txt "February")
      | 3 -> Some (txt "March")
      | 4 -> Some (txt "April")
      | 5 -> Some (txt "May")
      | 6 -> Some (txt "June")
      | 7 -> Some (txt "July")
      | 8 -> Some (txt "August")
      | 9 -> Some (txt "September")
      | 10 -> Some (txt "October")
      | 11 -> Some (txt "November")
      | 12 -> Some (txt "December")
      | _ -> assert false
  in
  let day =
    match Human_datetime.day date with
    | None -> null []
    | Some i -> txt "%i" i
  in
  (* Dates have no page of their own, so unlike tree.xsl there is no link. *)
  li
    [class_ "meta-item"]
    [
      Option.value ~default: (null []) month;
      if Option.is_some month then txt " " else null [];
      day;
      if Option.is_some month then txt ", " else null [];
      year
    ]

(* Rendering context. Transclusions are rendered inline (as in the static
   build) so that section numbers and the table of contents can be computed
   the same way the XSLT theme does. Rendering never yields to the Eio
   scheduler, so a plain reference is enough to thread the context. *)
type ctx = {
  ancestors: URI.t list; (* trees being rendered, to detect transclusion loops *)
  number: int list; (* number of the closest numbered ancestor section *)
  unnumbered: bool; (* some ancestor has [numbered = false] or [toc = false] *)
  in_backmatter: bool;
}

let root_ctx = {ancestors = []; number = []; unnumbered = false; in_backmatter = false}

let current_ctx = ref root_ctx

let with_ctx ctx kont =
  let old = !current_ctx in
  current_ctx := ctx;
  Fun.protect ~finally: (fun () -> current_ctx := old) kont

let is_ancestor uri = List.exists (URI.equal uri) !current_ctx.ancestors

let nbsp = "\u{00A0}"

let route_string uri = "/trees" ^ URI.path_string uri

(* Replace a full transclusion by the section it denotes, unless it would loop. *)
let inline_full_transclusion (forest : State.t) (node : T.content T.content_node) =
  match node with
  | T.Transclude ({target = Full _; href} as transclusion) when not (is_ancestor href) ->
    begin
      match State.get_content_of_transclusion transclusion forest with
      | Some (T.Content [T.Section section]) -> T.Section section
      | _ -> node
    end
  | _ -> node

(* The sections that appear directly in some content once transclusions are
   inlined, i.e. the [f:tree] children of an [f:mainmatter] in the XML. *)
let rec child_sections ?(visited = []) (forest : State.t) (T.Content nodes) =
  let@ node = List.concat_map @~ nodes in
  match inline_full_transclusion forest node with
  | T.Section section -> [section]
  | T.Transclude ({target = Mainmatter; href} as transclusion) when not (List.exists (URI.equal href) visited) ->
    begin
      match State.get_content_of_transclusion transclusion forest with
      | Some content -> child_sections ~visited: (href :: visited) forest content
      | None -> []
    end
  | _ -> []

let excluded_from_numbering (section : T.content T.section) =
  section.flags.numbered = Some false || section.flags.included_in_toc = Some false

(* Number a list of sibling sections following the rules of the
   [tree-taxon-with-number] template in tree.xsl. Returns, for each section,
   the number to display (if any) and the context for rendering its body. *)
let number_siblings (forest : State.t) (sections : T.content T.section list) =
  let ctx = !current_ctx in
  let siblings = List.length sections in
  let counter = ref 0 in
  let@ section = List.map @~ sections in
  let excluded = excluded_from_numbering section in
  let number =
    if excluded then ctx.number
    else (incr counter; ctx.number @ [!counter])
  in
  let unnumbered = ctx.unnumbered || excluded in
  let implicitly_unnumbered =
    siblings = 1 && not (List.length (child_sections forest section.mainmatter) > 1)
  in
  let label =
    match section.frontmatter.number with
    | Some number -> Some number
    | None ->
      if not ctx.in_backmatter && not unnumbered && not implicitly_unnumbered then
        Some (String.concat "." (List.map string_of_int number))
      else None
  in
  (* The section's body is a mainmatter transclusion, which records its URI. *)
  section, label, {ctx with number; unnumbered}

let section_id (section : T.content T.section) label =
  match section.frontmatter.uri, label with
  | Some uri, _ -> Some ("tree-" ^ String.concat "-" (List.filter (( <> ) "") (URI.path_components uri)))
  (* Ids are used as CSS selectors by the TOC, so avoid dots. *)
  | None, Some label -> Some ("section-" ^ String.map (function '.' -> '-' | c -> c) label)
  | None, None -> None

let rec render_article (forest : State.t) (article : T.content T.article) : node =
  let@ () = Xmlns.run ~reserved: [] in
  let@ () = with_ctx {root_ctx with ancestors = Option.to_list article.frontmatter.uri} in
  HTML.article
    [id "tree-container";]
    [
      HTML.section
        [class_ "block"]
        [
          details
            [open_] @@
            summary
              []
              [render_frontmatter forest ?label: article.frontmatter.number article.frontmatter] :: render_content forest article.mainmatter;
        ];
      match article.frontmatter.uri with
      | Some uri when URI.equal (Config.home_uri forest.config) uri -> null []
      | _ -> footer [] @@ render_backmatter forest article.backmatter
    ]

and render_section ?label (forest : State.t) (section : T.content T.section) : node =
  match section with
  | {frontmatter; mainmatter; flags} ->
    let test k = function
      | Some true -> true
      | Some false -> false
      | None -> k
    in
    let class_ =
      if test false flags.metadata_shown then class_ "block"
      else class_ "block hide-metadata"
    in
    let id_ =
      match section_id section label with
      | Some s -> id "%s" s
      | None -> null_
    in
    HTML.section
      [class_; id_]
      [
        if test true flags.header_shown then
          details
            [if test true flags.expanded then open_ else null_]
            [
              summary [] [render_frontmatter forest ?label frontmatter];
              null @@ render_content forest mainmatter;
            ]
        else null @@ render_content forest mainmatter;
      ]

(* Same as render_section, but adds the backmatter-section class *)
and render_backmatter (forest : State.t) backmatter =
  let@ () = with_ctx {!current_ctx with in_backmatter = true} in
  let@ node = List.map @~ render_content forest backmatter in
  let attrs = Format.asprintf "%s backmatter-section" node.@["class"] in
  node +@ class_ "%s" attrs

and render_attributions forest (attributions : T.content T.attribution list) =
  let render_attribution attribution =
    match attribution with
    | T.{vertex; _} ->
      match vertex with
      | T.Uri_vertex href ->
        let content = T.Content [T.Transclude {href; target = Title {empty_when_untitled = false}}] in
        null @@ render_link forest T.{href; content}
      | T.Content_vertex content ->
        null @@ render_content forest content
  in
  let authors, contributors =
    attributions
    |> List.partition_map @@ fun a ->
      match T.(a.role) with
      | T.Author -> Left a
      | Contributor -> Right a
  in
  li
    [class_ "meta-item"]
    [
      address [class_ "author"] @@
      List.map render_attribution authors @
      begin
        if List.length contributors > 0 then
          [txt "with contributions from "]
        else []
      end @
      List.map render_attribution contributors
    ]

(* The taxon and number shown before a title, e.g. "Definition 1.2. " *)
and render_taxon_with_number ?label (forest : State.t) (frontmatter : T.content T.frontmatter) =
  let taxon = Option.map (render_content forest) frontmatter.taxon in
  span [class_ "taxon"] @@
  Option.value ~default: [] taxon @
  (if Option.is_some taxon && Option.is_some label then [txt "%s" nbsp] else []) @
  (match label with Some l -> [txt "%s" l] | None -> []) @
  (if Option.is_some taxon || Option.is_some label then [txt ".%s" nbsp] else [])

and render_frontmatter ?label (forest : State.t) (frontmatter : T.content T.frontmatter) : node =
  let title =
    Option.value ~default: [] @@
      let@ c = Option.map @~ frontmatter.title in
      render_content forest c
  in
  let uri =
    match frontmatter.uri with
    | None -> null []
    | Some uri ->
      a
        [class_ "slug"; href "%s" (route_string uri);]
        [txt "[%s]" (URI.display_path_string ~base: forest.config.url uri)]
  in
  let source_path =
    match frontmatter.source_path with
    | Some path ->
      [a [class_ "edit-button"; href "zed://file%s" path] [txt "[edit]"]]
    | None -> []
  in
  let find_meta key =
    let@ str, content = List.find_map @~ frontmatter.metas in
    if str = key then Some content
    else None
  in
  let render_meta key f =
    Option.value
      ~default: (null [])
      (Option.map f (find_meta key))
  in
  let default_meta_item content =
    li
      [class_ "meta-item"]
      (render_content forest content)
  in
  let labelled_external_link ~href ~label =
    li
      [class_ "meta-item"]
      [a [class_ "link external"; href] [txt "%s" label]]
  in
  let to_string =
    Plain_text_client.string_of_content
      ~forest
      ~router: (Legacy_xml_client.route forest)
  in
  let position = render_meta "position" default_meta_item in
  let institution = render_meta "institution" default_meta_item in
  let venue = render_meta "venue" default_meta_item in
  let source = render_meta "source" default_meta_item in
  let doi = render_meta "doi" default_meta_item in
  let orcid =
    render_meta "orcid" @@ fun c ->
    let content = to_string c in
    li
      [class_ "meta-item"]
      [
        a
          [class_ "doi link"; href "https://www.doi.org/%s" content;]
          [txt "%s" content]
      ]
  in
  let external_ =
    render_meta "external" @@ fun c ->
    let content = to_string c in
    li
      [class_ "meta-item"]
      [
        a
          [class_ "link external"; href "%s" content;]
          [txt "%s" content]
      ]
  in
  let slides =
    render_meta "slides" @@ fun c ->
    labelled_external_link ~href: (href "%s" (to_string c)) ~label: "Slides"
  in
  let video =
    render_meta "video" @@ fun c ->
    labelled_external_link ~href: (href "%s" (to_string c)) ~label: "Video"
  in
  header
    []
    [
      h1 [] @@ [render_taxon_with_number ?label forest frontmatter] @ title @ [txt " "; uri; txt " "] @ source_path;
      div
        [class_ "metadata"]
        [
          ul [] @@
          List.map render_date frontmatter.dates @
          [
            render_attributions forest frontmatter.attributions;
            position;
            institution;
            venue;
            source;
            doi;
            orcid;
            external_;
            slides;
            video;
          ]
        ];
    ]

and render_transclusion_node (forest : State.t) (transclusion : T.transclusion) =
  if is_ancestor transclusion.href then
    [span [class_ "error"] [txt "Transclusion loop detected: %s" (URI.to_string transclusion.href)]]
  else
    match State.get_content_of_transclusion transclusion forest with
    | None -> []
    | Some content ->
      let ctx = !current_ctx in
      let ancestors =
        match transclusion.target with
        | Full _ | Mainmatter -> transclusion.href :: ctx.ancestors
        | Title _ | Taxon -> ctx.ancestors
      in
      let@ () = with_ctx {ctx with ancestors} in
      render_content forest content

and render_content (forest : State.t) (Content content: T.content) : node list =
  let content = List.map (inline_full_transclusion forest) content in
  let sections =
    List.filter_map (function T.Section s -> Some s | _ -> None) content
  in
  let numbered = ref (number_siblings forest sections) in
  let@ node = List.concat_map @~ content in
  match node, !numbered with
  | T.Section _, (section, label, ctx) :: rest ->
    numbered := rest;
    let@ () = with_ctx ctx in
    [render_section ?label forest section]
  | _ -> render_content_node forest node

and render_content_node (forest : State.t) (node : 'a T.content_node) : node list =
  match node with
  | Text str ->
    [txt "%s" str]
  | CDATA str ->
    [txt ~raw: true "<![CDATA[%s]]>" str]
  | Xml_elt elt ->
    let prefixes_to_add, (name, attrs, content) =
      let@ () = Xmlns.within_scope in
      render_xml_qname elt.name,
      List.map (render_xml_attr forest) elt.attrs,
      render_content forest elt.content
    in
    let attrs =
      let xmlns_attrs = List.map render_xmlns_prefix prefixes_to_add in
      attrs @ xmlns_attrs
    in
    [std_tag name attrs content]
  | Transclude transclusion ->
    render_transclusion_node forest transclusion
  | Contextual_number addr ->
    begin
      match State.get_article addr forest with
      | Some {frontmatter = {number = Some number; _}; _} -> [txt "%s" number]
      | Some _ -> [txt "[%s]" (URI.display_path_string ~base: forest.config.url addr)]
      | None -> []
    end
  | Link link ->
    render_link forest link
  | Section section ->
    [render_section forest section]
  | KaTeX (mode, content) ->
    let body = Plain_text_client.string_of_content ~forest content in
    begin
      match mode with
      | Inline ->
        [span [class_ "math"] [txt ~raw: true "%s" body]]
      | Display ->
        [div [class_ "math"] [txt ~raw: true "%s" body]]
    end
  | Results_of_datalog_query q ->
    (* We could just evaluate the query immediately. This is just experimental*)
    [
      span
        [
          Hx.get "/query";
          Hx.trigger "load";
          Hx.swap "outerHTML";
          Hx.target "this";
          Hx.vals
            "%s"
            Repr.(
              to_json_string
                ~minify: true
                query_t
                {query = q}
            )
        ]
        []
    ]
  | T.Datalog_script _ -> []
  | T.Artefact _
  | T.Uri _
  | T.Route_of_uri _ ->
    [txt "todo"]

(* TODO: links need to be flattened in order to produce valid HTML. *)
and render_link (forest : State.t) (link : T.content T.link) : node list =
  let attrs =
    match State.get_article link.href forest with
    | None ->
      (* TODO: rendering of hrefs is suboptimal... *)
      [
        href "%s" (Format.asprintf "%a" URI.pp link.href);
      ]
    | Some article ->
      begin
        match article.frontmatter.uri with
        | Some _uri ->
          (* A plain link: the body is boosted, so htmx fetches the full page
             (including header and table of contents) and pushes the URL. *)
          [
            title_ "%s" @@
            Option.value ~default: "" @@
            Option.map
              (
                Plain_text_client.string_of_content
                  ~forest
                  ~router: (Legacy_xml_client.route forest)
              )
              article.frontmatter.title;
            href "%s" (route_string link.href);
          ]
        | None -> [HTML.null_]
      end;
  in
  [
    span
      [class_ "link local"]
      [a attrs (render_content forest link.content)]
  ]

let rec render_toc_items (forest : State.t) (content : T.content) : node list =
  let sections = child_sections forest content in
  let@ section, label, ctx = List.filter_map @~ number_siblings forest sections in
  if section.flags.included_in_toc = Some false then None
  else
    Option.some @@
    let@ () = with_ctx ctx in
    let title =
      Option.value ~default: [] @@
      Option.map (render_content forest) section.frontmatter.title
    in
    let title_text =
      Option.value ~default: "" @@
      Option.map
        (Plain_text_client.string_of_content ~forest ~router: (Legacy_xml_client.route forest))
        section.frontmatter.title
    in
    let anchor = Option.map (Format.sprintf "#%s") (section_id section label) in
    let bullet_href, bullet_title =
      match section.frontmatter.uri with
      | Some uri -> route_string uri, Format.sprintf "%s%s[%s]" title_text nbsp (URI.display_path_string ~base: forest.config.url uri)
      | None -> Option.value ~default: "" anchor, title_text
    in
    let children = render_toc_items forest section.mainmatter in
    li
      []
      [
        a [class_ "bullet"; href "%s" bullet_href; title_ "%s" bullet_title] [txt "■"];
        span
          [
            class_ "link local";
            (match anchor with Some a -> string_attr "data-target" "%s" a | None -> null_)
          ]
          (render_taxon_with_number ?label forest section.frontmatter :: title);
        if children = [] then null [] else ul [class_ "block"] children
      ]

(* The table of contents, as in tree.xsl: shown when the mainmatter has
   sections in the table of contents, unless the tree has [\meta{toc}{false}]. *)
let render_toc (forest : State.t) (article : T.content T.article) : node option =
  let@ () = Xmlns.run ~reserved: [] in
  let@ () = with_ctx {root_ctx with ancestors = Option.to_list article.frontmatter.uri} in
  let toc_disabled =
    List.exists (fun (k, v) -> k = "toc" && v = T.Content [T.Text "false"]) article.frontmatter.metas
  in
  match render_toc_items forest article.mainmatter with
  | [] -> None
  | _ when toc_disabled -> None
  | items ->
    Some
      (nav
        [id "toc"]
        [
          div
            [class_ "block"]
            [
              h1 [] [txt "Table of Contents"];
              ul [class_ "block"] items;
            ]
        ])

(* The site header with a link home, shown on every tree except the home tree. *)
let render_header (forest : State.t) (article : T.content T.article) : node =
  let is_home =
    Option.fold ~none: false ~some: (URI.equal (Config.home_uri forest.config)) article.frontmatter.uri
  in
  header
    [class_ "header"]
    (
      if is_home then []
      else [nav [class_ "nav"] [div [class_ "logo"] [a [href "/"; title_ "Home"] [txt "« Home"]]]]
    )

let render_title (forest : State.t) (article : T.content T.article) : string =
  Option.value ~default: "" @@
  Option.map
    (Plain_text_client.string_of_content ~forest ~router: (Legacy_xml_client.route forest))
    article.frontmatter.title

let render_transclusion (forest : State.t) (content : T.content) =
  let@ () = Xmlns.run ~reserved: [] in
  render_content forest content

let render_query_result (forest : State.t) (vs : Vertex_set.t) =
  let@ () = Xmlns.run ~reserved: [] in
  let@ () = with_ctx {root_ctx with in_backmatter = true} in
  let module C = Types.Comparators(struct
    let string_of_content =
      Plain_text_client.string_of_content
        ~forest
        ~router: (route forest)
  end) in
  let make_section =
    T.article_to_section
      ~flags: {T.default_section_flags with
        expanded = Some false;
        numbered = Some false;
        included_in_toc = Some false;
        metadata_shown = Some true
      }
  in
  let nodes =
    vs
    |> Vertex_set.to_seq
    |> Seq.filter_map Vertex.uri_of_vertex
    |> Seq.filter_map (State.get_article @~ forest)
    |> List.of_seq
    |> List.sort C.compare_article
    |> List.map (Fun.compose (render_section forest) make_section)
  in
  if List.length nodes = 0 then None
  else Some (div [class_ "tree-content"] nodes)

let render_query (forest : State.t) query =
  render_query_result forest @@ Forest.run_datalog_query forest.graphs query
