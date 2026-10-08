(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_xml_names
open Base

type xml_qname = Forester_xml_names.xml_qname = {prefix: string; uname: string; xmlns: string option}

type 'content vertex =
  | Uri_vertex of URI.t
  | Content_vertex of 'content
[@@deriving show, repr]

type section_flags = {
  hidden_when_empty: bool option;
  included_in_toc: bool option;
  header_shown: bool option;
  metadata_shown: bool option;
  numbered: bool option;
  expanded: bool option
}
[@@deriving show, repr]

type title_flags = {empty_when_untitled: bool}
[@@deriving show, repr]

let default_section_flags = {
  hidden_when_empty = None;
  included_in_toc = None;
  header_shown = None;
  metadata_shown = Some false;
  numbered = None;
  expanded = None
}

type 'content xml_attr = {key: xml_qname; value: 'content}
[@@deriving show, repr]

type 'content xml_elt = {
  name: xml_qname;
  attrs: 'content xml_attr list;
  content: 'content
}
[@@deriving show, repr]

type attribution_role =
  | Author
  | Contributor
[@@deriving show, repr]

type 'content attribution = {
  role: attribution_role;
  vertex: 'content vertex
}
[@@deriving show, repr]

type 'content frontmatter = {
  uri: URI.t option;
  title: 'content option;
  dates: Human_datetime.t list;
  attributions: 'content attribution list;
  taxon: 'content option;
  number: string option;
  designated_parent: URI.t option;
  source_path: string option;
  source_line: int option;
  tags: 'content vertex list;
  metas: (string * 'content) list;
  last_changed: float option;
}
[@@deriving show, repr]

type 'content section = {
  frontmatter: 'content frontmatter;
  mainmatter: 'content;
  flags: section_flags
}
[@@deriving show, repr]

type 'content article = {
  frontmatter: 'content frontmatter;
  mainmatter: 'content;
  backmatter: 'content;
}
[@@deriving show, repr]

type asset = {uri: URI.t; content: string}
[@@deriving show, repr]

type 'a json_blob_syndication = {blob_uri: URI.t; query: (string, 'a vertex) Datalog_expr.query}
[@@deriving show, repr]

type atom_feed_syndication = {source_uri: URI.t; feed_uri: URI.t}
[@@deriving show, repr]

type 'content syndication =
  | Json_blob of 'content json_blob_syndication
  | Atom_feed of atom_feed_syndication
[@@deriving show, repr]

type 'content resource =
  | Article of 'content article
  | Asset of asset
  | Syndication of 'content syndication
[@@deriving show, repr]

type 'content forest = 'content resource list
[@@deriving show, repr]

type content_target =
  | Full of section_flags
  | Mainmatter
  | Title of title_flags
  | Taxon
[@@deriving show, repr]

type transclusion = {
  href: URI.t;
  target: content_target
}
[@@deriving show, repr]

type 'content link = {
  href: URI.t;
  content: 'content
}
[@@deriving show, repr]

type artefact_source = {
  type_: string;
  part: string;
  source: string
}
[@@deriving show, repr]

type 'content artefact = {
  hash: string;
  content: 'content;
  sources: artefact_source list
}
[@@deriving show, repr]

type 'content content_node =
  | Text of string
  | CDATA of string
  | Xml_elt of 'content xml_elt
  | Transclude of transclusion
  | Contextual_number of URI.t
  | Section of 'content section
  | KaTeX of math_mode * 'content
  | Link of 'content link
  | Artefact of 'content artefact
  | Uri of URI.t
  | Route_of_uri of URI.t
  | Datalog_script of (string, 'content vertex) Datalog_expr.script
  | Results_of_datalog_query of (string, 'content vertex) Datalog_expr.query
[@@deriving show, repr]

type content =
  Content of content content_node list
[@@deriving show, repr]

let rec compress_nodes = function
  | [] -> []
  | Text x :: Text y :: ys -> compress_nodes (Text (x ^ y) :: ys)
  | x :: xs -> x :: compress_nodes xs

let compress_content = function
  | Content nodes -> Content (compress_nodes nodes)

let concat_compressed_content (Content xs) (Content ys) =
  let rec loop xs ys =
    match xs, ys with
    | Bwd.Emp, ys -> ys
    | _, [] -> Bwd.prepend xs []
    | Bwd.Snoc (xs, Text x), Text y :: ys -> loop xs (Text (x ^ y) :: ys)
    | Bwd.Snoc (xs, x), ys -> loop xs (x :: ys)
  in
  Content (loop (Bwd.append Bwd.Emp xs) ys)

let html_elt uname (content : 'content) : 'content content_node =
  let name = {prefix = "html"; uname; xmlns = Some "http://www.w3.org/1999/xhtml"} in
  Xml_elt {content; name; attrs = []}

let prim (p : Prim.t) : 'content -> 'content content_node =
  html_elt @@
    match p with
    | `P -> "p"
    | `Ol -> "ol"
    | `Ul -> "ul"
    | `Li -> "li"
    | `Figure -> "figure"
    | `Figcaption -> "figcaption"
    | `Em -> "em"
    | `Strong -> "strong"
    | `Blockquote -> "blockquote"
    | `Pre -> "pre"
    | `Code -> "code"

let map_content f = function Content nodes -> Content (f nodes)
let extract_content = function Content nodes -> nodes

type dx_query = (string, content vertex) Datalog_expr.query [@@deriving show]

let is_whitespace node =
  match node with
  | Text txt -> String.trim txt = ""
  | _ -> false

let strip_whitespace =
  map_content @@
  List.filter @@
  Fun.compose not is_whitespace

let trim_whitespace xs =
  let rec trim_front = function
    | x :: xs when is_whitespace x -> trim_front xs
    | xs -> xs
  and trim_back xs = List.rev @@ trim_front @@ List.rev xs
  in
  trim_back @@ trim_front xs

let default_frontmatter ?uri ?source_path ?source_line ?designated_parent ?(dates = []) ?(attributions = []) ?taxon ?number ?(metas = []) ?(tags = []) ?title ?last_changed () = {uri; source_path; source_line; designated_parent; dates; attributions; taxon; number; metas; tags; title; last_changed}

let article_to_section ?(flags = default_section_flags) (article : 'a article) =
  let mainmatter =
    match article.frontmatter.uri with
    | Some href -> Content [Transclude {href; target = Mainmatter}]
    | None -> article.mainmatter
  in
  {
    frontmatter = article.frontmatter;
    mainmatter;
    flags
  }

let uri_for_syndication = function
  | Atom_feed feed -> Some feed.feed_uri
  | Json_blob blob -> Some blob.blob_uri

let uri_for_resource = function
  | Article article -> article.frontmatter.uri
  | Asset asset -> Some asset.uri
  | Syndication syndication -> uri_for_syndication syndication

module Comparators (I : sig val string_of_content : content -> string end) = struct
  let compare_content =
    Compare.under I.string_of_content String.compare

  let compare_frontmatter =
    let latest_date (fm : content frontmatter) =
      let sorted_dates = fm.dates |> List.sort @@ Compare.invert Human_datetime.compare in
      List.nth_opt sorted_dates 0
    in
    let by_date = Fun.flip @@ Compare.under latest_date @@ Compare.option Human_datetime.compare in
    let by_title = Compare.option compare_content |> Compare.under @@ fun fm -> fm.title in
    let by_parent = compare |> Compare.under @@ fun fm -> Option.is_some fm.designated_parent in
    Compare.cascade by_parent @@ Compare.cascade by_date by_title

  let compare_article = compare_frontmatter |> Compare.under @@ fun x -> x.frontmatter
end
