(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_xml_names
open State.Syntax

open struct
  module T = Types
  module P = Pure_html
  module X = Xml_forester
end

type env = {
  forest: State.t;
  scope: URI.t option;
  section_depth: int;
  loops: Loop_detection.t;
  xmlns: Xmlns.t;
}

let hx ~env attrs children =
  P.std_tag (Format.sprintf "h%i" @@ min 6 env.section_depth) attrs children

let route uri = URI.to_string uri

let get_expanded_title ~env frontmatter forest =
  State.get_expanded_title ?scope:env.scope
    ~flags:T.{empty_when_untitled = true}
    frontmatter forest

let render_xml_qname qname =
  match qname.prefix with
  | "" -> qname.uname
  | _ ->
    if qname.xmlns = Some "http://www.w3.org/1999/xhtml" then qname.uname
    else Format.sprintf "%s:%s" qname.prefix qname.uname

let render_xml_attr ~env T.{key; value} =
  let str_value =
    Plain_text_client.string_of_content ~forest:env.forest value
  in
  P.string_attr (render_xml_qname key) "%s" str_value

let render_xmlns_prefix ({prefix; xmlns} : Forester_xml_names.xmlns_attr) =
  let attr = match prefix with "" -> "xmlns" | _ -> "xmlns:" ^ prefix in
  P.string_attr attr "%s" xmlns

let rec render_content ~env (Content content : T.content) : P.node list =
  match content with
  | T.Text txt0 :: T.Text txt1 :: content ->
    render_content ~env @@ Content (T.Text (txt0 ^ txt1) :: content)
  | node :: content ->
    let xs = render_content_node ~env node in
    let ys = render_content ~env (Content content) in
    xs @ ys
  | [] -> []

and render_content_node ~env (node : 'a T.content_node) : P.node list =
  let config = env.forest.config in
  match node with
  | Text str -> [P.txt "%s" str]
  | CDATA str -> [P.txt ~raw:true "<![CDATA[%s]]>" str]
  | Uri uri -> [P.txt "%s" (URI.to_string uri)]
  | Xml_elt elt ->
    let name = render_xml_qname elt.name in
    let xmlns_attrs = Xmlns.xmlns_attrs_for_elt elt env.xmlns in
    let env = {env with xmlns = Xmlns.extend xmlns_attrs env.xmlns} in
    let content = render_content ~env elt.content in
    [
      P.std_tag name
        (List.map render_xmlns_prefix xmlns_attrs
        @ List.map (render_xml_attr ~env) elt.attrs)
        content;
    ]
  | Route_of_uri uri -> [P.txt "%s" (route uri)]
  | Contextual_number uri ->
    let custom_number =
      let@ resource = Option.bind @@ env.forest.@{uri} in
      match resource with
      | T.Article article -> article.frontmatter.number
      | _ -> None
    in
    begin match custom_number with
    | None -> [P.txt "%s" @@ URI.relative_path_string ~base:config.url uri]
    | Some num -> [P.txt "%s" num]
    end
  | KaTeX (_, content) -> [P.HTML.code [] @@ render_content ~env content]
  | Artefact artefact -> render_content ~env @@ artefact.content
  | Section section -> render_section ~env section
  | Transclude transclusion -> render_transclusion ~env transclusion
  | Link link -> render_link ~env link
  | Results_of_datalog_query _ -> [] (* TODO: just make a list of links *)
  | Datalog_script _ -> []

and render_link ~env (link : T.content T.link) : P.node list =
  [
    P.HTML.a [P.HTML.href "%s" (Format.asprintf "%a" URI.pp link.href)]
    @@ render_content ~env link.content;
  ]

and render_transclusion ~env (transclusion : T.transclusion) : P.node list =
  match State.get_content_of_transclusion transclusion env.forest with
  | None -> Reporter.fatal (Resource_not_found transclusion.href)
  | Some content -> render_content ~env content

and render_section ~env (section : T.content T.section) : P.node list =
  let env =
    {
      env with
      section_depth = env.section_depth + 1;
      scope = section.frontmatter.uri;
    }
  in
  [
    P.HTML.section []
      [
        begin match section.frontmatter.title with
        | None -> P.HTML.null []
        | Some title ->
          P.HTML.header [] [hx ~env [] @@ render_content ~env title]
        end;
        begin if
          Loop_detection.have_seen_uri_opt section.frontmatter.uri env.loops
        then P.txt "Transclusion loop detected, rendering stopped."
        else
          P.HTML.null
          @@ render_content
               ~env:
                 {
                   env with
                   loops =
                     Loop_detection.add_seen_uri_opt section.frontmatter.uri
                       env.loops;
                 }
               section.mainmatter
        end;
      ];
  ]

let render_article_as_div ?(heading_level = 0) (forest : State.t)
    (article : T.content T.article) : P.node =
  let reserved = [{prefix = ""; xmlns = "http://www.w3.org/1999/xhtml"}] in
  let env =
    {
      forest;
      section_depth = heading_level;
      scope = article.frontmatter.uri;
      loops = Loop_detection.empty;
      xmlns = Xmlns.init ~reserved;
    }
  in
  P.HTML.div
    (List.map render_xmlns_prefix reserved)
    [
      P.HTML.null
      @@ render_content
           ~env:
             {
               env with
               loops =
                 Loop_detection.add_seen_uri_opt article.frontmatter.uri
                   env.loops;
             }
           article.mainmatter;
    ]
