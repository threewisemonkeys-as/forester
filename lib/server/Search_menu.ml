(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler
open Forester_frontend

open Forester_prelude
open Pure_html
open HTML

open struct module T = Types end

let contains ~sub str =
  let n = String.length sub and m = String.length str in
  let rec go i = i + n <= m && (String.sub str i n = sub || go (i + 1)) in
  go 0

(* The text of a tree's own content, including its inline subtrees but not
   trees it transcludes (those are found under their own address). *)
let rec own_text ~to_string (T.Content nodes) =
  String.concat " " @@
    let@ node = List.map @~ nodes in
    match node with
    | T.Section section ->
      Option.fold ~none: "" ~some: to_string section.frontmatter.title ^ " " ^ own_text ~to_string section.mainmatter
    | T.Transclude {target = Full _ | Mainmatter; _} -> ""
    | T.Xml_elt elt -> own_text ~to_string elt.content
    | node -> to_string (T.Content [node])

(* Trees whose title, taxon or address (and, for a full-text search, body)
   contain every word of [term]. An empty term lists every tree, like the
   command palette of the static site. *)
let search (forest : State.t) ~full_text (term : string) : URI.t list =
  let to_string = Plain_text_client.string_of_content ~forest ~router: (Legacy_xml_client.route forest) in
  let words =
    String.split_on_char ' ' (String.lowercase_ascii term)
    |> List.filter (( <> ) "")
  in
  let matches =
    let@ article = Seq.filter_map @~ State.get_all_articles forest in
    let@ uri = Option.bind article.T.frontmatter.uri in
    let title = Option.fold ~none: "" ~some: to_string article.frontmatter.title in
    let haystack =
      String.lowercase_ascii @@
        String.concat
          " "
          [
            Option.fold ~none: "" ~some: to_string article.frontmatter.taxon;
            title;
            URI.display_path_string ~base: forest.config.url uri;
            if full_text then own_text ~to_string article.mainmatter else "";
          ]
    in
    if List.for_all (fun sub -> contains ~sub haystack) words then Some (String.lowercase_ascii title, uri)
    else None
  in
  matches
  |> List.of_seq
  |> List.sort (fun (t1, u1) (t2, u2) -> match String.compare t1 t2 with 0 -> URI.compare u1 u2 | c -> c)
  |> List.map snd

let v =
  let markup =
    div
      [
        class_ "modal-overlay";
        Hx.trigger "click target:.modal-overlay, keyup[key=='Escape'] from:body";
        Hx.target "#modal-container";
        Hx.get "/nil";
      ]
      [
        div
          [class_ "modal-content";]
          [
            form
              [
                class_ "search-form";
                Hx.post "/search";
                Hx.trigger "input changed delay:200ms, search, load";
                Hx.target "#search-results";
                Hx.swap "outerHTML";
              ]
              [
                input
                  [
                    autofocus;
                    class_ "search";
                    type_ "search";
                    name "search";
                    placeholder "Start typing a note title or ID";
                  ];
                span
                  []
                  [
                    input [type_ "radio"; name "search-for"; value "title"; id "title-text"; checked];
                    label [for_ "title-text"] [txt "Titles"];
                  ];
                span
                  []
                  [
                    input [type_ "radio"; name "search-for"; value "full-text"; id "full-text"];
                    label [for_ "full-text"] [txt "Full text"];
                  ];
              ];
            ul
              [id "search-results";]
              [];
          ];
      ]
  in
  Pure_html.to_string markup

let results (forest : State.t) (links : URI.t list) =
  Pure_html.to_string @@
    ul
      [id "search-results"]
      (
        List.filter_map
          (fun uri ->
            let title = State.get_content_of_transclusion {href = uri; target = Title {empty_when_untitled = false}} forest in
            Option.map
              (fun t ->
                li
                  []
                  [
                    a
                      [class_ "search-result-item"; href "/trees%s" (URI.path_string uri)] @@
                      Htmx_client.render_transclusion forest t @
                      [txt " "; span [class_ "slug"] [txt "[%s]" (URI.display_path_string ~base: forest.config.url uri)]]
                  ]
              )
              title
          )
          links
      )
