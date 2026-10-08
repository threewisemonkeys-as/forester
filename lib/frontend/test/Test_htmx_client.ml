(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(* Tests for the HTML rendered by [forester serve]. *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_frontend
open Forester_test

open struct module T = Types end

let config = Config.default ()

let raw_trees = [
  {
    path = "a.tree";
    content = {|\title{Tree A}
\p{Hello \strong{world}.}
\ul{\li{first}\li{second}}
\<html:span>[class]{note}{annotated}
\pre{\startverb
x_1 = a < b {c}
\stopverb}
|};
  };
  {path = "b.tree"; content = {|\title{Tree \strong{B}}
\p{See [[a]].}
|}};
  {path = "c.tree"; content = {|\title{Tree C}
\p{Not linked to anything.}
|}};
  {path = "index.tree"; content = {|\title{Home}
\p{Welcome.}
|}};
  (* Mirrors the numbering rules of tree.xsl: s2 has a single child section
     with no children of its own, so that child is implicitly unnumbered. *)
  {path = "parent.tree"; content = {|\title{Parent}
\transclude{s1}
\transclude{s2}
\subtree{\title{Inline}\p{Inline body.}
  \subtree{\title{Nested one}}
  \subtree{\title{Nested two}}
}
|}};
  {path = "s1.tree"; content = {|\taxon{Definition}\title{First}
\p{Body of s1.}
\transclude{s1a}
\transclude{s1b}
|}};
  {path = "s1a.tree"; content = {|\title{First A}
\p{Body of s1a.}
|}};
  {path = "s1b.tree"; content = {|\title{First B}
|}};
  {path = "s2.tree"; content = {|\title{Second}
\transclude{s2a}
|}};
  {path = "s2a.tree"; content = {|\title{Second A}
|}};
  {path = "notoc.tree"; content = {|\title{No TOC}
\meta{toc}{false}
\transclude{s1a}
|}};
  {path = "loop.tree"; content = {|\title{Loop}
\transclude{loop}
|}};
]

let uri name = URI_scheme.named_uri ~base: config.url name

let with_forest ~env kont =
  with_test_forest ~env ~raw_trees ~config @@ fun path ->
  Sys.chdir (Eio.Path.native_exn path);
  let@ () = Reporter.easy_run in
  let forest =
    State.make ~env ~config ~dev: true ()
    |> Driver.run_until_done Load_all_configured_dirs
  in
  kont forest

let get_article forest name =
  match State.get_article (uri name) forest with
  | Some article -> article
  | None -> Alcotest.failf "tree %s was not evaluated" name

(* Pure_html wraps long tags across lines; collapse whitespace so tests can match. *)
let normalise str =
  String.split_on_char '\n' str |> List.map String.trim |> String.concat " "

let nodes_to_string nodes = normalise @@ Pure_html.to_string (Pure_html.HTML.div [] nodes)

let contains ~sub str =
  let n = String.length sub and m = String.length str in
  let rec go i = i + n <= m && (String.sub str i n = sub || go (i + 1)) in
  go 0

let check_contains msg ~sub str =
  if not (contains ~sub str) then
    Alcotest.failf "%s: expected to find %S in@.%s" msg sub str

let check_absent msg ~sub str =
  if contains ~sub str then
    Alcotest.failf "%s: did not expect %S in@.%s" msg sub str

(* The visible text of some HTML, with non-breaking spaces as spaces. *)
let text_of html =
  let buf = Buffer.create (String.length html) in
  let in_tag = ref false in
  String.iter
    (fun c ->
      match c with
      | '<' -> in_tag := true
      | '>' -> in_tag := false
      | c when not !in_tag -> Buffer.add_char buf c
      | _ -> ()
    )
    html;
  Buffer.contents buf
  |> Str.global_replace (Str.regexp_string "\u{00A0}") " "
  |> Str.global_replace (Str.regexp "[ \n]+") " "

let render_page forest name =
  let article = get_article forest name in
  let toc = Option.map Pure_html.to_string (Htmx_client.render_toc forest article) in
  normalise @@ Pure_html.to_string (Htmx_client.render_article forest article), toc

let test_inline_transclusions ~env () =
  let@ forest = with_forest ~env in
  let html, _ = render_page forest "parent" in
  check_contains "transcluded body" ~sub: "Body of s1." html;
  check_contains "nested transcluded body" ~sub: "Body of s1a." html;
  check_absent "no lazy transclusions" ~sub: "transclusion:" html;
  check_absent "no lazy requests for trees" ~sub: {|hx-get="/trees|} html

let test_numbering ~env () =
  let@ forest = with_forest ~env in
  let html, _ = render_page forest "parent" in
  let text = text_of html in
  check_contains "taxon and number" ~sub: "Definition 1. First" text;
  check_contains "nested numbers" ~sub: "1.1. First A" text;
  check_contains "second nested number" ~sub: "1.2. First B" text;
  check_contains "second section" ~sub: "2. Second" text;
  check_contains "inline subtree" ~sub: "3. Inline" text;
  check_contains "nested inline subtree" ~sub: "3.2. Nested two" text;
  check_absent "only child without children is unnumbered" ~sub: "2.1. Second A" text;
  check_contains "unnumbered child still rendered" ~sub: "Second A" text;
  check_absent "root is unnumbered" ~sub: ". Parent" text

let test_toc ~env () =
  let@ forest = with_forest ~env in
  begin
    match render_page forest "parent" with
    | _, None -> Alcotest.fail "Parent should have a table of contents"
    | _, Some toc ->
      let text = text_of toc in
      check_contains "heading" ~sub: "Table of Contents" text;
      check_contains "numbered entry" ~sub: "Definition 1. First" text;
      check_contains "nested entry" ~sub: "1.2. First B" text;
      check_contains "entry links to tree" ~sub: {|href="/trees/s1a/"|} toc;
      check_contains "inline subtree anchor" ~sub: {|data-target="#section-3-2"|} toc;
      check_absent "anchors are valid CSS selectors" ~sub: {|#section-3.2|} toc
  end;
  Alcotest.(check (option string)) "leaf has no TOC" None (snd (render_page forest "s1a"));
  Alcotest.(check (option string)) "toc=false is respected" None (snd (render_page forest "notoc"))

let test_loop ~env () =
  let@ forest = with_forest ~env in
  let html, _ = render_page forest "loop" in
  check_contains "loop is reported" ~sub: "Transclusion loop detected" html

let test_header ~env () =
  let@ forest = with_forest ~env in
  let header name = Pure_html.to_string (Htmx_client.render_header forest (get_article forest name)) in
  check_contains "link home" ~sub: "« Home" (header "parent");
  check_absent "no link home on the home tree" ~sub: "« Home" (header "index");
  Alcotest.(check string) "page title" "Parent" (Htmx_client.render_title forest (get_article forest "parent"))

let test_slug ~env () =
  let@ forest = with_forest ~env in
  let html, _ = render_page forest "s1a" in
  check_contains "slug shows the short address" ~sub: {|href="/trees/s1a/">[s1a]</a>|} html

let test_xhtml_names () =
  let xhtml = Some "http://www.w3.org/1999/xhtml" in
  let check = Alcotest.(check string) in
  check "xhtml namespace" "p" (Htmx_client.render_xml_qname {prefix = "html"; uname = "p"; xmlns = xhtml});
  check "html prefix" "li" (Htmx_client.render_xml_qname {prefix = "html"; uname = "li"; xmlns = None});
  check "no prefix" "ul" (Htmx_client.render_xml_qname {prefix = ""; uname = "ul"; xmlns = None});
  check "foreign prefix kept" "mml:mi" (Htmx_client.render_xml_qname {prefix = "mml"; uname = "mi"; xmlns = Some "http://www.w3.org/1998/Math/MathML"})

let test_mainmatter_is_html ~env () =
  let@ forest = with_forest ~env in
  let article = get_article forest "a" in
  let html = nodes_to_string (Htmx_client.render_transclusion forest article.mainmatter) in
  check_absent "no XHTML prefixes" ~sub: "html:" html;
  check_contains "paragraph" ~sub: "<p>Hello <strong>world</strong>.</p>" html;
  check_contains "list" ~sub: "<ul><li>first</li><li>second</li></ul>" html;
  check_contains "attribute value" ~sub: {|<span class="note">annotated</span>|} html;
  check_absent "no placeholder attributes" ~sub: "todo" html;
  check_absent "verbatim is not a CDATA section" ~sub: "CDATA" html;
  check_contains "verbatim is escaped text" ~sub: "x_1 = a &lt; b {c}" html

let test_article_is_html ~env () =
  let@ forest = with_forest ~env in
  let html = normalise @@ Pure_html.to_string (Htmx_client.render_article forest (get_article forest "a")) in
  check_absent "no XHTML prefixes" ~sub: "html:" html;
  check_contains "slug link" ~sub: {|href="/trees/a/"|} html;
  check_contains "edit link opens Zed" ~sub: {|href="zed://file/|} html

let test_subtree_edit_links_have_line ~env () =
  let@ forest = with_forest ~env in
  let html = normalise @@ Pure_html.to_string (Htmx_client.render_article forest (get_article forest "parent")) in
  check_contains "tree edit link has no line" ~sub: {|parent.tree"|} html;
  check_contains "subtree edit link has its line" ~sub: {|parent.tree:4"|} html;
  check_contains "nested subtree edit link has its line" ~sub: {|parent.tree:6"|} html

(* The /query endpoint receives the query as JSON in a request parameter. *)
let roundtrip_query q =
  let query_repr = Datalog_expr.(query_t Repr.string (T.vertex_t T.content_t)) in
  match Repr.of_json_string query_repr (Repr.to_json_string query_repr q) with
  | Ok q -> q
  | Error (`Msg msg) -> Alcotest.failf "query did not parse: %s" msg

let render_backlinks forest name =
  Builtin_queries.backlinks_datalog (T.Uri_vertex (uri name))
  |> roundtrip_query
  |> Htmx_client.render_query forest
  |> Option.map (Fun.compose normalise Pure_html.to_string)

let test_backlinks ~env () =
  let@ forest = with_forest ~env in
  match render_backlinks forest "a" with
  | None -> Alcotest.fail "tree A should have a backlink from tree B"
  | Some html ->
    check_contains "backlink to B" ~sub: "Tree <strong>B</strong>" html;
    check_contains "link to B" ~sub: "/trees/b/" html;
    check_absent "no unrelated trees" ~sub: "Tree C" html;
    check_absent "no XHTML prefixes" ~sub: "html:" html

let test_no_backlinks ~env () =
  let@ forest = with_forest ~env in
  Alcotest.(check (option string)) "tree C has no backlinks" None (render_backlinks forest "c")

let test_backmatter_queries_run ~env () =
  let@ forest = with_forest ~env in
  let article = get_article forest "a" in
  let queries =
    let@ node = List.filter_map @~ T.extract_content article.backmatter in
    match node with
    | T.Section {mainmatter; _} ->
      List.find_map (function T.Results_of_datalog_query q -> Some q | _ -> None) (T.extract_content mainmatter)
    | _ -> None
  in
  Alcotest.(check bool) "backmatter has queries" true (List.length queries > 0);
  let results = List.filter_map (fun q -> Htmx_client.render_query forest (roundtrip_query q)) queries in
  Alcotest.(check bool) "some backmatter section is non-empty" true (List.length results > 0)

let () =
  let@ env = Eio_main.run in
  let open Alcotest in
  run
    "Htmx_client"
    [
      "XHTML output",
      [
        test_case "XHTML names lose their prefix" `Quick test_xhtml_names;
        test_case "mainmatter renders as HTML" `Quick (test_mainmatter_is_html ~env);
        test_case "article renders as HTML" `Quick (test_article_is_html ~env);
        test_case "subtree edit links include the line" `Quick (test_subtree_edit_links_have_line ~env);
      ];
      "page structure",
      [
        test_case "transclusions are inlined" `Quick (test_inline_transclusions ~env);
        test_case "sections are numbered like tree.xsl" `Quick (test_numbering ~env);
        test_case "table of contents" `Quick (test_toc ~env);
        test_case "transclusion loops are reported" `Quick (test_loop ~env);
        test_case "header and title" `Quick (test_header ~env);
        test_case "slug" `Quick (test_slug ~env);
      ];
      "queries",
      [
        test_case "backlinks are rendered" `Quick (test_backlinks ~env);
        test_case "empty query renders nothing" `Quick (test_no_backlinks ~env);
        test_case "backmatter queries evaluate" `Quick (test_backmatter_queries_run ~env);
      ];
    ]
