(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(* End-to-end tests for [forester serve]: start the real request handler on
   a loopback port and talk to it over HTTP, the way htmx does. *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_server
open Forester_test

open struct module T = Types end

let config = Config.default ()

let raw_trees = [
  {path = "a.tree"; content = {|\title{Tree A}
\p{Hello \strong{world}.}
\ul{\li{first}\li{second}}
|}};
  {path = "b.tree"; content = {|\title{Tree \strong{B}}
\p{See [[a]].}
|}};
  {path = "c.tree"; content = {|\title{Tree C}
\p{Not linked to anything.}
|}};
  {path = "index.tree"; content = {|\title{Home}
\p{Welcome.}
|}};
  {path = "parent.tree"; content = {|\title{Parent}
\transclude{a}
\transclude{c}
\subtree{\title{Inline}\p{Mentions Kepler.}}
|}};
]

let theme : Server.theme = {
  stylesheet = "";
  htmx = "";
  js_bundle = "";
  font_dir = "";
  favicon = "";
}

let contains ~sub str =
  let n = String.length sub and m = String.length str in
  let rec go i = i + n <= m && (String.sub str i n = sub || go (i + 1)) in
  go 0

let check_contains msg ~sub str =
  if not (contains ~sub str) then Alcotest.failf "%s: expected to find %S in@.%s" msg sub str

let check_absent msg ~sub str =
  if contains ~sub str then Alcotest.failf "%s: did not expect %S in@.%s" msg sub str

type response = {status: int; headers: Http.Header.t; body: string}

(* Build the forest, serve it on an ephemeral port and pass a [get] function
   to [kont]. The server is cancelled once [kont] returns. *)
let with_server ~env kont =
  with_test_forest ~env ~raw_trees ~config @@ fun path ->
  Sys.chdir (Eio.Path.native_exn path);
  let forest =
    let@ () = Reporter.easy_run in
    State.make ~env ~config ~dev: true ()
    |> Driver.run_until_done Load_all_configured_dirs
  in
  let@ sw = Eio.Switch.run ?name: None in
  let socket =
    Eio.Net.listen env#net ~sw ~backlog: 16 ~reuse_addr: true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | _ -> assert false
  in
  let client = Cohttp_eio.Client.make ~https: None env#net in
  let post ?(headers = []) path form =
    let@ sw = Eio.Switch.run ?name: None in
    let uri = Uri.of_string (Format.sprintf "http://127.0.0.1:%i%s" port path) in
    let headers = Http.Header.of_list (("Content-Type", "application/x-www-form-urlencoded") :: headers) in
    let body = Cohttp_eio.Body.of_string (Uri.encoded_of_query (List.map (fun (k, v) -> k, [v]) form)) in
    let resp, body = Cohttp_eio.Client.post client ~sw ~headers ~body uri in
    let body = Eio.Buf_read.(parse_exn take_all) body ~max_size: max_int in
    {status = Http.Status.to_int resp.status; headers = resp.headers; body}
  in
  let get ?(headers = []) path =
    let@ sw = Eio.Switch.run ?name: None in
    let uri = Uri.of_string (Format.sprintf "http://127.0.0.1:%i%s" port path) in
    let resp, body = Cohttp_eio.Client.get client ~sw ~headers: (Http.Header.of_list headers) uri in
    let body = Eio.Buf_read.(parse_exn take_all) body ~max_size: max_int in
    {status = Http.Status.to_int resp.status; headers = resp.headers; body}
  in
  let server = Cohttp_eio.Server.make ~callback: (Server.handler ~env ~theme ~forest) () in
  Eio.Fiber.first
    (fun () -> Cohttp_eio.Server.run socket server ~on_error: raise)
    (fun () -> kont (get, post))

let htmx = ["HX-Request", "true"]

let test_article ~env () =
  let@ get, _ = with_server ~env in
  let r = get ~headers: htmx "/trees/a/" in
  Alcotest.(check int) "status" 200 r.status;
  check_contains "paragraph" ~sub: "<p>Hello <strong>world</strong>.</p>" r.body;
  check_absent "no XHTML prefixes" ~sub: "html:" r.body;
  check_contains "slug link" ~sub: {|href="/trees/a/"|} r.body;
  check_contains "edit link opens Zed" ~sub: "zed://file/" r.body

let check_full_page r =
  Alcotest.(check int) "status" 200 r.status;
  Alcotest.(check (option string)) "content type" (Some "text/html; charset=utf-8") (Http.Header.get r.headers "Content-Type");
  check_contains "full page" ~sub: "<html" r.body;
  check_contains "charset" ~sub: {|charset="utf-8"|} r.body

let test_full_page ~env () =
  let@ get, _ = with_server ~env in
  let r = get "/trees/parent/" in
  check_full_page r;
  check_contains "title" ~sub: "<title>Parent</title>" r.body;
  check_contains "link home" ~sub: "« Home" r.body;
  check_contains "table of contents" ~sub: {|id="toc"|} r.body;
  check_contains "transcluded content is inline" ~sub: "<p>Hello <strong>world</strong>.</p>" r.body

(* Link clicks are boosted by htmx and swap the whole body, so they must get
   the full page (with header and table of contents), not a fragment. *)
let test_boosted ~env () =
  let@ get, _ = with_server ~env in
  let r = get ~headers: (("HX-Boosted", "true") :: htmx) "/trees/parent/" in
  check_full_page r;
  check_contains "table of contents" ~sub: {|id="toc"|} r.body

let test_home ~env () =
  let@ get, _ = with_server ~env in
  let r = get "/" in
  check_full_page r;
  check_contains "home tree" ~sub: "<title>Home</title>" r.body;
  check_contains "home content" ~sub: "Welcome." r.body;
  check_absent "no link home on the home page" ~sub: "« Home" r.body

let test_charsets ~env () =
  let@ get, _ = with_server ~env in
  let content_type path = Http.Header.get (get path).headers "Content-Type" in
  Alcotest.(check (option string)) "stylesheet" (Some "text/css; charset=utf-8") (content_type "/style.css");
  Alcotest.(check (option string)) "script" (Some "application/javascript; charset=utf-8") (content_type "/min.js")

let test_transclusion ~env () =
  let@ get, _ = with_server ~env in
  let r = get ~headers: (("Mainmatter", "true") :: htmx) "/trees/a/" in
  Alcotest.(check int) "status" 200 r.status;
  check_contains "transcluded body" ~sub: "<li>first</li>" r.body

(* The backmatter asks for each query with hx-get="/query" and hx-vals. *)
let query_path query =
  let query_repr = Datalog_expr.(query_t Repr.string (T.vertex_t T.content_t)) in
  "/query?query=" ^ Uri.pct_encode ~component: `Query_value (Repr.to_json_string query_repr query)

let backlinks name =
  query_path @@ Builtin_queries.backlinks_datalog (T.Uri_vertex (URI_scheme.named_uri ~base: config.url name))

let test_backlinks ~env () =
  let@ get, _ = with_server ~env in
  let r = get ~headers: htmx (backlinks "a") in
  Alcotest.(check int) "status" 200 r.status;
  check_contains "backlink from B" ~sub: "Tree <strong>B</strong>" r.body;
  check_contains "link to B" ~sub: "/trees/b/" r.body;
  check_absent "no unrelated trees" ~sub: "Tree C" r.body

let test_empty_query ~env () =
  let@ get, _ = with_server ~env in
  let r = get ~headers: htmx (backlinks "c") in
  Alcotest.(check int) "status" 200 r.status;
  Alcotest.(check (option string)) "section is deleted" (Some "delete") (Http.Header.get r.headers "Hx-Swap")

let test_backmatter_queries ~env () =
  let@ get, _ = with_server ~env in
  let page = (get ~headers: htmx "/trees/a/").body in
  let re = Str.regexp {|hx-vals='\([^']*\)'|} in
  let rec collect pos acc =
    match Str.search_forward re page pos with
    | exception Not_found -> List.rev acc
    | _ -> collect (Str.match_end ()) (Str.matched_group 1 page :: acc)
  in
  let vals = collect 0 [] in
  Alcotest.(check bool) "backmatter requests queries" true (List.length vals > 0);
  let bodies =
    let@ v = List.map @~ vals in
    let json = Yojson.Safe.from_string v in
    let query = Yojson.Safe.to_string (Yojson.Safe.Util.member "query" json) in
    let r = get ~headers: htmx ("/query?query=" ^ Uri.pct_encode ~component: `Query_value query) in
    Alcotest.(check int) "query status" 200 r.status;
    r.body
  in
  Alcotest.(check bool) "Backlinks section is filled" true (List.exists (contains ~sub: "/trees/b/") bodies)

let search_titles (post : ?headers: (string * string) list -> string -> (string * string) list -> response) form =
  let r = post ~headers: htmx "/search" form in
  Alcotest.(check int) "status" 200 r.status;
  r.body

let test_search ~env () =
  let@ _, post = with_server ~env in
  let all = search_titles post ["search", ""; "search-for", "title"] in
  List.iter (fun t -> check_contains "empty search lists every tree" ~sub: t all) ["Tree A"; "Parent"; "Home"];
  let r = search_titles post ["search", "tree c"; "search-for", "title"] in
  check_contains "title match" ~sub: {|href="/trees/c/"|} r;
  check_absent "other trees excluded" ~sub: {|href="/trees/a/"|} r;
  let r = search_titles post ["search", "kepler"; "search-for", "title"] in
  check_absent "title search ignores bodies" ~sub: {|href="/trees/parent/"|} r;
  let r = search_titles post ["search", "kepler"; "search-for", "full-text"] in
  check_contains "full text includes inline subtrees" ~sub: {|href="/trees/parent/"|} r;
  let r = search_titles post ["search", "hello"; "search-for", "full-text"] in
  check_contains "full text match" ~sub: {|href="/trees/a/"|} r;
  check_absent "transcluded text is found under its own tree" ~sub: {|href="/trees/parent/"|} r

let test_search_menu ~env () =
  let@ get, _ = with_server ~env in
  let r = get ~headers: htmx "/searchmenu" in
  Alcotest.(check int) "status" 200 r.status;
  check_contains "search form" ~sub: {|hx-post="/search"|} r.body

let () =
  let@ env = Eio_main.run in
  let open Alcotest in
  run
    "Server"
    [
      "trees",
      [
        test_case "htmx article" `Quick (test_article ~env);
        test_case "full page" `Quick (test_full_page ~env);
        test_case "boosted navigation gets full page" `Quick (test_boosted ~env);
        test_case "home page" `Quick (test_home ~env);
        test_case "charsets" `Quick (test_charsets ~env);
        test_case "transclusion" `Quick (test_transclusion ~env);
      ];
      "queries",
      [
        test_case "backlinks" `Quick (test_backlinks ~env);
        test_case "empty query deletes section" `Quick (test_empty_query ~env);
        test_case "backmatter queries round-trip" `Quick (test_backmatter_queries ~env);
      ];
      "search",
      [
        test_case "search menu" `Quick (test_search_menu ~env);
        test_case "search results" `Quick (test_search ~env);
      ];
    ]
