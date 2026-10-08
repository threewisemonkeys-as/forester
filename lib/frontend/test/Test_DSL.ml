(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_frontend.DSL
open Forester_core
open struct module T = Types end

let content_node =
(module struct
  type t = T.content T.content_node
  let equal = (=)
  let pp = T.pp_content_node T.pp_content
end: Alcotest.TESTABLE with type t = T.content T.content_node)

let content =
  p
    [
      ol
        [
          li [txt "First item"];
          li [txt "Second item"]
        ];
      ul
        [
          li [txt "First item"];
          li [txt "Second item"]
        ];
      section ~mainmatter: ([p [txt "section"]]) ();
      em [txt "Emphasized item"];
      strong [txt "Strong text"];
      code [txt "fun _ -> ()"];
      blockquote [txt "blockquote"];
      pre [txt "pre"];
      figure [txt "figure"];
      figcaption [txt "caption"];
      cdata "cdata";
      xml_elt (None, "html") [];
      transclude "foo-001";
      contextual_number "chapter-3";
      katex Inline [txt "a = b"];
      link "https://git.sr.ht/~jonsterling/ocaml-forester" [txt "Forester"];
      artefact [txt "res"];
    ]

let test () =
  Alcotest.(check @@ content_node)
    "works"
    (
      T.prim `P @@
        T.Content
          [
            T.prim `Ol @@
              T.Content
                [
                  T.prim `Li @@ T.Content [Text "First item"];
                  T.prim `Li @@ T.Content [Text "Second item"]
                ];
            T.prim `Ul @@
              T.Content
                [
                  T.prim `Li @@ T.Content [Text "First item"];
                  T.prim `Li @@ T.Content [Text "Second item"]
                ];
            Section
              {
                frontmatter = {
                  uri = None;
                  title = None;
                  dates = [];
                  attributions = [];
                  taxon = None;
                  number = None;
                  designated_parent = None;
                  source_path = None;
                  source_line = None;
                  tags = [];
                  metas = [];
                  last_changed = None
                };
                mainmatter = Content [T.prim `P @@ T.Content [Text "section"]];
                flags = {hidden_when_empty = None; included_in_toc = None; header_shown = None; metadata_shown = (Some false); numbered = None; expanded = None}
              };
            T.prim `Em @@ T.Content [Text "Emphasized item"];
            T.prim `Strong @@ T.Content [Text "Strong text"];
            T.prim `Code @@ T.Content [Text "fun _ -> ()"];
            T.prim `Blockquote @@ T.Content [Text "blockquote"];
            T.prim `Pre @@ T.Content [Text "pre"];
            T.prim `Figure @@ T.Content [Text "figure"];
            T.prim `Figcaption @@ T.Content [Text "caption"];
            CDATA "cdata";
            Xml_elt {name = {prefix = ""; uname = "html"; xmlns = None}; attrs = []; content = (Content [])};
            Transclude {href = URI.of_string_exn "foo-001"; target = Mainmatter};
            Contextual_number (URI.of_string_exn "chapter-3");
            KaTeX (Inline, Content [Text "a = b"]);
            Link {href = URI.of_string_exn "https://git.sr.ht/~jonsterling/ocaml-forester"; content = Content [Text "Forester"]};
            Artefact {hash = ""; content = (Content [Text "res"]); sources = []}
          ]
    )
    content

let () =
  let open Alcotest in
  run "DSL" ["works", [test_case "" `Quick test]]
