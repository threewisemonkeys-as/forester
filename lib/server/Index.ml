(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Pure_html
open HTML

let v ?(title_text = "") ?(header_ = header [] []) ?c ?toc () =
  let tree_container =
    match c with
    | Some stuff -> stuff
    | None ->
      article
        [
          id "tree-container";
          Hx.get "/home";
          Hx.trigger "load";
          Hx.target "this";
          Hx.swap "outerHTML";
        ]
        []
  in
  html
    []
    [
      head
        []
        [
          meta [charset "utf-8"];
          meta [name "viewport"; content "width=device-width";];
          link [rel "stylesheet"; href "/style.css";];
          link [rel "icon"; type_ "image/x-icon"; href "/favicon.ico";];
          script [type_ "module"; src "/min.js";] "";
          script [src "/htmx.js"] "";
          link [rel "stylesheet"; href "https://cdn.jsdelivr.net/npm/katex@0.16.21/dist/katex.min.css"; integrity "sha384-zh0CIslj+VczCZtlzBcjt5ppRcsAmDnRem7ESsYwWwg3m/OaJ2l4x7YBZl9Kxxib"; crossorigin `anonymous;];
          script [src "https://cdn.jsdelivr.net/npm/katex@0.16.21/dist/katex.js"; integrity "sha384-CAltQiu9myJj3FAllEacN6FT+rOyXo+hFZKGuR2p4HB8JvJlyUHm31eLfL4eEiJL"; crossorigin `anonymous;] "";
          title [] "%s" title_text;
        ];
      body
        [Hx.boost true;]
        [
          header_;
          div
            [id "grid-wrapper";]
            [
              tree_container;
              Option.value ~default: (null []) toc;
            ];
          div [id "modal-container";] [];
        ];
    ]
