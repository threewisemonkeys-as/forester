(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Pure_html
open HTML

(* Styles for the search palette, which the theme's stylesheet (written for
   the static site's ninja-keys palette) does not cover. *)
let palette_style = {|
.modal-overlay { position: fixed; inset: 0; background: rgba(0,0,0,.25); z-index: 100; display: flex; justify-content: center; align-items: flex-start; padding-top: 12vh; }
.modal-content { background: var(--background-color, #fff); color: inherit; width: min(640px, 92vw); max-height: 70vh; overflow-y: auto; border-radius: 8px; box-shadow: 0 10px 40px rgba(0,0,0,.25); padding: 1em; }
.modal-content input.search { width: 100%; box-sizing: border-box; font-size: 1.1em; padding: .4em .6em; margin-bottom: .5em; }
.modal-content .search-form span { margin-right: 1em; font-size: .9em; }
.modal-content #search-results { list-style: none; padding: 0; margin: .5em 0 0; }
.modal-content #search-results li a { display: block; padding: .3em .5em; border-radius: 4px; text-decoration: none; color: inherit; }
.modal-content #search-results li a:hover, .modal-content #search-results li a:focus { background: rgba(0,0,0,.07); outline: none; }
|}

(* Keyboard shortcuts and table-of-contents jumps, mirroring the static
   theme's forester.js: Cmd/Ctrl+K searches, Cmd/Ctrl+E edits the current
   tree, and clicking a TOC entry scrolls to its section. Ctrl+K itself is
   handled by min.js. *)
let shortcuts_script = {|
document.addEventListener("keydown", (e) => {
  const mod = e.metaKey || e.ctrlKey;
  if (e.metaKey && !e.ctrlKey && e.key === "k") {
    e.preventDefault();
    htmx.ajax("GET", "/searchmenu", "#modal-container");
  } else if (mod && e.key === "e") {
    const edit = document.querySelector("#tree-container .edit-button");
    if (edit) { e.preventDefault(); window.location.href = edit.href; }
  } else if (e.key === "Enter" && e.target.matches && e.target.matches("input.search")) {
    e.preventDefault();
    const first = document.querySelector("#search-results a");
    if (first) first.click();
  } else if ((e.key === "ArrowDown" || e.key === "ArrowUp") && document.querySelector(".modal-overlay")) {
    const links = [...document.querySelectorAll("#search-results a")];
    if (links.length === 0) return;
    e.preventDefault();
    const i = links.indexOf(document.activeElement);
    const next = e.key === "ArrowDown" ? Math.min(i + 1, links.length - 1) : i - 1;
    if (next < 0) document.querySelector("input.search").focus(); else links[next].focus();
  }
});
document.addEventListener("click", (e) => {
  if (e.target.closest("a")) return;
  const link = e.target.closest("nav#toc [data-target^='#']");
  if (!link) return;
  const tree = document.querySelector(link.getAttribute("data-target"));
  if (!tree) return;
  for (let elt = tree; elt; elt = elt.parentNode) if (elt.nodeName === "DETAILS") elt.open = true;
  tree.scrollIntoView();
  history.replaceState(history.state, "", link.getAttribute("data-target"));
});
|}

(* Live reload: poll the server's build id and reload the page when it
   changes (the preview script restarts the server whenever a tree changes).
   While the server is restarting the request fails and polling continues. *)
let live_reload_script = {|
(() => {
  let current = null;
  const poll = async () => {
    try {
      const response = await fetch("/build-id", { cache: "no-store" });
      if (response.ok) {
        const id = (await response.text()).trim();
        if (current === null) current = id;
        else if (id !== current) { location.reload(); return; }
      }
    } catch (_) {}
    setTimeout(poll, 1000);
  };
  poll();
})();
|}

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
          style [] "%s" palette_style;
          script [] "%s" shortcuts_script;
          script [] "%s" live_reload_script;
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
