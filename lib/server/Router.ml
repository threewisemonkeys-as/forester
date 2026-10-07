(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Routes

type route =
  | Index
  | Font of string
  | Js_bundle
  | Stylesheet
  | Favicon
  | Tree of string
  | Search
  | Searchmenu
  | Nil
  | Home
  | Query
  | Htmx
  | Build_id

let routes : route router =
  one_of
    [
      route (Routes.nil) Index;
      route (s "fonts" / str /? nil) (fun s -> Font s);
      route (s "style.css" /? nil) Stylesheet;
      route (s "min.js" /? nil) Js_bundle;
      route (s "favicon.ico" /? nil) Favicon;
      route (s "trees" / str /? nil) (fun s -> Tree s);
      route (s "search" /? nil) Search;
      route (s "searchmenu" /? nil) Searchmenu;
      route (s "nil" /? nil) Nil;
      route (s "home" /? nil) Home;
      route (s "query" /? nil) Query;
      route (s "htmx.js" /? nil) Htmx;
      route (s "build-id" /? nil) Build_id;
    ]
