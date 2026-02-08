(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude

let file_uri ~base ~ext name = URI.resolve ~base @@ URI.make ~path:[name ^ ext] ()

let named_uri ~base name =
  URI.resolve ~base
  @@
  let path = if Filename.extension name = "" then [name; ""] else [name] in
  URI.make ~path ()

let last_segment str = str |> String.split_on_char '/' |> List.rev |> List.hd

let name (uri : URI.t) : string option =
  uri |> URI.path_components
  |> List.filter (fun x -> not (x = ""))
  |> List.rev |> List.nth_opt @~ 0

let split_addr (uri : URI.t) : (string option * int) option =
  let@ name = Option.bind @@ name uri in
  (* primitively check for address of form YYYY-MM-DD *)
  let date_regex =
    Str.regexp {|^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$|}
  in
  if Str.string_match date_regex name 0 then None
  else
    match String.rindex_opt name '-' with
    | Some i ->
      let prefix = String.sub name 0 i
      and suffix = String.sub name (i + 1) (String.length name - i - 1) in
      let@ key = Option.map @~ BaseN.Base36.int_of_string suffix in
      (Some prefix, key)
    | _ ->
      let@ key = Option.map @~ BaseN.Base36.int_of_string name in
      (None, key)

let lsp_uri_to_uri ~(base : URI.t) (uri : Lsp.Uri.t) : URI.t =
  let uri =
    uri |> Lsp.Uri.to_path |> Filename.chop_extension |> last_segment
    |> named_uri ~base
  in
  uri

let path_to_uri ~(base : URI.t) str =
  str |> last_segment |> Filename.chop_extension |> named_uri ~base
