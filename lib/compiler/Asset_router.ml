(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

let router : (string, URI.t) Hashtbl.t = Hashtbl.create 100

let normalize ?loc source_path =
  try Unix.realpath source_path
  with Unix.Unix_error (e, _, m) ->
    Reporter.fatal ?loc
      (Asset_not_found (Format.asprintf "%s: %s" (Unix.error_message e) m))

let install ~(config : Config.t) ~source_path ~content =
  let normalized = normalize source_path in
  match Hashtbl.find_opt router normalized with
  | Some uri -> uri
  | None ->
    let hash =
      Result.get_ok
      @@ Multihash_digestif.of_cstruct `Sha3_256 (Cstruct.of_string content)
    in
    let cid = Cid.v ~version:`Cidv1 ~codec:`Raw ~base:`Base32 ~hash in
    let cid_str = Cid.to_string cid in
    let ext = Filename.extension normalized in
    let uri = URI_scheme.file_uri ~base:config.url ~ext cid_str in
    Hashtbl.add router normalized uri;
    uri

let uri_of_asset ?loc ~source_path () =
  let normalized = normalize ?loc source_path in
  match Hashtbl.find_opt router normalized with
  | Some uri -> uri
  | None -> Reporter.fatal ?loc (Asset_has_no_content_address normalized)
