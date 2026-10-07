(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

module T := Types

type query = {
  query: (string, T.content T.vertex) Forester_core.Datalog_expr.query;
}
val query_t : query Repr.t

val route : State.t -> URI.t -> URI.t
val render_article : State.t -> T.content T.article -> Pure_html.node

val render_content : State.t -> T.content -> Pure_html.node list
val render_transclusion : State.t -> T.content -> Pure_html.node list
val render_frontmatter : State.t -> T.content T.frontmatter -> Pure_html.node

val render_query_result : State.t -> Vertex_set.t -> Pure_html.node option

val render_toc : T.content T.section -> Pure_html.node
