(** {!Markup} exercises odoc markup: headings, code blocks,
    lists, emphasis, anchors, and external links.

    {1 Overview}

    This module is a test target for odoc's markup parser. Each
    section below uses one piece of syntax.

    {2 Code block}

    {[
      let square x = x * x
      let () = ignore (square 7)
    ]}

    {2 Lists}

    Bulleted:
    - one
    - two
    - three

    {2 Emphasis and style}

    {b Bold}, {i italic}, and [inline code] should render
    distinctly.

    {2 Link}

    See {{:https://ocaml.org/} the OCaml homepage}.

    {2 Anchor}

    This section is linkable as {!section-anchor-example}.
    {@html[<a id="anchor-example"></a>]}
*)

(** Square a number. *)
let square x = x * x

(** Double a number. *)
let double x = x + x
