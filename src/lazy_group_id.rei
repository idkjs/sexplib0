/** [Lazy_group_id] is a cheap way to allocate unique integer identifiers for sexp
    grammars. See [sexp_intf.ml] for details. */;

type t;

let compare: (t, t) => int;

let create: unit => t;
let force: t => int;
