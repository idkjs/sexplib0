/** Type of S-expressions */;

type t =
  | Atom(string)
  | List(list(t));

/*_ We don't use [@@deriving sexp] as this would generated references to [Sexplib],
  creating a circular dependency */
let t_of_sexp: t => t;
let sexp_of_t: t => t;
let t_sexp_grammar: Raw_grammar.t;

let equal: (t, t) => bool;
let compare: (t, t) => int;

/** [Not_found_s] is used by functions that historically raised [Not_found], to allow them
    to raise an exception that contains an informative error message (as a sexp), while
    still having an exception that can be distinguished from other exceptions. */

exception Not_found_s(t);

/** [Of_sexp_error (exn, sexp)] the exception raised when an S-expression could not be
    successfully converted to an OCaml-value. */

exception Of_sexp_error(exn, t);

/** {1 Helpers} */;

/** Helper to build nice s-expressions for error messages.  It imitates the behavior of
    [[%message ...]] from the ppx_sexp_message rewriter.

    [message name key_values] produces a s-expression list starting with atom [name] and
    followed by list of size 2 of the form [(key value)].  When the key is the empty
    string, [value] is used directly instead as for [[%message]].

    For instance the following code:

    {[
      Sexp.message "error"
        [ "x", sexp_of_int 42
        ; "" , sexp_of_exn Exit
        ]
    ]}

    produces the s-expression:

    {[
      (error (x 42) Exit)
    ]} */

let message: (string, list((string, t))) => t;

/** {1 Defaults} */;

/** [default_indent] reference to default indentation level for human-readable
    conversions.

    Initialisation value: 2. */

let default_indent: ref(int);

/** {1 Pretty printing of S-expressions} */;

/** [pp_hum ppf sexp] outputs S-expression [sexp] to formatter [ppf] in human readable
    form. */

let pp_hum: (Format.formatter, t) => unit;

/** [pp_hum_indent n ppf sexp] outputs S-expression [sexp] to formatter [ppf] in human
    readable form and indentation level [n]. */

let pp_hum_indent: (int, Format.formatter, t) => unit;

/** [pp_mach ppf sexp] outputs S-expression [sexp] to formatter [ppf] in machine readable
    (i.e. most compact) form. */

let pp_mach: (Format.formatter, t) => unit;

/** Same as [pp_mach]. */

let pp: (Format.formatter, t) => unit;

/** {1 Conversion to strings} */;

/** [to_string_hum ?indent sexp] converts S-expression [sexp] to a
    string in human readable form with indentation level [indent].

    @param indent default = [!default_indent] */

let to_string_hum: (~indent: int=?, t) => string;

/** [to_string_mach sexp] converts S-expression [sexp] to a string in
    machine readable (i.e. most compact) form. */

let to_string_mach: t => string;

/** Same as [to_string_mach]. */

let to_string: t => string;

/** {1 Styles} */;

let of_float_style: ref([ | `Underscores | `No_underscores]);
let of_int_style: ref([ | `Underscores | `No_underscores]);
/*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules */
module Private: {
  /*_ exported for downstream tools */
  module Raw_grammar: {
    include (module type of {
      include Raw_grammar;
    });

    module Builtin: {
      let unit_sexp_grammar: t;
      let bool_sexp_grammar: t;
      let string_sexp_grammar: t;
      let bytes_sexp_grammar: t;
      let char_sexp_grammar: t;
      let int_sexp_grammar: t;
      let float_sexp_grammar: t;
      let int32_sexp_grammar: t;
      let int64_sexp_grammar: t;
      let nativeint_sexp_grammar: t;
      let ref_sexp_grammar: t;
      let lazy_t_sexp_grammar: t;
      let option_sexp_grammar: t;
      let list_sexp_grammar: t;
      let array_sexp_grammar: t;
    };

    let empty_sexp_grammar: t;
    let opaque_sexp_grammar: t;
    let fun_sexp_grammar: t;
    let tuple2_sexp_grammar: t;

    /** [Placeholder.t_sexp_grammar] is valid for any type, but indicates that a more
        specific grammar is coming. */

    module Placeholder: {
      module type S = Raw_grammar.Placeholder;

      let t_sexp_grammar: t;
    };
  };

  /*_ Exported for sexplib */

  let size: t => (int, int);

  let buffer: unit => Buffer.t;

  let to_buffer: (~buf: Buffer.t, t) => unit;
  let to_buffer_hum: (~buf: Buffer.t, ~indent: int=?, t) => unit;
  let to_buffer_mach: (~buf: Buffer.t, t) => unit;
  let to_buffer_gen:
    (
      ~buf: 'buffer,
      ~add_char: ('buffer, char) => unit,
      ~add_string: ('buffer, string) => unit,
      t
    ) =>
    unit;

  let mach_maybe_esc_str: string => string;
  let must_escape: string => bool;
  let esc_str: string => string;
};
