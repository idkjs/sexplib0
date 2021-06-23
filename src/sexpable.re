module type S = {
  type t;

  let t_of_sexp: Sexp.t => t;
  let sexp_of_t: t => Sexp.t;
};

module type S1 = {
  type t('a);

  let t_of_sexp: (Sexp.t => 'a, Sexp.t) => t('a);
  let sexp_of_t: ('a => Sexp.t, t('a)) => Sexp.t;
};

module type S2 = {
  type t('a, 'b);

  let t_of_sexp: (Sexp.t => 'a, Sexp.t => 'b, Sexp.t) => t('a, 'b);
  let sexp_of_t: ('a => Sexp.t, 'b => Sexp.t, t('a, 'b)) => Sexp.t;
};

module type S3 = {
  type t('a, 'b, 'c);

  let t_of_sexp:
    (Sexp.t => 'a, Sexp.t => 'b, Sexp.t => 'c, Sexp.t) => t('a, 'b, 'c);

  let sexp_of_t:
    ('a => Sexp.t, 'b => Sexp.t, 'c => Sexp.t, t('a, 'b, 'c)) => Sexp.t;
};
