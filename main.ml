module type RationalType = sig
  type t
  val make : int -> int -> t
  val numer : t -> int
  val denom : t -> int

  val ( /+ ) : t -> t -> t
  val ( /- ) : t -> t -> t
  val ( /* ) : t -> t -> t
  val ( // ) : t -> t -> t

  val ( /> ) : t -> t -> bool
  val ( /< ) : t -> t -> bool

  val string_of_rational : t -> string
end

module Rational : RationalType = struct
  type t = int * int

  exception GcdNegativeException
  let rec gcd a b =
    if a < 0 || b < 0
    then raise GcdNegativeException
    else match (a, b) with
      | (a, 0) -> a
      | (0, b) -> b
      | _ ->
        if a > b then gcd b (a mod b)
        else gcd a (b mod a)

  let rec make a b =
    if b < 0
    then make (-a) (-b)
    else
      let (m, n, is_negative) = if a < 0 then (-a, b, true) else (a, b, false)
      in let d = gcd m n
      in let (p, q) = (m / d, n / d)
      in if is_negative then (-p, q) else (p, q)

  let numer (a, _) = a
  let denom (_, b) = b

  (* p/q + r/s = (ps + rq) / qs *)
  let ( /+ ) (p, q) (r, s) = make (p*s + r*q) (q*s)

  (* p/q * r/s = pr / qs *)
  let ( /* ) (p, q) (r, s) = make (p*r) (q*s)

  (* a - b = a + (b * -1) *)
  let ( /- ) a b = a /+ make (- numer b) (denom b)

  (* a / (r/s) = a * s/r *)
  let ( // ) a (r, s) = a /* (s, r)

  let ( /> ) a b =
    let (n', _) = a /- b
    in n' > 0

  let ( /< ) a b =
    let (n', _) = a /- b
    in n' < 0

  let string_of_rational (a, b) =
    string_of_int a ^ "/" ^ string_of_int b
end

(* https://en.wikipedia.org/wiki/Egyptian_fraction *)
let rec egyptian n =
  let open Rational in
  if numer n = 1
  then [n]
  else
    let biggest_inv = make 1 (1 + (denom n / numer n))
    in
    biggest_inv :: egyptian (n /- biggest_inv)

let () =
  (* 5/121 = 1/25 + 1/757 + 1/763309 + 1/873960180913 + 1/1025410058030422033 *)
  let r = Rational.make 5 121
  in
  Printf.sprintf
    "%s = %s"
    (Rational.string_of_rational r)
    ( egyptian r
      |> List.map Rational.string_of_rational
      |> String.concat " + "
    )
  |> print_endline
