(* Ensure that all of the extension points expand without causing compiler errors. *)
module _ = struct
  module T' = struct
    open Bin_prot.Std

    type t =
      { a : int
      ; b : string
      }
    [@@deriving bin_io]
  end

  open T'

  (* [Bin_prot.Std] doesn't need to be included in namespace. *)

  let _ = [%bin_shape: t]
  let (_ : string) = [%bin_digest: t]
  let _ = [%bin_size: t]
  let _ = [%bin_write: t]
  let _ = [%bin_writer: t]
  let (_ : t Bin_prot.Read.reader) = [%bin_read: t]
  let (_ : t Bin_prot.Type_class.reader) = [%bin_reader: t]
  let _ = [%bin_type_class: t]
end

(* Check extension points on polymorphic variants. *)
module _ = struct
  type t =
    [ `A
    | `B of int
    ]

  open Bin_prot.Std

  let (_ : Bin_shape.t) = [%bin_shape: [ `A | `B of int ]]
  let (_ : string) = [%bin_digest: [ `A | `B of int ]]
  let _ = [%bin_size: [ `A | `B of int ]]
  let _ = [%bin_write: [ `A | `B of int ]]
  let _ = [%bin_writer: [ `A | `B of int ]]
  let (_ : t Bin_prot.Read.reader) = [%bin_read: [ `A | `B of int ]]
  let (_ : t Bin_prot.Type_class.reader) = [%bin_reader: [ `A | `B of int ]]
  let _ = [%bin_type_class: [ `A | `B of int ]]
end

open! Core
open Expect_test_helpers_core

module type S = sig
  type t [@@deriving equal, quickcheck, sexp_of]
end

(* Testing [%bin_size{,_local}], [%bin_write{,_local}], and [%bin_read] extension points
   behave the same as the derived functions. *)
let test
  (type a)
  bin_size
  bin_size_local
  bin_write
  bin_write_local
  bin_read
  (module M : S with type t = a)
  =
  quickcheck_m
    [%here]
    (module M)
    ~f:(fun t ->
      let computed_size = bin_size t in
      let computed_size_local = bin_size_local t in
      require
        [%here]
        (computed_size = computed_size_local)
        ~if_false_then_print_s:
          [%lazy_message
            "bin_size differs from bin_size_local"
              (computed_size : int)
              (computed_size_local : int)];
      let message = Bigstring.create computed_size in
      let written_size = bin_write message ~pos:0 t in
      require
        [%here]
        (computed_size = written_size)
        ~if_false_then_print_s:
          [%lazy_message
            "did not write entire message"
              (computed_size : int)
              (written_size : int)
              ~written:(Bigstring.sub message ~pos:0 ~len:written_size : Bigstring.t)];
      let pos_ref = ref 0 in
      let round_trip = bin_read message ~pos_ref in
      let read_size = !pos_ref in
      require
        [%here]
        (computed_size = read_size)
        ~if_false_then_print_s:
          [%lazy_message
            "did not read entire message"
              (computed_size : int)
              (read_size : int)
              (message : Bigstring.t)];
      require
        [%here]
        (M.equal t round_trip)
        ~if_false_then_print_s:
          [%lazy_message "value did not round-trip" (t : M.t) (round_trip : M.t)];
      let message_local = Bigstring.create computed_size in
      let (_ : int) = bin_write_local message_local ~pos:0 t in
      require
        [%here]
        (Bigstring.equal message message_local)
        ~if_false_then_print_s:
          [%lazy_message
            "bin_write differs from bin_write_local"
              ~output:(message : Bigstring.t)
              ~local_output:(message_local : Bigstring.t)])
;;

let%expect_test _ =
  test
    [%bin_size: int]
    [%bin_size_local: int]
    [%bin_write: int]
    [%bin_write_local: int]
    [%bin_read: int]
    (module Int);
  [%expect {| |}]
;;

let%expect_test _ =
  test
    [%bin_size: string list]
    [%bin_size_local: string list]
    [%bin_write: string list]
    [%bin_write_local: string list]
    [%bin_read: string list]
    (module struct
      type t = string list [@@deriving equal, quickcheck, sexp_of]
    end);
  [%expect {| |}]
;;

let%expect_test _ =
  let open struct
    type c = [ `C of string ] [@@deriving bin_io ~localize, equal, quickcheck, sexp_of]
  end in
  test
    [%bin_size: [ `A | `B of int | c ]]
    [%bin_size_local: [ `A | `B of int | c ]]
    [%bin_write: [ `A | `B of int | c ]]
    [%bin_write_local: [ `A | `B of int | c ]]
    [%bin_read: [ `A | `B of int | c ]]
    (module struct
      type t =
        [ `A
        | `B of int
        | c
        ]
      [@@deriving equal, quickcheck, sexp_of]
    end);
  [%expect {| |}]
;;
