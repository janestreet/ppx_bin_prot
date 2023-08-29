open! Core
open Bigarray
open Bin_prot
open Common
open Utils
open Type_class
open Bin_prot.Std

module Array1_extras (M : Expect_test_helpers_base.With_equal) = struct
  let to_list : type a b c. (a, b, c) Array1.t -> a list =
    fun t ->
    let get i =
      match Array1.layout t with
      | C_layout -> Array1.get t i
      | Fortran_layout -> Array1.get t (i + 1)
    in
    List.init (Array1.dim t) ~f:get
  ;;

  let equal t1 t2 = Comparable.lift (List.equal M.equal) ~f:to_list t1 t2
  let sexp_of_t t = List.sexp_of_t M.sexp_of_t (to_list t)
end

module Common = struct
  type tuple = float * string * int64
  [@@deriving bin_io ~localize, bin_io, equal, sexp_of]

  type 'a record =
    { a : int
    ; b : 'a
    ; c : 'a option
    }
  [@@deriving bin_io ~localize, bin_io, equal, sexp_of]

  type 'a singleton_record = { y : 'a }
  [@@deriving bin_io ~localize, bin_io, equal, sexp_of]

  type 'a inline_record =
    | IR of
        { mutable ir_a : int
        ; ir_b : 'a
        ; ir_c : 'a option
        }
    | Other of int
  [@@deriving bin_io ~localize, bin_io, equal, sexp_of]

  type 'a sum =
    | Foo
    | Bar of int
    | Bla of 'a * string
  [@@deriving bin_io ~localize, bin_io, equal, sexp_of]

  type 'a variant =
    [ `Foo
    | `Bar of int
    | `Bla of 'a * string
    ]
  [@@deriving bin_io ~localize, bin_io, equal, sexp_of]

  type variant_extension =
    [ float variant
    | `Baz of int * float
    ]
  [@@deriving bin_io ~localize, bin_io, equal, sexp_of]

  type 'a poly_app =
    (tuple * int singleton_record * 'a record * 'a inline_record) variant sum list
  [@@deriving bin_io ~localize, bin_io, equal, sexp_of]

  type 'a rec_t1 = RecFoo1 of 'a rec_t2

  and 'a rec_t2 =
    | RecFoo2 of 'a poly_app * 'a rec_t1
    | RecNone
  [@@deriving bin_io ~localize, bin_io, equal, sexp_of]

  type 'a poly_id = 'a rec_t1 [@@deriving bin_io ~localize, bin_io, equal, sexp_of]
  type el = float poly_id [@@deriving bin_io ~localize, bin_io, equal, sexp_of]
  type els = el array [@@deriving bin_io ~localize, bin_io, equal, sexp_of]

  module Wildcard : sig
    type _ transparent = int [@@deriving bin_io ~localize, bin_io, equal, sexp_of]
    type _ opaque [@@deriving bin_io ~localize, bin_io, equal, sexp_of]

    val opaque_examples : int opaque list
  end = struct
    type _ transparent = int [@@deriving bin_io ~localize, bin_io, equal, sexp_of]
    type 'a opaque = 'a option [@@deriving bin_io ~localize, bin_io, equal, sexp_of]

    let opaque_examples = [ None; Some 0; Some 1 ]
  end

  let%expect_test "Utils.bin_dump" =
    let el =
      let record = { a = 17; b = 2.78; c = None } in
      let inline_record = IR { ir_a = 18; ir_b = 43210.; ir_c = None } in
      let arg = (3.1, "foo", 42L), { y = 4321 }, record, inline_record in
      let variant = `Bla (arg, "fdsa") in
      let sum = Bla (variant, "asdf") in
      let poly_app = [ sum ] in
      RecFoo1 (RecFoo2 (poly_app, RecFoo1 RecNone))
    in
    let els = Array.create ~len:10 el in
    let buf = bin_dump ~header:true bin_els.writer els in
    let pos_ref = ref 0 in
    let els_len = Read.bin_read_int_64bit buf ~pos_ref in
    Expect_test_helpers_base.require_equal
      [%here]
      (module Int)
      ~message:"pos_ref for length incorrect"
      !pos_ref
      8;
    Expect_test_helpers_base.require_equal
      [%here]
      (module Int)
      ~message:"els_len disagrees with bin_size"
      els_len
      (bin_size_els els);
    let new_els = bin_read_els buf ~pos_ref in
    Expect_test_helpers_base.require_equal
      [%here]
      (module struct
        type t = float poly_id Array.t [@@deriving equal, sexp_of]
      end)
      ~message:"new_els and els not equal"
      els
      new_els
  ;;
end

let%test_module "Inline" =
  (module struct
    let check_compatible m xs derived_tc inline_writer inline_reader inline_tc =
      List.iter xs ~f:(fun x ->
        Expect_test_helpers_base.require_equal
          [%here]
          (module Int)
          ~message:"incorrect size from inline writer"
          (derived_tc.writer.size x)
          (inline_writer.size x);
        Expect_test_helpers_base.require_equal
          [%here]
          (module Int)
          ~message:"incorrect size from inline type class"
          (derived_tc.writer.size x)
          (inline_tc.writer.size x);
        let buf = bin_dump derived_tc.writer x in
        Expect_test_helpers_base.require_equal
          [%here]
          (module struct
            type t = buf

            include Array1_extras (Char)
          end)
          ~message:"incorrect bin dump from inline writer"
          buf
          (bin_dump inline_writer x);
        Expect_test_helpers_base.require_equal
          [%here]
          (module struct
            type t = buf

            include Array1_extras (Char)
          end)
          ~message:"incorrect bin dump from inline type class"
          buf
          (bin_dump inline_tc.writer x);
        let val_and_len reader =
          let pos_ref = ref 0 in
          let x = reader.read buf ~pos_ref in
          x, !pos_ref
        in
        let _, len = val_and_len derived_tc.reader in
        let x', len' = val_and_len inline_reader in
        Expect_test_helpers_base.require_equal
          [%here]
          m
          ~message:"incorrect value from inline reader"
          x
          x';
        Expect_test_helpers_base.require_equal
          [%here]
          (module Int)
          ~message:"incorrect length from inline reader"
          len
          len';
        let x', len' = val_and_len inline_tc.reader in
        Expect_test_helpers_base.require_equal
          [%here]
          m
          ~message:"incorrect value from inline type class"
          x
          x';
        Expect_test_helpers_base.require_equal
          [%here]
          (module Int)
          ~message:"incorrect length from inline type class"
          len
          len')
    ;;

    let%expect_test "simple tuple" =
      check_compatible
        (module struct
          type t = Common.tuple [@@deriving equal, sexp_of]
        end)
        [ 50.5, "hello", 1234L ]
        Common.bin_tuple
        [%bin_writer: Common.tuple]
        [%bin_reader: Common.tuple]
        [%bin_type_class: Common.tuple]
    ;;

    let%expect_test "redefine tuple" =
      check_compatible
        (module struct
          type t = Common.tuple [@@deriving equal, sexp_of]
        end)
        [ 50.5, "hello", 1234L ]
        Common.bin_tuple
        [%bin_writer: float * string * int64]
        [%bin_reader: float * string * int64]
        [%bin_type_class: float * string * int64]
    ;;

    let%expect_test "simple variant" =
      check_compatible
        (module struct
          type t = float Common.variant [@@deriving equal, sexp_of]
        end)
        [ `Foo; `Bar 8; `Bla (33.3, "world") ]
        (Common.bin_variant bin_float)
        [%bin_writer: float Common.variant]
        [%bin_reader: float Common.variant]
        [%bin_type_class: float Common.variant]
    ;;

    let%expect_test "redefine variant" =
      check_compatible
        (module struct
          type t =
            [ `Foo
            | `Bar of int
            | `Bla of float * string
            ]
          [@@deriving equal, sexp_of]
        end)
        [ `Foo; `Bar 8; `Bla (33.3, "world") ]
        (Common.bin_variant bin_float)
        [%bin_writer: [ `Foo | `Bar of int | `Bla of float * string ]]
        [%bin_reader: [ `Foo | `Bar of int | `Bla of float * string ]]
        [%bin_type_class: [ `Foo | `Bar of int | `Bla of float * string ]]
    ;;

    let%expect_test "variant_extension" =
      check_compatible
        (module struct
          type t =
            [ float Common.variant
            | `Baz of int * float
            ]
          [@@deriving equal, sexp_of]
        end)
        [ `Foo; `Bar 8; `Bla (33.3, "world"); `Baz (17, 17.71) ]
        Common.bin_variant_extension
        [%bin_writer: [ float Common.variant | `Baz of int * float ]]
        [%bin_reader: [ float Common.variant | `Baz of int * float ]]
        [%bin_type_class: [ float Common.variant | `Baz of int * float ]]
    ;;

    let%expect_test "sub variant" =
      check_compatible
        (module struct
          type t = [ `Foo | `Bar of int | `Bla of int * string ] Common.singleton_record
          [@@deriving equal, sexp_of]
        end)
        [ { Common.y = `Foo }; { y = `Bar 42 }; { y = `Bla (42, "world") } ]
        (Common.bin_singleton_record (Common.bin_variant bin_int))
        [%bin_writer:
          [ `Foo | `Bar of int | `Bla of int * string ] Common.singleton_record]
        [%bin_reader:
          [ `Foo | `Bar of int | `Bla of int * string ] Common.singleton_record]
        [%bin_type_class:
          [ `Foo | `Bar of int | `Bla of int * string ] Common.singleton_record]
    ;;

    let%expect_test "transparent wildcard" =
      check_compatible
        (module struct
          type t = string Common.Wildcard.transparent [@@deriving equal, sexp_of]
        end)
        [ 1; 2; 3 ]
        (Common.Wildcard.bin_transparent bin_string)
        [%bin_writer: string Common.Wildcard.transparent]
        [%bin_reader: string Common.Wildcard.transparent]
        [%bin_type_class: string Common.Wildcard.transparent]
    ;;

    let%expect_test "opaque wildcard" =
      check_compatible
        (module struct
          type t = int Common.Wildcard.opaque [@@deriving equal, sexp_of]
        end)
        Common.Wildcard.opaque_examples
        (Common.Wildcard.bin_opaque bin_int)
        [%bin_writer: int Common.Wildcard.opaque]
        [%bin_reader: int Common.Wildcard.opaque]
        [%bin_type_class: int Common.Wildcard.opaque]
    ;;
  end)
;;

let%test_module "Local" =
  (module struct
    module type S = sig
      type t [@@deriving bin_io ~localize]

      include Expect_test_helpers_base.With_equal with type t := t
    end

    let check_compatible : type a. (module S with type t = a) -> a list -> unit =
      fun (module M) xs ->
      List.iter xs ~f:(fun x ->
        if phys_equal M.bin_write_t (M.bin_write_t__local :> _ Write.writer)
        then print_endline "bin_write_t = bin_write_t__local"
        else (
          Expect_test_helpers_base.require_equal
            [%here]
            (module Int)
            ~message:"bin_size differs from bin_size_local"
            (M.bin_size_t x)
            (M.bin_size_t__local x);
          let buf = bin_dump M.bin_writer_t x in
          Expect_test_helpers_base.require_equal
            [%here]
            (module struct
              type t = buf

              include Array1_extras (Char)
            end)
            ~message:"incorrect bin dump from local writer"
            (bin_dump
               { size = (M.bin_size_t__local :> _ Size.sizer)
               ; write = (M.bin_write_t__local :> _ Write.writer)
               }
               x)
            buf;
          let x' = M.bin_read_t buf ~pos_ref:(ref 0) in
          Expect_test_helpers_base.require_equal
            [%here]
            (module M)
            ~message:"bin_write_local -> bin_read roundtrip failed"
            x
            x'))
    ;;

    let%expect_test "tuple" =
      check_compatible
        (module struct
          type t = Common.tuple [@@deriving bin_io ~localize, bin_io, equal, sexp_of]
        end)
        [ 1., "hi", 2L; Float.infinity, "", 0L ]
    ;;

    let%expect_test "variant" =
      check_compatible
        (module struct
          type t = float Common.variant
          [@@deriving bin_io ~localize, bin_io, equal, sexp_of]
        end)
        [ `Foo; `Bar 8; `Bla (33.3, "world") ]
    ;;

    let%expect_test "variant_extension" =
      check_compatible
        (module struct
          type t = Common.variant_extension
          [@@deriving bin_io ~localize, bin_io, equal, sexp_of]
        end)
        [ `Foo; `Bar 8; `Bla (33.3, "world"); `Baz (17, 17.71) ]
    ;;

    let%expect_test "sub variant" =
      check_compatible
        (module struct
          type t = int Common.variant Common.singleton_record
          [@@deriving bin_io ~localize, bin_io, equal, sexp_of]
        end)
        [ { Common.y = `Foo }; { y = `Bar 42 }; { y = `Bla (42, "world") } ]
    ;;

    let%expect_test "transparent wildcard" =
      check_compatible
        (module struct
          type t = string Common.Wildcard.transparent
          [@@deriving bin_io ~localize, bin_io, equal, sexp_of]
        end)
        [ 1; 2; 3 ]
    ;;

    let%expect_test "opaque wildcard" =
      check_compatible
        (module struct
          type t = int Common.Wildcard.opaque
          [@@deriving bin_io ~localize, bin_io, equal, sexp_of]
        end)
        Common.Wildcard.opaque_examples
    ;;

    let%expect_test "complicated type" =
      let el =
        let open Common in
        let record = { a = 17; b = 2.78; c = None } in
        let inline_record = IR { ir_a = 18; ir_b = 43210.; ir_c = None } in
        let arg = (3.1, "foo", 42L), { y = 4321 }, record, inline_record in
        let variant = `Bla (arg, "fdsa") in
        let sum = Bla (variant, "asdf") in
        let poly_app = [ sum ] in
        RecFoo1 (RecFoo2 (poly_app, RecFoo1 RecNone))
      in
      let els = Array.create ~len:10 el in
      check_compatible
        (module struct
          type t = Common.els [@@deriving bin_io ~localize, bin_io, equal, sexp_of]
        end)
        [ els ]
    ;;
  end)
;;
