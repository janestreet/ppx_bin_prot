open! Bin_prot.Std

[@@@warning "-60"]

module T : sig
  type t [@@deriving_inline bin_io, bin_io ~localize]

  include sig
    [@@@ocaml.warning "-32"]

    include Bin_prot.Binable.S with type t := t
    include Bin_prot.Binable.S_local with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = A [@@deriving_inline bin_io ~localize ~hide_locations]

  let _ = fun (_ : t) -> ()

  let bin_shape_t =
    let _group =
      Bin_prot.Shape.group
        (Bin_prot.Shape.Location.of_string "<hidden>")
        [ Bin_prot.Shape.Tid.of_string "t", [], Bin_prot.Shape.variant [ "A", [] ] ]
    in
    (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) []
  ;;

  let _ = bin_shape_t

  let (bin_size_t__local : t Bin_prot.Size.sizer_local) = function
    | A -> 1
  ;;

  let _ = bin_size_t__local
  let bin_size_t = (bin_size_t__local :> _ Bin_prot.Size.sizer)
  let _ = bin_size_t

  let (bin_write_t__local : t Bin_prot.Write.writer_local) =
    fun buf ~pos -> function
    | A -> Bin_prot.Write.bin_write_int_8bit buf ~pos 0
  ;;

  let _ = bin_write_t__local
  let bin_write_t = (bin_write_t__local :> _ Bin_prot.Write.writer)
  let _ = bin_write_t

  let bin_writer_t =
    ({ size = bin_size_t; write = bin_write_t } : _ Bin_prot.Type_class.writer)
  ;;

  let _ = bin_writer_t

  let (__bin_read_t__ : (int -> t) Bin_prot.Read.reader) =
    fun _buf ~pos_ref _vint ->
    Bin_prot.Common.raise_variant_wrong_type "deriving_inline.ml.T.t" !pos_ref
  ;;

  let _ = __bin_read_t__

  let (bin_read_t : t Bin_prot.Read.reader) =
    fun buf ~pos_ref ->
    match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
    | 0 -> A
    | _ ->
      Bin_prot.Common.raise_read_error
        (Bin_prot.Common.ReadError.Sum_tag "deriving_inline.ml.T.t")
        !pos_ref
  ;;

  let _ = bin_read_t

  let bin_reader_t =
    ({ read = bin_read_t; vtag_read = __bin_read_t__ } : _ Bin_prot.Type_class.reader)
  ;;

  let _ = bin_reader_t

  let bin_t =
    ({ writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
      : _ Bin_prot.Type_class.t)
  ;;

  let _ = bin_t

  [@@@end]
end

module T1 : sig
  type 'a t [@@deriving_inline bin_io, bin_io ~localize]

  include sig
    [@@@ocaml.warning "-32"]

    include Bin_prot.Binable.S1 with type 'a t := 'a t
    include Bin_prot.Binable.S_local1 with type 'a t := 'a t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type 'a t = A of 'a [@@deriving_inline bin_io ~localize ~hide_locations]

  let _ = fun (_ : 'a t) -> ()

  let bin_shape_t =
    let _group =
      Bin_prot.Shape.group
        (Bin_prot.Shape.Location.of_string "<hidden>")
        [ ( Bin_prot.Shape.Tid.of_string "t"
          , [ Bin_prot.Shape.Vid.of_string "a" ]
          , Bin_prot.Shape.variant
              [ ( "A"
                , [ Bin_prot.Shape.var
                      (Bin_prot.Shape.Location.of_string "<hidden>")
                      (Bin_prot.Shape.Vid.of_string "a")
                  ] )
              ] )
        ]
    in
    fun a -> (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) [ a ]
  ;;

  let _ = bin_shape_t

  let bin_size_t__local :
        'a. 'a Bin_prot.Size.sizer_local -> 'a t Bin_prot.Size.sizer_local
    =
    fun _size_of_a__local -> function
    | A v1 ->
      let size = 1 in
      Bin_prot.Common.( + ) size (_size_of_a__local v1)
  ;;

  let _ = bin_size_t__local

  let bin_size_t : 'a. 'a Bin_prot.Size.sizer -> 'a t Bin_prot.Size.sizer =
    fun _size_of_a -> function
    | A v1 ->
      let size = 1 in
      Bin_prot.Common.( + ) size (_size_of_a v1)
  ;;

  let _ = bin_size_t

  let bin_write_t__local :
        'a. 'a Bin_prot.Write.writer_local -> 'a t Bin_prot.Write.writer_local
    =
    fun _write_a__local buf ~pos -> function
    | A v1 ->
      let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 0 in
      _write_a__local buf ~pos v1
  ;;

  let _ = bin_write_t__local

  let bin_write_t : 'a. 'a Bin_prot.Write.writer -> 'a t Bin_prot.Write.writer =
    fun _write_a buf ~pos -> function
    | A v1 ->
      let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 0 in
      _write_a buf ~pos v1
  ;;

  let _ = bin_write_t

  let bin_writer_t =
    (fun bin_writer_a ->
       { size = (fun v -> bin_size_t bin_writer_a.size v)
       ; write = (fun v -> bin_write_t bin_writer_a.write v)
       }
      : _ Bin_prot.Type_class.writer -> _ Bin_prot.Type_class.writer)
  ;;

  let _ = bin_writer_t

  let __bin_read_t__ : 'a. 'a Bin_prot.Read.reader -> (int -> 'a t) Bin_prot.Read.reader =
    fun _of__a _buf ~pos_ref _vint ->
    Bin_prot.Common.raise_variant_wrong_type "deriving_inline.ml.T1.t" !pos_ref
  ;;

  let _ = __bin_read_t__

  let bin_read_t : 'a. 'a Bin_prot.Read.reader -> 'a t Bin_prot.Read.reader =
    fun _of__a buf ~pos_ref ->
    match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
    | 0 ->
      let arg_1 = _of__a buf ~pos_ref in
      A arg_1
    | _ ->
      Bin_prot.Common.raise_read_error
        (Bin_prot.Common.ReadError.Sum_tag "deriving_inline.ml.T1.t")
        !pos_ref
  ;;

  let _ = bin_read_t

  let bin_reader_t =
    (fun bin_reader_a ->
       { read = (fun buf ~pos_ref -> (bin_read_t bin_reader_a.read) buf ~pos_ref)
       ; vtag_read =
           (fun buf ~pos_ref vtag -> (__bin_read_t__ bin_reader_a.read) buf ~pos_ref vtag)
       }
      : _ Bin_prot.Type_class.reader -> _ Bin_prot.Type_class.reader)
  ;;

  let _ = bin_reader_t

  let bin_t =
    (fun bin_a ->
       { writer = bin_writer_t bin_a.writer
       ; reader = bin_reader_t bin_a.reader
       ; shape = bin_shape_t bin_a.shape
       }
      : _ Bin_prot.Type_class.t -> _ Bin_prot.Type_class.t)
  ;;

  let _ = bin_t

  [@@@end]
end

module T_write : sig
  type t [@@deriving_inline bin_write, bin_write ~localize]

  include sig
    [@@@ocaml.warning "-32"]

    val bin_size_t : t Bin_prot.Size.sizer
    val bin_write_t : t Bin_prot.Write.writer
    val bin_writer_t : t Bin_prot.Type_class.writer
    val bin_size_t : t Bin_prot.Size.sizer
    val bin_size_t__local : t Bin_prot.Size.sizer_local
    val bin_write_t : t Bin_prot.Write.writer
    val bin_write_t__local : t Bin_prot.Write.writer_local
    val bin_writer_t : t Bin_prot.Type_class.writer
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t [@@deriving bin_write ~localize]
end

module T_read : sig
  type t [@@deriving_inline bin_read]

  include sig
    [@@@ocaml.warning "-32"]

    val bin_read_t : t Bin_prot.Read.reader
    val __bin_read_t__ : (int -> t) Bin_prot.Read.reader
    val bin_reader_t : t Bin_prot.Type_class.reader
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t [@@deriving bin_read]
end

module T_type_class : sig
  type t [@@deriving_inline bin_type_class]

  include sig
    [@@@ocaml.warning "-32"]

    val bin_t : t Bin_prot.Type_class.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t [@@deriving bin_io]
end

module Mutual_recursion : sig
  type t =
    | Int of int
    | Add of u * u

  and u = Mul of t * t [@@deriving bin_io ~localize]
end = struct
  type t =
    | Int of int
    | Add of u * u

  and u = Mul of t * t [@@deriving_inline bin_io ~localize ~hide_locations]

  let _ = fun (_ : t) -> ()
  let _ = fun (_ : u) -> ()

  let bin_shape_t, bin_shape_u =
    let _group =
      Bin_prot.Shape.group
        (Bin_prot.Shape.Location.of_string "<hidden>")
        [ ( Bin_prot.Shape.Tid.of_string "t"
          , []
          , Bin_prot.Shape.variant
              [ "Int", [ bin_shape_int ]
              ; ( "Add"
                , [ (Bin_prot.Shape.rec_app (Bin_prot.Shape.Tid.of_string "u")) []
                  ; (Bin_prot.Shape.rec_app (Bin_prot.Shape.Tid.of_string "u")) []
                  ] )
              ] )
        ; ( Bin_prot.Shape.Tid.of_string "u"
          , []
          , Bin_prot.Shape.variant
              [ ( "Mul"
                , [ (Bin_prot.Shape.rec_app (Bin_prot.Shape.Tid.of_string "t")) []
                  ; (Bin_prot.Shape.rec_app (Bin_prot.Shape.Tid.of_string "t")) []
                  ] )
              ] )
        ]
    in
    ( (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) []
    , (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "u")) [] )
  ;;

  let _ = bin_shape_t
  and _ = bin_shape_u

  let rec (bin_size_t__local : t Bin_prot.Size.sizer_local) = function
    | Int v1 ->
      let size = 1 in
      Bin_prot.Common.( + ) size (bin_size_int__local v1)
    | Add (v1, v2) ->
      let size = 1 in
      let size = Bin_prot.Common.( + ) size (bin_size_u__local v1) in
      Bin_prot.Common.( + ) size (bin_size_u__local v2)

  and (bin_size_u__local : u Bin_prot.Size.sizer_local) = function
    | Mul (v1, v2) ->
      let size = 1 in
      let size = Bin_prot.Common.( + ) size (bin_size_t__local v1) in
      Bin_prot.Common.( + ) size (bin_size_t__local v2)
  ;;

  let _ = bin_size_t__local
  and _ = bin_size_u__local

  let bin_size_t = (bin_size_t__local :> _ Bin_prot.Size.sizer)
  and bin_size_u = (bin_size_u__local :> _ Bin_prot.Size.sizer)

  let _ = bin_size_t
  and _ = bin_size_u

  let rec (bin_write_t__local : t Bin_prot.Write.writer_local) =
    fun buf ~pos -> function
    | Int v1 ->
      let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 0 in
      bin_write_int__local buf ~pos v1
    | Add (v1, v2) ->
      let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 1 in
      let pos = bin_write_u__local buf ~pos v1 in
      bin_write_u__local buf ~pos v2

  and (bin_write_u__local : u Bin_prot.Write.writer_local) =
    fun buf ~pos -> function
    | Mul (v1, v2) ->
      let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 0 in
      let pos = bin_write_t__local buf ~pos v1 in
      bin_write_t__local buf ~pos v2
  ;;

  let _ = bin_write_t__local
  and _ = bin_write_u__local

  let bin_write_t = (bin_write_t__local :> _ Bin_prot.Write.writer)
  and bin_write_u = (bin_write_u__local :> _ Bin_prot.Write.writer)

  let _ = bin_write_t
  and _ = bin_write_u

  let bin_writer_t =
    ({ size = bin_size_t; write = bin_write_t } : _ Bin_prot.Type_class.writer)

  and bin_writer_u =
    ({ size = bin_size_u; write = bin_write_u } : _ Bin_prot.Type_class.writer)
  ;;

  let _ = bin_writer_t
  and _ = bin_writer_u

  let rec (__bin_read_t__ : (int -> t) Bin_prot.Read.reader) =
    fun _buf ~pos_ref _vint ->
    Bin_prot.Common.raise_variant_wrong_type
      "deriving_inline.ml.Mutual_recursion.t"
      !pos_ref

  and (__bin_read_u__ : (int -> u) Bin_prot.Read.reader) =
    fun _buf ~pos_ref _vint ->
    Bin_prot.Common.raise_variant_wrong_type
      "deriving_inline.ml.Mutual_recursion.u"
      !pos_ref

  and (bin_read_t : t Bin_prot.Read.reader) =
    fun buf ~pos_ref ->
    match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
    | 0 ->
      let arg_1 = bin_read_int buf ~pos_ref in
      Int arg_1
    | 1 ->
      let arg_1 = bin_read_u buf ~pos_ref in
      let arg_2 = bin_read_u buf ~pos_ref in
      Add (arg_1, arg_2)
    | _ ->
      Bin_prot.Common.raise_read_error
        (Bin_prot.Common.ReadError.Sum_tag "deriving_inline.ml.Mutual_recursion.t")
        !pos_ref

  and (bin_read_u : u Bin_prot.Read.reader) =
    fun buf ~pos_ref ->
    match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
    | 0 ->
      let arg_1 = bin_read_t buf ~pos_ref in
      let arg_2 = bin_read_t buf ~pos_ref in
      Mul (arg_1, arg_2)
    | _ ->
      Bin_prot.Common.raise_read_error
        (Bin_prot.Common.ReadError.Sum_tag "deriving_inline.ml.Mutual_recursion.u")
        !pos_ref
  ;;

  let _ = __bin_read_t__
  and _ = __bin_read_u__
  and _ = bin_read_t
  and _ = bin_read_u

  let bin_reader_t =
    ({ read = bin_read_t; vtag_read = __bin_read_t__ } : _ Bin_prot.Type_class.reader)

  and bin_reader_u =
    ({ read = bin_read_u; vtag_read = __bin_read_u__ } : _ Bin_prot.Type_class.reader)
  ;;

  let _ = bin_reader_t
  and _ = bin_reader_u

  let bin_t =
    ({ writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
      : _ Bin_prot.Type_class.t)

  and bin_u =
    ({ writer = bin_writer_u; reader = bin_reader_u; shape = bin_shape_u }
      : _ Bin_prot.Type_class.t)
  ;;

  let _ = bin_t
  and _ = bin_u

  [@@@end]
end

module Float_array : sig
  type t = float array * int list [@@deriving_inline bin_io]

  include sig
    [@@@ocaml.warning "-32"]

    include Bin_prot.Binable.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = float array * int list [@@deriving_inline bin_io ~hide_locations]

  let _ = fun (_ : t) -> ()

  let bin_shape_t =
    let _group =
      Bin_prot.Shape.group
        (Bin_prot.Shape.Location.of_string "<hidden>")
        [ ( Bin_prot.Shape.Tid.of_string "t"
          , []
          , Bin_prot.Shape.tuple
              [ bin_shape_array bin_shape_float; bin_shape_list bin_shape_int ] )
        ]
    in
    (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) []
  ;;

  let _ = bin_shape_t

  let (bin_size_t : t Bin_prot.Size.sizer) = function
    | v1, v2 ->
      let size = 0 in
      let size = Bin_prot.Common.( + ) size (bin_size_array bin_size_float v1) in
      Bin_prot.Common.( + ) size (bin_size_list bin_size_int v2)
  ;;

  let _ = bin_size_t

  let (bin_write_t : t Bin_prot.Write.writer) =
    fun buf ~pos -> function
    | v1, v2 ->
      let pos = bin_write_array bin_write_float buf ~pos v1 in
      bin_write_list bin_write_int buf ~pos v2
  ;;

  let _ = bin_write_t

  let bin_writer_t =
    ({ size = bin_size_t; write = bin_write_t } : _ Bin_prot.Type_class.writer)
  ;;

  let _ = bin_writer_t

  let (__bin_read_t__ : (int -> t) Bin_prot.Read.reader) =
    fun _buf ~pos_ref _vint ->
    Bin_prot.Common.raise_variant_wrong_type "deriving_inline.ml.Float_array.t" !pos_ref
  ;;

  let _ = __bin_read_t__

  let (bin_read_t : t Bin_prot.Read.reader) =
    fun buf ~pos_ref ->
    let v1 = (bin_read_array bin_read_float) buf ~pos_ref in
    let v2 = (bin_read_list bin_read_int) buf ~pos_ref in
    v1, v2
  ;;

  let _ = bin_read_t

  let bin_reader_t =
    ({ read = bin_read_t; vtag_read = __bin_read_t__ } : _ Bin_prot.Type_class.reader)
  ;;

  let _ = bin_reader_t

  let bin_t =
    ({ writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
      : _ Bin_prot.Type_class.t)
  ;;

  let _ = bin_t

  [@@@end]
end

module Global_fields_with_localize : sig
  module Record : sig
    type t [@@deriving bin_io ~localize]
  end

  module Record_constructor : sig
    type t [@@deriving bin_io ~localize]
  end

  module Tuple_constructor : sig
    type t [@@deriving bin_io ~localize]
  end
end = struct
  module T = struct
    type t [@@deriving bin_io ~localize]
  end

  module Normal = T
  module Mutable = T
  module Global = T
  module Ocaml_global = T
  module Extension_global = T

  module Record = struct
    type t =
      { a : Normal.t
      ; mutable b : Mutable.t
      ; c : Global.t
      ; d : Ocaml_global.t
      ; e : Extension_global.t
      }
    [@@deriving_inline bin_io ~localize ~hide_locations]

    let _ = fun (_ : t) -> ()

    let bin_shape_t =
      let _group =
        Bin_prot.Shape.group
          (Bin_prot.Shape.Location.of_string "<hidden>")
          [ ( Bin_prot.Shape.Tid.of_string "t"
            , []
            , Bin_prot.Shape.record
                [ "a", Normal.bin_shape_t
                ; "b", Mutable.bin_shape_t
                ; "c", Global.bin_shape_t
                ; "d", Ocaml_global.bin_shape_t
                ; "e", Extension_global.bin_shape_t
                ] )
          ]
      in
      (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) []
    ;;

    let _ = bin_shape_t

    let (bin_size_t__local : t Bin_prot.Size.sizer_local) = function
      | { a = v1; b = v2; c = v3; d = v4; e = v5 } ->
        let size = 0 in
        let size = Bin_prot.Common.( + ) size (Normal.bin_size_t__local v1) in
        let size = Bin_prot.Common.( + ) size (Mutable.bin_size_t v2) in
        let size = Bin_prot.Common.( + ) size (Global.bin_size_t v3) in
        let size = Bin_prot.Common.( + ) size (Ocaml_global.bin_size_t v4) in
        Bin_prot.Common.( + ) size (Extension_global.bin_size_t v5)
    ;;

    let _ = bin_size_t__local
    let bin_size_t = (bin_size_t__local :> _ Bin_prot.Size.sizer)
    let _ = bin_size_t

    let (bin_write_t__local : t Bin_prot.Write.writer_local) =
      fun buf ~pos -> function
      | { a = v1; b = v2; c = v3; d = v4; e = v5 } ->
        let pos = Normal.bin_write_t__local buf ~pos v1 in
        let pos = Mutable.bin_write_t buf ~pos v2 in
        let pos = Global.bin_write_t buf ~pos v3 in
        let pos = Ocaml_global.bin_write_t buf ~pos v4 in
        Extension_global.bin_write_t buf ~pos v5
    ;;

    let _ = bin_write_t__local
    let bin_write_t = (bin_write_t__local :> _ Bin_prot.Write.writer)
    let _ = bin_write_t

    let bin_writer_t =
      ({ size = bin_size_t; write = bin_write_t } : _ Bin_prot.Type_class.writer)
    ;;

    let _ = bin_writer_t

    let (__bin_read_t__ : (int -> t) Bin_prot.Read.reader) =
      fun _buf ~pos_ref _vint ->
      Bin_prot.Common.raise_variant_wrong_type
        "deriving_inline.ml.Global_fields_with_localize.Record.t"
        !pos_ref
    ;;

    let _ = __bin_read_t__

    let (bin_read_t : t Bin_prot.Read.reader) =
      fun buf ~pos_ref ->
      let v_a = Normal.bin_read_t buf ~pos_ref in
      let v_b = Mutable.bin_read_t buf ~pos_ref in
      let v_c = Global.bin_read_t buf ~pos_ref in
      let v_d = Ocaml_global.bin_read_t buf ~pos_ref in
      let v_e = Extension_global.bin_read_t buf ~pos_ref in
      { a = v_a; b = v_b; c = v_c; d = v_d; e = v_e }
    ;;

    let _ = bin_read_t

    let bin_reader_t =
      ({ read = bin_read_t; vtag_read = __bin_read_t__ } : _ Bin_prot.Type_class.reader)
    ;;

    let _ = bin_reader_t

    let bin_t =
      ({ writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
        : _ Bin_prot.Type_class.t)
    ;;

    let _ = bin_t

    [@@@end]
  end

  module Record_constructor = struct
    type t =
      | T of
          { a : Normal.t
          ; mutable b : Mutable.t
          ; c : Global.t
          ; d : Ocaml_global.t
          ; e : Extension_global.t
          }
    [@@deriving_inline bin_io ~localize ~hide_locations]

    let _ = fun (_ : t) -> ()

    let bin_shape_t =
      let _group =
        Bin_prot.Shape.group
          (Bin_prot.Shape.Location.of_string "<hidden>")
          [ ( Bin_prot.Shape.Tid.of_string "t"
            , []
            , Bin_prot.Shape.variant
                [ ( "T"
                  , [ Bin_prot.Shape.record
                        [ "a", Normal.bin_shape_t
                        ; "b", Mutable.bin_shape_t
                        ; "c", Global.bin_shape_t
                        ; "d", Ocaml_global.bin_shape_t
                        ; "e", Extension_global.bin_shape_t
                        ]
                    ] )
                ] )
          ]
      in
      (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) []
    ;;

    let _ = bin_shape_t

    let (bin_size_t__local : t Bin_prot.Size.sizer_local) = function
      | T { a = v1; b = v2; c = v3; d = v4; e = v5 } ->
        let size = 1 in
        let size = Bin_prot.Common.( + ) size (Normal.bin_size_t__local v1) in
        let size = Bin_prot.Common.( + ) size (Mutable.bin_size_t v2) in
        let size = Bin_prot.Common.( + ) size (Global.bin_size_t v3) in
        let size = Bin_prot.Common.( + ) size (Ocaml_global.bin_size_t v4) in
        Bin_prot.Common.( + ) size (Extension_global.bin_size_t v5)
    ;;

    let _ = bin_size_t__local
    let bin_size_t = (bin_size_t__local :> _ Bin_prot.Size.sizer)
    let _ = bin_size_t

    let (bin_write_t__local : t Bin_prot.Write.writer_local) =
      fun buf ~pos -> function
      | T { a = v1; b = v2; c = v3; d = v4; e = v5 } ->
        let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 0 in
        let pos = Normal.bin_write_t__local buf ~pos v1 in
        let pos = Mutable.bin_write_t buf ~pos v2 in
        let pos = Global.bin_write_t buf ~pos v3 in
        let pos = Ocaml_global.bin_write_t buf ~pos v4 in
        Extension_global.bin_write_t buf ~pos v5
    ;;

    let _ = bin_write_t__local
    let bin_write_t = (bin_write_t__local :> _ Bin_prot.Write.writer)
    let _ = bin_write_t

    let bin_writer_t =
      ({ size = bin_size_t; write = bin_write_t } : _ Bin_prot.Type_class.writer)
    ;;

    let _ = bin_writer_t

    let (__bin_read_t__ : (int -> t) Bin_prot.Read.reader) =
      fun _buf ~pos_ref _vint ->
      Bin_prot.Common.raise_variant_wrong_type
        "deriving_inline.ml.Global_fields_with_localize.Record_constructor.t"
        !pos_ref
    ;;

    let _ = __bin_read_t__

    let (bin_read_t : t Bin_prot.Read.reader) =
      fun buf ~pos_ref ->
      match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
      | 0 ->
        let v_a = Normal.bin_read_t buf ~pos_ref in
        let v_b = Mutable.bin_read_t buf ~pos_ref in
        let v_c = Global.bin_read_t buf ~pos_ref in
        let v_d = Ocaml_global.bin_read_t buf ~pos_ref in
        let v_e = Extension_global.bin_read_t buf ~pos_ref in
        T { a = v_a; b = v_b; c = v_c; d = v_d; e = v_e }
      | _ ->
        Bin_prot.Common.raise_read_error
          (Bin_prot.Common.ReadError.Sum_tag
             "deriving_inline.ml.Global_fields_with_localize.Record_constructor.t")
          !pos_ref
    ;;

    let _ = bin_read_t

    let bin_reader_t =
      ({ read = bin_read_t; vtag_read = __bin_read_t__ } : _ Bin_prot.Type_class.reader)
    ;;

    let _ = bin_reader_t

    let bin_t =
      ({ writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
        : _ Bin_prot.Type_class.t)
    ;;

    let _ = bin_t

    [@@@end]
  end

  module Tuple_constructor = struct
    type t = T of Normal.t * Global.t * Ocaml_global.t * Extension_global.t
    [@@deriving_inline bin_io ~localize ~hide_locations]

    let _ = fun (_ : t) -> ()

    let bin_shape_t =
      let _group =
        Bin_prot.Shape.group
          (Bin_prot.Shape.Location.of_string "<hidden>")
          [ ( Bin_prot.Shape.Tid.of_string "t"
            , []
            , Bin_prot.Shape.variant
                [ ( "T"
                  , [ Normal.bin_shape_t
                    ; Global.bin_shape_t
                    ; Ocaml_global.bin_shape_t
                    ; Extension_global.bin_shape_t
                    ] )
                ] )
          ]
      in
      (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) []
    ;;

    let _ = bin_shape_t

    let (bin_size_t__local : t Bin_prot.Size.sizer_local) = function
      | T (v1, v2, v3, v4) ->
        let size = 1 in
        let size = Bin_prot.Common.( + ) size (Normal.bin_size_t__local v1) in
        let size = Bin_prot.Common.( + ) size (Global.bin_size_t v2) in
        let size = Bin_prot.Common.( + ) size (Ocaml_global.bin_size_t v3) in
        Bin_prot.Common.( + ) size (Extension_global.bin_size_t v4)
    ;;

    let _ = bin_size_t__local
    let bin_size_t = (bin_size_t__local :> _ Bin_prot.Size.sizer)
    let _ = bin_size_t

    let (bin_write_t__local : t Bin_prot.Write.writer_local) =
      fun buf ~pos -> function
      | T (v1, v2, v3, v4) ->
        let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 0 in
        let pos = Normal.bin_write_t__local buf ~pos v1 in
        let pos = Global.bin_write_t buf ~pos v2 in
        let pos = Ocaml_global.bin_write_t buf ~pos v3 in
        Extension_global.bin_write_t buf ~pos v4
    ;;

    let _ = bin_write_t__local
    let bin_write_t = (bin_write_t__local :> _ Bin_prot.Write.writer)
    let _ = bin_write_t

    let bin_writer_t =
      ({ size = bin_size_t; write = bin_write_t } : _ Bin_prot.Type_class.writer)
    ;;

    let _ = bin_writer_t

    let (__bin_read_t__ : (int -> t) Bin_prot.Read.reader) =
      fun _buf ~pos_ref _vint ->
      Bin_prot.Common.raise_variant_wrong_type
        "deriving_inline.ml.Global_fields_with_localize.Tuple_constructor.t"
        !pos_ref
    ;;

    let _ = __bin_read_t__

    let (bin_read_t : t Bin_prot.Read.reader) =
      fun buf ~pos_ref ->
      match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
      | 0 ->
        let arg_1 = Normal.bin_read_t buf ~pos_ref in
        let arg_2 = Global.bin_read_t buf ~pos_ref in
        let arg_3 = Ocaml_global.bin_read_t buf ~pos_ref in
        let arg_4 = Extension_global.bin_read_t buf ~pos_ref in
        T (arg_1, arg_2, arg_3, arg_4)
      | _ ->
        Bin_prot.Common.raise_read_error
          (Bin_prot.Common.ReadError.Sum_tag
             "deriving_inline.ml.Global_fields_with_localize.Tuple_constructor.t")
          !pos_ref
    ;;

    let _ = bin_read_t

    let bin_reader_t =
      ({ read = bin_read_t; vtag_read = __bin_read_t__ } : _ Bin_prot.Type_class.reader)
    ;;

    let _ = bin_reader_t

    let bin_t =
      ({ writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
        : _ Bin_prot.Type_class.t)
    ;;

    let _ = bin_t

    [@@@end]
  end
end
