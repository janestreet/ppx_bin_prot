open! Bin_prot.Std

[@@@warning "-unused-module"]

module Bin_shape : sig
  type t [@@warning "-unused-type-declaration"] [@@deriving_inline bin_shape ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val bin_shape_t : Bin_prot.Shape.t @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline bin_shape ~portable]

  let _ = fun (_ : t) -> ()

  let bin_shape_t @ portable =
    let _group =
      Bin_prot.Shape.group
        (Bin_prot.Shape.Location.of_string "ppx/ppx_bin_prot/test/modalities.ml:17:2")
        [ Bin_prot.Shape.Tid.of_string "t", [], bin_shape_int ]
    in
    (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) []
  ;;

  let _ = bin_shape_t

  [@@@end]
end

module Bin_write : sig
  type t [@@deriving_inline bin_write ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val bin_size_t : t Bin_prot.Size.sizer @@ portable
    val bin_write_t : t Bin_prot.Write.writer @@ portable
    val bin_writer_t : t Bin_prot.Type_class.writer @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline bin_write ~portable]

  let _ = fun (_ : t) -> ()
  let bin_size_t : t Bin_prot.Size.sizer @ portable = bin_size_int
  let _ = bin_size_t
  let bin_write_t : t Bin_prot.Write.writer @ portable = bin_write_int
  let _ = bin_write_t

  let bin_writer_t : t Bin_prot.Type_class.writer @ portable =
    { size = bin_size_t; write = bin_write_t }
  ;;

  let _ = bin_writer_t

  [@@@end]
end

module Bin_write_local : sig
  type t [@@deriving_inline bin_write ~localize ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val bin_size_t : t Bin_prot.Size.sizer @@ portable
    val bin_size_t__local : t Bin_prot.Size.sizer__local @@ portable
    val bin_write_t : t Bin_prot.Write.writer @@ portable
    val bin_write_t__local : t Bin_prot.Write.writer__local @@ portable
    val bin_writer_t : t Bin_prot.Type_class.writer @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline bin_write ~localize ~portable]

  let _ = fun (_ : t) -> ()
  let bin_size_t__local : t Bin_prot.Size.sizer__local @ portable = bin_size_int__local
  let _ = bin_size_t__local
  let bin_size_t @ portable = (bin_size_t__local :> _ Bin_prot.Size.sizer)
  let _ = bin_size_t

  let bin_write_t__local : t Bin_prot.Write.writer__local @ portable =
    bin_write_int__local
  ;;

  let _ = bin_write_t__local
  let bin_write_t @ portable = (bin_write_t__local :> _ Bin_prot.Write.writer)
  let _ = bin_write_t

  let bin_writer_t : t Bin_prot.Type_class.writer @ portable =
    { size = bin_size_t; write = bin_write_t }
  ;;

  let _ = bin_writer_t

  [@@@end]
end

module%template [@mode local] Bin_write : sig
  type t [@@deriving_inline (bin_write [@mode local]) ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val bin_size_t : t Bin_prot.Size.sizer @@ portable
    val bin_size_t__local : t Bin_prot.Size.sizer__local @@ portable
    val bin_write_t : t Bin_prot.Write.writer @@ portable
    val bin_write_t__local : t Bin_prot.Write.writer__local @@ portable
    val bin_writer_t : t Bin_prot.Type_class.writer @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline (bin_write [@mode local]) ~portable]

  let _ = fun (_ : t) -> ()
  let bin_size_t__local : t Bin_prot.Size.sizer__local @ portable = bin_size_int__local
  let _ = bin_size_t__local
  let bin_size_t @ portable = (bin_size_t__local :> _ Bin_prot.Size.sizer)
  let _ = bin_size_t

  let bin_write_t__local : t Bin_prot.Write.writer__local @ portable =
    bin_write_int__local
  ;;

  let _ = bin_write_t__local
  let bin_write_t @ portable = (bin_write_t__local :> _ Bin_prot.Write.writer)
  let _ = bin_write_t

  let bin_writer_t : t Bin_prot.Type_class.writer @ portable =
    { size = bin_size_t; write = bin_write_t }
  ;;

  let _ = bin_writer_t

  [@@@end]
end

module Bin_read : sig
  type t [@@deriving_inline bin_read ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val bin_read_t : t Bin_prot.Read.reader @@ portable
    val __bin_read_t__ : t Bin_prot.Read.vtag_reader @@ portable
    val bin_reader_t : t Bin_prot.Type_class.reader @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline bin_read ~portable]

  let _ = fun (_ : t) -> ()
  let __bin_read_t__ : t Bin_prot.Read.vtag_reader @ portable = __bin_read_int__
  let _ = __bin_read_t__
  let bin_read_t : t Bin_prot.Read.reader @ portable = bin_read_int
  let _ = bin_read_t

  let bin_reader_t : t Bin_prot.Type_class.reader @ portable =
    { read = bin_read_t; vtag_read = __bin_read_t__ }
  ;;

  let _ = bin_reader_t

  [@@@end]
end

module Bin_type_class : sig
  type t [@@deriving_inline bin_type_class ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val bin_t : t Bin_prot.Type_class.t @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  let bin_shape_t = bin_shape_int
  let bin_reader_t = bin_reader_int
  let bin_writer_t = bin_writer_int

  type t = int [@@deriving_inline bin_type_class ~portable]

  let _ = fun (_ : t) -> ()

  let bin_t @ portable =
    ({ writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
     : _ Bin_prot.Type_class.t)
  ;;

  let _ = bin_t

  [@@@end]
end

module Bin_io : sig
  type t [@@deriving_inline bin_io ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    include Bin_prot.Binable.S with type t := t @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline bin_io ~portable]

  let _ = fun (_ : t) -> ()

  let bin_shape_t @ portable =
    let _group =
      Bin_prot.Shape.group
        (Bin_prot.Shape.Location.of_string "ppx/ppx_bin_prot/test/modalities.ml:221:2")
        [ Bin_prot.Shape.Tid.of_string "t", [], bin_shape_int ]
    in
    (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) []
  ;;

  let _ = bin_shape_t
  let bin_size_t : t Bin_prot.Size.sizer @ portable = bin_size_int
  let _ = bin_size_t
  let bin_write_t : t Bin_prot.Write.writer @ portable = bin_write_int
  let _ = bin_write_t

  let bin_writer_t : t Bin_prot.Type_class.writer @ portable =
    { size = bin_size_t; write = bin_write_t }
  ;;

  let _ = bin_writer_t
  let __bin_read_t__ : t Bin_prot.Read.vtag_reader @ portable = __bin_read_int__
  let _ = __bin_read_t__
  let bin_read_t : t Bin_prot.Read.reader @ portable = bin_read_int
  let _ = bin_read_t

  let bin_reader_t : t Bin_prot.Type_class.reader @ portable =
    { read = bin_read_t; vtag_read = __bin_read_t__ }
  ;;

  let _ = bin_reader_t

  let bin_t @ portable =
    ({ writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
     : _ Bin_prot.Type_class.t)
  ;;

  let _ = bin_t

  [@@@end]
end

module Bin_io_local : sig
  type t [@@deriving_inline bin_io ~localize ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    include Bin_prot.Binable.S__local with type t := t @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline bin_io ~localize ~portable]

  let _ = fun (_ : t) -> ()

  let bin_shape_t @ portable =
    let _group =
      Bin_prot.Shape.group
        (Bin_prot.Shape.Location.of_string "ppx/ppx_bin_prot/test/modalities.ml:278:2")
        [ Bin_prot.Shape.Tid.of_string "t", [], bin_shape_int ]
    in
    (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) []
  ;;

  let _ = bin_shape_t
  let bin_size_t__local : t Bin_prot.Size.sizer__local @ portable = bin_size_int__local
  let _ = bin_size_t__local
  let bin_size_t @ portable = (bin_size_t__local :> _ Bin_prot.Size.sizer)
  let _ = bin_size_t

  let bin_write_t__local : t Bin_prot.Write.writer__local @ portable =
    bin_write_int__local
  ;;

  let _ = bin_write_t__local
  let bin_write_t @ portable = (bin_write_t__local :> _ Bin_prot.Write.writer)
  let _ = bin_write_t

  let bin_writer_t : t Bin_prot.Type_class.writer @ portable =
    { size = bin_size_t; write = bin_write_t }
  ;;

  let _ = bin_writer_t
  let __bin_read_t__ : t Bin_prot.Read.vtag_reader @ portable = __bin_read_int__
  let _ = __bin_read_t__
  let bin_read_t : t Bin_prot.Read.reader @ portable = bin_read_int
  let _ = bin_read_t

  let bin_reader_t : t Bin_prot.Type_class.reader @ portable =
    { read = bin_read_t; vtag_read = __bin_read_t__ }
  ;;

  let _ = bin_reader_t

  let bin_t @ portable =
    ({ writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
     : _ Bin_prot.Type_class.t)
  ;;

  let _ = bin_t

  [@@@end]
end

module%template [@mode local] Bin_io : sig
  type t [@@deriving_inline (bin_io [@mode local]) ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    include Bin_prot.Binable.S__local with type t := t @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline (bin_io [@mode local]) ~portable]

  let _ = fun (_ : t) -> ()

  let bin_shape_t @ portable =
    let _group =
      Bin_prot.Shape.group
        (Bin_prot.Shape.Location.of_string "ppx/ppx_bin_prot/test/modalities.ml:343:2")
        [ Bin_prot.Shape.Tid.of_string "t", [], bin_shape_int ]
    in
    (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) []
  ;;

  let _ = bin_shape_t
  let bin_size_t__local : t Bin_prot.Size.sizer__local @ portable = bin_size_int__local
  let _ = bin_size_t__local
  let bin_size_t @ portable = (bin_size_t__local :> _ Bin_prot.Size.sizer)
  let _ = bin_size_t

  let bin_write_t__local : t Bin_prot.Write.writer__local @ portable =
    bin_write_int__local
  ;;

  let _ = bin_write_t__local
  let bin_write_t @ portable = (bin_write_t__local :> _ Bin_prot.Write.writer)
  let _ = bin_write_t

  let bin_writer_t : t Bin_prot.Type_class.writer @ portable =
    { size = bin_size_t; write = bin_write_t }
  ;;

  let _ = bin_writer_t
  let __bin_read_t__ : t Bin_prot.Read.vtag_reader @ portable = __bin_read_int__
  let _ = __bin_read_t__
  let bin_read_t : t Bin_prot.Read.reader @ portable = bin_read_int
  let _ = bin_read_t

  let bin_reader_t : t Bin_prot.Type_class.reader @ portable =
    { read = bin_read_t; vtag_read = __bin_read_t__ }
  ;;

  let _ = bin_reader_t

  let bin_t @ portable =
    ({ writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
     : _ Bin_prot.Type_class.t)
  ;;

  let _ = bin_t

  [@@@end]
end

module Recursive : sig
  type 'a t
  and u [@@deriving_inline bin_io ~portable]

  include sig
    [@@@ocaml.warning "-32"]

    val bin_shape_t : Bin_prot.Shape.t -> Bin_prot.Shape.t @@ portable
    val bin_shape_u : Bin_prot.Shape.t @@ portable
    val bin_size_t : 'a. 'a Bin_prot.Size.sizer -> 'a t Bin_prot.Size.sizer @@ portable

    val bin_write_t
      : 'a.
      'a Bin_prot.Write.writer -> 'a t Bin_prot.Write.writer
      @@ portable

    val bin_writer_t
      : 'a.
      'a Bin_prot.Type_class.writer -> 'a t Bin_prot.Type_class.writer
      @@ portable

    val bin_size_u : u Bin_prot.Size.sizer @@ portable
    val bin_write_u : u Bin_prot.Write.writer @@ portable
    val bin_writer_u : u Bin_prot.Type_class.writer @@ portable
    val bin_read_t : 'a. 'a Bin_prot.Read.reader -> 'a t Bin_prot.Read.reader @@ portable

    val __bin_read_t__
      : 'a.
      'a Bin_prot.Read.reader -> 'a t Bin_prot.Read.vtag_reader
      @@ portable

    val bin_reader_t
      : 'a.
      'a Bin_prot.Type_class.reader -> 'a t Bin_prot.Type_class.reader
      @@ portable

    val bin_read_u : u Bin_prot.Read.reader @@ portable
    val __bin_read_u__ : u Bin_prot.Read.vtag_reader @@ portable
    val bin_reader_u : u Bin_prot.Type_class.reader @@ portable
    val bin_t : 'a. 'a Bin_prot.Type_class.t -> 'a t Bin_prot.Type_class.t @@ portable
    val bin_u : u Bin_prot.Type_class.t @@ portable
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type 'a t =
    { a : 'a
    ; u : u
    }

  and u = { t : int t } [@@deriving_inline bin_io ~portable]

  let _ = fun (_ : 'a t) -> ()
  let _ = fun (_ : u) -> ()

  let (bin_shape_t, bin_shape_u) @ portable =
    let _group =
      Bin_prot.Shape.group
        (Bin_prot.Shape.Location.of_string "ppx/ppx_bin_prot/test/modalities.ml:442:2")
        [ ( Bin_prot.Shape.Tid.of_string "t"
          , [ Bin_prot.Shape.Vid.of_string "a" ]
          , Bin_prot.Shape.record
              [ ( "a"
                , Bin_prot.Shape.var
                    (Bin_prot.Shape.Location.of_string
                       "ppx/ppx_bin_prot/test/modalities.ml:443:10")
                    (Bin_prot.Shape.Vid.of_string "a") )
              ; "u", (Bin_prot.Shape.rec_app (Bin_prot.Shape.Tid.of_string "u")) []
              ] )
        ; ( Bin_prot.Shape.Tid.of_string "u"
          , []
          , Bin_prot.Shape.record
              [ ( "t"
                , (Bin_prot.Shape.rec_app (Bin_prot.Shape.Tid.of_string "t"))
                    [ bin_shape_int ] )
              ] )
        ]
    in
    ( (fun a -> (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) [ a ])
    , (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "u")) [] )
  ;;

  let _ = bin_shape_t
  and _ = bin_shape_u

  let rec bin_size_t : 'a. ('a Bin_prot.Size.sizer -> 'a t Bin_prot.Size.sizer) @ portable
    =
    fun _size_of_a -> function
    | { a = v1; u = v2 } ->
      let size = 0 in
      let size = Bin_prot.Common.( + ) size (_size_of_a v1) in
      Bin_prot.Common.( + ) size (bin_size_u v2)

  and bin_size_u : u Bin_prot.Size.sizer @ portable = function
    | { t = v1 } ->
      let size = 0 in
      Bin_prot.Common.( + ) size (bin_size_t bin_size_int v1)
  ;;

  let _ = bin_size_t
  and _ = bin_size_u

  let rec bin_write_t
    : 'a. ('a Bin_prot.Write.writer -> 'a t Bin_prot.Write.writer) @ portable
    =
    fun _write_a buf ~pos -> function
    | { a = v1; u = v2 } ->
      let pos = _write_a buf ~pos v1 in
      bin_write_u buf ~pos v2

  and bin_write_u : u Bin_prot.Write.writer @ portable =
    fun buf ~pos -> function
    | { t = v1 } -> bin_write_t bin_write_int buf ~pos v1
  ;;

  let _ = bin_write_t
  and _ = bin_write_u

  let bin_writer_t
    : 'a. ('a Bin_prot.Type_class.writer -> 'a t Bin_prot.Type_class.writer) @ portable
    =
    fun bin_writer_a ->
    { size = (fun v -> bin_size_t bin_writer_a.size v)
    ; write = (fun buf ~pos v -> bin_write_t bin_writer_a.write buf ~pos v)
    }

  and bin_writer_u : u Bin_prot.Type_class.writer @ portable =
    { size = bin_size_u; write = bin_write_u }
  ;;

  let _ = bin_writer_t
  and _ = bin_writer_u

  let rec __bin_read_t__
    : 'a. ('a Bin_prot.Read.reader -> 'a t Bin_prot.Read.vtag_reader) @ portable
    =
    fun _of__a _buf ~pos_ref _vint ->
    match
      Bin_prot.Common.raise_variant_wrong_type "modalities.ml.Recursive.t" !pos_ref
    with
    | (_ : Bin_prot.Common.nothing) -> .

  and __bin_read_u__ : u Bin_prot.Read.vtag_reader @ portable =
    fun _buf ~pos_ref _vint ->
    match
      Bin_prot.Common.raise_variant_wrong_type "modalities.ml.Recursive.u" !pos_ref
    with
    | (_ : Bin_prot.Common.nothing) -> .

  and bin_read_t : 'a. ('a Bin_prot.Read.reader -> 'a t Bin_prot.Read.reader) @ portable =
    fun _of__a buf ~pos_ref ->
    let v_a = _of__a buf ~pos_ref in
    let v_u = bin_read_u buf ~pos_ref in
    { a = v_a; u = v_u }

  and bin_read_u : u Bin_prot.Read.reader @ portable =
    fun buf ~pos_ref ->
    let v_t = (bin_read_t bin_read_int) buf ~pos_ref in
    { t = v_t }
  ;;

  let _ = __bin_read_t__
  and _ = __bin_read_u__
  and _ = bin_read_t
  and _ = bin_read_u

  let bin_reader_t
    : 'a. ('a Bin_prot.Type_class.reader -> 'a t Bin_prot.Type_class.reader) @ portable
    =
    fun bin_reader_a ->
    { read = (fun buf ~pos_ref -> (bin_read_t bin_reader_a.read) buf ~pos_ref)
    ; vtag_read =
        (fun buf ~pos_ref vtag -> (__bin_read_t__ bin_reader_a.read) buf ~pos_ref vtag)
    }

  and bin_reader_u : u Bin_prot.Type_class.reader @ portable =
    { read = bin_read_u; vtag_read = __bin_read_u__ }
  ;;

  let _ = bin_reader_t
  and _ = bin_reader_u

  let bin_t @ portable =
    (fun bin_a ->
       { writer = bin_writer_t bin_a.writer
       ; reader = bin_reader_t bin_a.reader
       ; shape = bin_shape_t bin_a.shape
       }
     : _ Bin_prot.Type_class.t -> _ Bin_prot.Type_class.t)

  and bin_u @ portable =
    ({ writer = bin_writer_u; reader = bin_reader_u; shape = bin_shape_u }
     : _ Bin_prot.Type_class.t)
  ;;

  let _ = bin_t
  and _ = bin_u

  [@@@end]
end
