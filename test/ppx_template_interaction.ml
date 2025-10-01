open! Bin_prot.Std

(* Demonstrate that [@@deriving bin_write [@mode local]] is equivalent to
   [@@deriving bin_write ~localize]. *)
module%template With_bin_write : sig
  type t [@@deriving_inline bin_write [@mode local]]

  include sig
    [@@@ocaml.warning "-32"]

    val bin_size_t : t Bin_prot.Size.sizer
    val bin_size_t__local : t Bin_prot.Size.sizer__local
    val bin_write_t : t Bin_prot.Write.writer
    val bin_write_t__local : t Bin_prot.Write.writer__local
    val bin_writer_t : t Bin_prot.Type_class.writer
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline bin_write [@mode local]]

  let _ = fun (_ : t) -> ()
  let bin_size_t__local : t Bin_prot.Size.sizer__local = bin_size_int__local
  let _ = bin_size_t__local
  let bin_size_t = (bin_size_t__local :> _ Bin_prot.Size.sizer)
  let _ = bin_size_t
  let bin_write_t__local : t Bin_prot.Write.writer__local = bin_write_int__local
  let _ = bin_write_t__local
  let bin_write_t = (bin_write_t__local :> _ Bin_prot.Write.writer)
  let _ = bin_write_t

  let bin_writer_t : t Bin_prot.Type_class.writer =
    { size = bin_size_t; write = bin_write_t }
  ;;

  let _ = bin_writer_t

  [@@@end]
end

(* Check that the signatures match. *)
module _ : sig
  type t [@@deriving bin_write ~localize]
end =
  With_bin_write

module _ : module type of With_bin_write = struct
  type t = int [@@deriving bin_write ~localize]
end

(* Demonstrate that [@@deriving bin_io [@mode local]] is equivalent to
   [@@deriving bin_io ~localize]. *)
module%template With_bin_io : sig
  type t [@@deriving_inline bin_io [@mode local]]

  include sig
    [@@@ocaml.warning "-32"]

    include Bin_prot.Binable.S__local with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving_inline bin_io [@mode local]]

  let _ = fun (_ : t) -> ()

  let bin_shape_t =
    let _group =
      Bin_prot.Shape.group
        (Bin_prot.Shape.Location.of_string
           "ppx/ppx_bin_prot/test/ppx_template_interaction.ml:66:2")
        [ Bin_prot.Shape.Tid.of_string "t", [], bin_shape_int ]
    in
    (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) []
  ;;

  let _ = bin_shape_t
  let bin_size_t__local : t Bin_prot.Size.sizer__local = bin_size_int__local
  let _ = bin_size_t__local
  let bin_size_t = (bin_size_t__local :> _ Bin_prot.Size.sizer)
  let _ = bin_size_t
  let bin_write_t__local : t Bin_prot.Write.writer__local = bin_write_int__local
  let _ = bin_write_t__local
  let bin_write_t = (bin_write_t__local :> _ Bin_prot.Write.writer)
  let _ = bin_write_t

  let bin_writer_t : t Bin_prot.Type_class.writer =
    { size = bin_size_t; write = bin_write_t }
  ;;

  let _ = bin_writer_t
  let __bin_read_t__ : t Bin_prot.Read.vtag_reader = __bin_read_int__
  let _ = __bin_read_t__
  let bin_read_t : t Bin_prot.Read.reader = bin_read_int
  let _ = bin_read_t

  let bin_reader_t : t Bin_prot.Type_class.reader =
    { read = bin_read_t; vtag_read = __bin_read_t__ }
  ;;

  let _ = bin_reader_t

  let bin_t =
    ({ writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
     : _ Bin_prot.Type_class.t)
  ;;

  let _ = bin_t

  [@@@end]
end

(* Check that the signatures match. *)
module _ : sig
  type t [@@deriving bin_io ~localize]
end =
  With_bin_io

module _ : module type of With_bin_io = struct
  type t = int [@@deriving bin_io ~localize]
end

(* Demonstrate that bin_io can be used with multiple nonrecursive bindings. *)
[%%template
type t [@@kind k = (value, word)] [@@deriving_inline bin_io]

let _ = fun (_ : t) -> ()
let _ = fun (_ : t__word) -> ()

let bin_shape_t, bin_shape_t__word =
  let _group =
    Bin_prot.Shape.group
      (Bin_prot.Shape.Location.of_string
         "ppx/ppx_bin_prot/test/ppx_template_interaction.ml:128:0")
      [ Bin_prot.Shape.Tid.of_string "t", [], Bin_prot.Shape.variant []
      ; Bin_prot.Shape.Tid.of_string "t__word", [], Bin_prot.Shape.variant []
      ]
  in
  ( (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t")) []
  , (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "t__word")) [] )
;;

let _ = bin_shape_t
and _ = bin_shape_t__word

let bin_size_t : t Bin_prot.Size.sizer =
  fun _v -> raise (Bin_prot.Common.Empty_type "ppx_template_interaction.ml.t")

and bin_size_t__word : t__word Bin_prot.Size.sizer =
  fun _v -> raise (Bin_prot.Common.Empty_type "ppx_template_interaction.ml.t__word")
;;

let _ = bin_size_t
and _ = bin_size_t__word

let bin_write_t : t Bin_prot.Write.writer =
  fun _buf ~pos:_ _v -> raise (Bin_prot.Common.Empty_type "ppx_template_interaction.ml.t")

and bin_write_t__word : t__word Bin_prot.Write.writer =
  fun _buf ~pos:_ _v ->
  raise (Bin_prot.Common.Empty_type "ppx_template_interaction.ml.t__word")
;;

let _ = bin_write_t
and _ = bin_write_t__word

let bin_writer_t : t Bin_prot.Type_class.writer =
  { size = bin_size_t; write = bin_write_t }

and bin_writer_t__word : t__word Bin_prot.Type_class.writer =
  { size = bin_size_t__word; write = bin_write_t__word }
;;

let _ = bin_writer_t
and _ = bin_writer_t__word

let __bin_read_t__ : t Bin_prot.Read.vtag_reader =
  fun _buf ~pos_ref _vint ->
  match
    Bin_prot.Common.raise_variant_wrong_type "ppx_template_interaction.ml.t" !pos_ref
  with
  | (_ : Bin_prot.Common.nothing) -> .

and __bin_read_t__word__ : t__word Bin_prot.Read.vtag_reader =
  fun _buf ~pos_ref _vint ->
  match
    Bin_prot.Common.raise_variant_wrong_type
      "ppx_template_interaction.ml.t__word"
      !pos_ref
  with
  | (_ : Bin_prot.Common.nothing) -> .
;;

let _ = __bin_read_t__
and _ = __bin_read_t__word__

let bin_read_t : t Bin_prot.Read.reader =
  fun buf ~pos_ref ->
  (fun _buf ~pos_ref ->
    Bin_prot.Common.raise_read_error
      (Bin_prot.Common.ReadError.Empty_type "ppx_template_interaction.ml.t")
      !pos_ref)
    buf
    ~pos_ref

and bin_read_t__word : t__word Bin_prot.Read.reader =
  fun buf ~pos_ref ->
  (fun _buf ~pos_ref ->
    Bin_prot.Common.raise_read_error
      (Bin_prot.Common.ReadError.Empty_type "ppx_template_interaction.ml.t__word")
      !pos_ref)
    buf
    ~pos_ref
;;

let _ = bin_read_t
and _ = bin_read_t__word

let bin_reader_t : t Bin_prot.Type_class.reader =
  { read = bin_read_t; vtag_read = __bin_read_t__ }

and bin_reader_t__word : t__word Bin_prot.Type_class.reader =
  { read = bin_read_t__word; vtag_read = __bin_read_t__word__ }
;;

let _ = bin_reader_t
and _ = bin_reader_t__word

let bin_t =
  ({ writer = bin_writer_t; reader = bin_reader_t; shape = bin_shape_t }
   : _ Bin_prot.Type_class.t)

and bin_t__word =
  ({ writer = bin_writer_t__word; reader = bin_reader_t__word; shape = bin_shape_t__word }
   : _ Bin_prot.Type_class.t)
;;

let _ = bin_t
and _ = bin_t__word

[@@@end]]
