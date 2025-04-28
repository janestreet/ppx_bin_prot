open Bin_prot.Std

[@@@warning "-unused-module"]

type t = float [@@deriving bin_io ~localize]

module M : sig
  type t = float [@@deriving bin_io ~localize]
end = struct
  type nonrec t = t [@@deriving bin_io ~localize]
end

module M1 : sig
  type t = float list [@@deriving bin_io ~localize]
end = struct
  type nonrec t = t list [@@deriving bin_io ~localize]
end

module M2 : sig
  type nonrec t = t list [@@deriving bin_io ~localize]
end = struct
  type nonrec t = t list [@@deriving bin_io ~localize]
end

module M3 : sig
  type nonrec t = [ `A of t ] [@@deriving bin_io ~localize]
end = struct
  type nonrec t = [ `A of t ] [@@deriving bin_io ~localize]
end

module M4 : sig
  type nonrec foo = [ `new_foo ]

  and bar =
    [ `old_foo
    | `bar
    ]
  [@@deriving bin_read]
end = struct
  (* Avoid "multiple definition of the type name foo" error. *)
  include struct
    type foo = [ `old_foo ] [@@deriving bin_read]
  end

  (* The fact that this compiles demonstrates that [__bin_read_bar__] is able to refer to
     the old [__bin_read_foo__] from above without it being shadowed by the new
     [__bin_read_foo__] from below. *)
  type nonrec foo = [ `new_foo ]

  and bar =
    [ foo
    | `bar
    ]
  [@@deriving bin_read]
end
