open Bin_prot.Std

module type S = sig
  type t [@@deriving bin_io ~localize]
end

module type S1 = sig
  type 'a t [@@deriving bin_io ~localize]
end

module type S2 = sig
  type ('a, 'b) t [@@deriving bin_io ~localize]
end

include (
struct
  type t = int [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type t = int32 [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type t = int64 [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type t = nativeint [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type t = float [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type t = char [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type t = int list [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type t = float array [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type t = int64 array [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type t = int * float * char [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type u =
    | A
    | B
  [@@deriving bin_io ~localize]

  type t =
    | C
    | D
    | E of u
  [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type u =
    [ `A
    | `B
    ]
  [@@deriving bin_io ~localize]

  type t =
    [ `C
    | `D
    | `E of u
    ]
  [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type a =
    [ `A1
    | `A2
    ]
  [@@deriving bin_io ~localize]

  type b =
    [ `B1
    | `B2
    ]
  [@@deriving bin_io ~localize]

  type t =
    [ a
    | b
    ]
  [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type t =
    { foo : char
    ; bar : int
    ; baz : string
    }
  [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type t =
    | A of
        { foo : char
        ; bar : int
        ; baz : string
        }
    | B of int
    | C of char * int * string
  [@@deriving bin_io ~localize]
end :
  S)

include (
struct
  type 'a t = 'a [@@deriving bin_io ~localize]
end :
  S1)

include (
struct
  type 'a t = 'a * int [@@deriving bin_io ~localize]
end :
  S1)

include (
struct
  type ('a, 'b) t = 'a * 'b [@@deriving bin_io ~localize]
end :
  S2)

include (
struct
  type 'a u = 'a constraint 'a = [< `A | `B ] [@@deriving bin_io ~localize]
  type 'a t = [ `A ] u [@@deriving bin_io ~localize]
end :
  S1)

include (
struct
  type 'a t =
    { foo : 'a
    ; bar : int
    }
  [@@deriving bin_io ~localize]
end :
  S1)

include (
struct
  type 'a t =
    | A of
        { foo : 'a
        ; bar : int
        }
    | B of 'a
    | C
  [@@deriving bin_io ~localize]
end :
  S1)
