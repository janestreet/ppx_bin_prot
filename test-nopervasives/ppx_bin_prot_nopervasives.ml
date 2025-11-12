open Base
open Bin_prot.Std

module M = struct
  type t =
    { a : float
    ; b : float
    }
  [@@deriving bin_io]
end
