open Ppxlib

module For_f_sharp : sig
  val bin_write
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> structure_item list

  val bin_read
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> structure_item list
end
