#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"ppx_bin_prot"
  [ oasis_lib "ppx_bin_prot"
  ; file "META" ~section:"lib"
  ]
