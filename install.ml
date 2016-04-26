#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"ppx_fields_conv"
  [ oasis_lib "ppx_fields_conv"
  ; file "META" ~section:"lib"
  ]
