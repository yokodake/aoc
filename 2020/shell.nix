{pkgs ? import <nixpkgs> {} }:
with pkgs;

let
  ocamlPackages = recurseIntoAttrs pkgs.ocamlPackages;
  ocamlVersion = (builtins.parseDrvName ocamlPackages.ocaml.name).version;
  findlibSiteLib = "${ocamlPackages.findlib}/lib/ocaml/${ocamlVersion}/site-lib";
  ocamlInit = writeText "ocamlinit" ''
    let () =
      try Topdirs.dir_directory "${findlibSiteLib}"
      with Not_found -> ()
    ;;

    #use "topfind";;
    #thread;;
    #require "batteries";;
  '';
in
mkShell rec {
  buildInputs =  [
    dune
  ] ++ ( with ocamlPackages;
  [
    ocaml # contains ocamllex
    findlib
    utop
    merlin
    ocp-indent

    core
    batteries

    menhir
  ]);
  IN_NIX_SHELL = 1;
  UTOP_SITE_LISP = "${ocamlPackages.utop}/share/emacs/site-lisp";
  MERLIN_SITE_LISP = "${ocamlPackages.merlin}/share/emacs/site-lisp";
  OCP_INDENT_SITE_LISP="${ocamlPackages.ocp-indent}/share/emacs/site-lisp";
  OCAMLINIT = "${ocamlInit}";
  shellHook = ''
    alias utop="utop -init ${ocamlInit}"
    alias ocaml="ocaml -init ${ocamlInit}"
  '';
}
