{
open Lexing
open Parse

exception SyntaxError of string
}

let space = ' '
let eol = '\r' | '\n' | "\r\n"

let int = ['0'-'9'] ['0'-'9']*
let color = ['a'-'z']+ ' ' ['a'-'z']+

let bags = " bag" ('s')?
let nothing = " no other"

rule token = parse
  | eol        { EOL }
  | eof        { EOF }

  | '.'        { DOT }
  | ','        { COMMA }
  | " contain" { CONT }
  | bags       { BAGS }
  | nothing    { NO }
  | color as c { COLOR c }
  | int as i   { INT (int_of_string i) }
  | space      { token lexbuf }
  | _ as c     { raise (SyntaxError ("Unexpected char or sequence: " ^ (String.make 1 c))) }
