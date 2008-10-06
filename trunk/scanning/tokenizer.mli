type token =
    Identifier of string
  | Decimal of string
  | Octal of string
  | Hexadecimal of string
  | LongDecimal of string
  | LongOctal of string
  | LongHexadecimal of string
  | Float of string
  | Str of string
  | Symbol of string
val _hex_not_terminated : exn
val _invalid_character : char -> exn
val reading_state : char list -> token list -> token list
val identifier_state : char list -> token list -> string -> token list
val string_state : char list -> token list -> string -> token list
val octal_start_state : char list -> token list -> string -> token list
val hex_start_state : char list -> token list -> string -> token list
val hex_state : char list -> token list -> string -> token list
val decimal_state : char list -> token list -> string -> token list
val float_state : char list -> token list -> string -> token list
val octal_state : char list -> token list -> string -> token list
val long_hex_state : char list -> token list -> string -> token list
val long_octal_state : char list -> token list -> string -> token list
val long_decimal_state : char list -> token list -> string -> token list
val end_state : token list -> token list
val tokenize : char list -> token list
val list : token list
