token_aliases <- list(
  "<-" = "left_assign",
  "->" = "right_assign",
  "," = "op_comma",
  "{" = "op_left_brace",
  "}" = "op_right_brace",
  "(" = "op_left_paren",
  ")" = "op_right_paren",
  "*" = "op_star",
  "-" = "op_minus",
  "+" = "op_plus",
  "/" = "op_slash",
  "^" = "op_caret",
  ";" = "op_semicolon",
  "=" = either_token(
    "eq_formals",
    "eq_assign"
  )
)
