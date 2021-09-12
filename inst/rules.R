rule_expression <- style(
  expr(
    "op-left-brace",
    "op-right-brace"
  )
)

rule_function = style(
  "function",
  "op-left-paren",
  with_indentation(
    2L,
    conditional_sequence(
      any_token(is_multiline, filter = "expr", until = "op-right-paren"),
      cycle(
        whitespace(newline = 1L),
        indent(),
        "symbol_formals",
        zero_or_one(
          whitespace(1L),
          "eq_formals",
          whitespace(1L),
          "expr"
        ),
        collapse = "op-comma"
      ), 
      cycle(
        "symbol_formals",
        zero_or_one(
          whitespace(1L),
          "eq_formals",
          whitespace(1L),
          "expr"
        ),
        conditional_sequence(
          any_token(
            is_multiline %or% overflows_width,
            until = c("symbol_formals", "op-right-paren")
          ),
          whitespace(newline = 1L),
          list()
        ),
        collapse = "op-comma"
        )
      )
    ),
  "op-right-paren",
  whitespace(1L),
  "expr"
)

