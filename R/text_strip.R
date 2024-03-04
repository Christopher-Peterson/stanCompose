## Text Stripping Functions ####

#' Strip outer `@(macro ... )` and `@macro` from macros
strip_macro_at = \(x) {
  paren_wrapped = str_detect(x, '^\\@\\(.+\\)$')
  x[paren_wrapped] = str_remove_all(x[paren_wrapped], '^\\@\\(|\\)$')
  x[!paren_wrapped] = str_remove(x[!paren_wrapped], '^\\@')
  x
}
#' Strip the outer `$( )` from inline args
strip_dollar_paren = \(x) {
  lits = str_detect(x, '^\\$\\(.+\\)$')
  x[lits] = x[lits]|> str_sub(3, -2) #str_remove_all('^\\$\\(|\\)$')
  x
}
#' Remove double quotes from a string, if its' fully surrounded by them
strip_outer_quotes = \(x) {
  with_quotes = str_detect(x, '^".*"$')
  x[with_quotes] = str_sub(x[with_quotes], 2, -2)
  x
}
#' Remove comments from a chunk of code
#' @param x a line-split section of stan code
#' @returns x with all line and block comments removed.
strip_comments = \(x) {
  # x is a split chunk
  x |> collapse_lines() |>
    # Remove block comments first
    str_remove_all('\\/\\*(.|\\n)+?\\*\\/') |>
    # Then line comments
    str_replace_all(regex('//.*(\n|$)', multiline = TRUE), fixed('\n')) |>
    split_line()
}

#' Convert $(1) to $((1)) if it's a passed argument, to avoid accidentially replacing it in the wrong spot
#' @param x argument vector
#' @returns protected argument vector
protect_passed_args = \(x) {
  unprotected_patterns = str_extract_all(x, '\\$\\([0-9]+\\)') |> unlist()
  protected_patterns = str_replace(unprotected_patterns, fixed("$("), fixed("$((")) |>
    str_replace(fixed(')'), fixed('))'))
  str_replace_fixed_multi(x, unprotected_patterns, protected_patterns)
}
#' Reverses `protect_passed_args()` after splicing
#' @param x an atomic vector of a macro call with spliced args
unprotect_passed_args = \(x) {
  # browser()
  protected_patterns = str_extract_all(x, '\\$\\(\\([0-9]+\\)\\)')[[1]]
  unprotected_patterns = str_replace(protected_patterns, fixed("$(("), fixed("$(")) |>
    str_replace(fixed('))'), fixed(')'))
  str_replace_fixed_multi(x, protected_patterns, unprotected_patterns)
}

