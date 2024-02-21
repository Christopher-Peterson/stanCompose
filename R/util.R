## Utility functions ####
# Swap name/value of a character vector
swap_name_val = \(x) {
  y = names(x)
  if(is.null(y)) y = rep('', length(x))
  y |> setNames(unname(x))
}
# Throw an error if any error cases are present
error_with_vec = \(error_cases, prefix = 'Error with these: ', ..., .name = NA) {
  if(!is.na(.name)) prefix = paste0("Chunk ", .name, "; ", prefix)
  if(length(error_cases) > 0) {
    msg = paste(prefix, ..., paste0(error_cases, collapse = ','))
    stop(msg, call. = FALSE)
  }
}
# These work for a single set of lines
collapse_lines = \(x) paste0(x , collapse = '\n')
split_lines = \(x) strsplit(x, '\n', fixed = TRUE)
split_line = \(x) split_lines(x)[[1]]
str_extract_recursive = \(x, perl_pattern) regmatches(x, m = gregexpr(perl_pattern, x, perl = TRUE))
str_replace_fixed_multi = \(x, patterns, replacements) {
  if(length(patterns) != length(replacements)) stop('patterns and replacements must be same length')
  if(length(patterns) > 0) {
    names(replacements) = patterns
    x = str_replace_all(x, fixed(replacements))
  }
  x
}
#' str_replace_fixed_multi(), but with an ordering that allows control over when replacements happen
#' @param x text vector to replace in
#' @param patterns vector of fixed patterns to detect & replace
#' @param replacements vector to replace `patterns` with
#' @param ordered_masks vector of regex patterns that controls the order
#' `replacements` will be replaced; `replacements` that don't match any patterns
#' will be replaced, followed by those that don't match `ordered_masks[-1]`,
#' then `ordered_masks[-(1:2)]`, etc.
#' @return `x` with all patterns replaced in order.
ordered_str_replace_multi = \(x, patterns, replacements, ordered_masks = character(0)) {
  # Runs str_replace_fixed_multi, but first replaces everything
  # in which the replacement isn't protect_pat
  # if protec_pat is a vector, it runs recursively, protecting the last item of the vector the most
  if(length(ordered_masks) == 0) return(str_replace_fixed_multi(x, patterns, replacements))
  combine_mask = paste(ordered_masks, collapse = '|')
  # anything that doesn't match the combined mask is replaced now
  replace_now = str_detect(replacements, combine_mask, negate = TRUE)
  # Repeat this w/o the first ordered mask until they're all used up
  x |> str_replace_fixed_multi(patterns[replace_now], replacements[replace_now]) |>
    ordered_str_replace_multi(patterns[!replace_now], replacements[!replace_now], ordered_masks[-1])
}

