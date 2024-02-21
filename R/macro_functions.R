## Macro parsing functions ####
#' @param x a character vector of macro calls, with arguments
extract_macro_args = \(x) {
  # nested_paren = "[@$](\\((?>[^()]|(?1))*\\))"  # Keeps track of nested parantheses, Only triggers on an opening $() or @()

  # This regex splits the macro call into each the macro name (the first element) and all of its arguments.
  # Arguments with $() and @() will be treated properly.
  split_regex = paste(
    "[@$](\\((?>[^()]|(?1))*\\))", # @() and $(), w/ nested parentheses tracking
    '".*?"', # quote, for multi-word literals,
    "[@]?[[:alpha:]][[:alnum:]_\\-.]*", # valid variable/macro names, optionally starting w/ an @
    "[0-9.]+", # pure numbers
    sep = '|')

  # nested_paren= "\\((?>[^()]|(?R))*\\)" # regex pattern; this doesn't work w/ stringr, needs perl
  str_extract_recursive(x, split_regex) |>
    map(strip_outer_quotes)
}

#' Inserts called macro arguments into its definition
#' @param macro_text the text of the macro definition; should be collapsed.
#' @param macro_args character vector of literal macro arguments
#' @param macro_name name of the macro being spliced (only used for error message)
#' @returns collapsed text of the macro with all args spliced into it
splice_macro_args = \(macro_text, macro_args, macro_name = '??') {
  # Within the text, identify $[0-9]+ and
  pat = list(
    all = '^\\s*\\$\\@.*?$', # ' $@
    num_line = '^\\s*\\$[0-9]+($|\\n|(\\s|//|/\\*).*?$)', # '$1', followed by whitespace / \n to EOL,
    num = '^\\s*\\$[0-9]+', # Just the number, w/o the whole line
    paren = '\\$\\([0-9]+\\)' # $(1)
  )
  # if(macro_name %in% c("generic_scaffold_template", 'c')) browser()

  # If any macro args are being passed, give them double parentheses to protect them
  macro_args = macro_args |> protect_passed_args()

  has_all = macro_text |> str_detect(regex(pat$all, multiline = TRUE))
  if(any(has_all)) {
    # macro_text |> str_extract(regex(pat$all, multiline = TRUE))
    macro_text =
      macro_text |> str_replace_all(regex(pat$all, multiline = TRUE),
                                    fixed(macro_args |> unlist() |> collapse_lines() ))
  }

  called_nums_il_txt = macro_text |> collapse_lines() |> str_extract_all(pat$paren) |>
    unlist()
  called_nums_il = called_nums_il_txt |> strip_dollar_paren() |> as.numeric()

  # called_nums_wl_pos = macro_text_split |> str_which(regex(pat$num, multiline = TRUE))
  called_nums_wl_txt = macro_text |> str_extract_all(regex(pat$num_line, multiline = TRUE)) |>
    unlist()
  called_nums_wl = called_nums_wl_txt |> str_extract(pat$num) |> unlist() |>
    str_remove('^\\s*\\$') |> as.numeric()
  # if(knitr::opts_current$get()$label == )
  called_nums = c(called_nums_wl, called_nums_il) |> na.omit() |> unique() |> sort()
  if(!any(has_all)) {
    # Don't run this check if $@ is called
    arg_nums = seq_along(macro_args)
    # used_arg_num = called_num[!is.na(called_num)] |> unique() |> sort()
    error_with_vec(arg_nums[!arg_nums %in% called_nums] ,
                   "Excess arguments provided to macro; positions:",
                   .name = macro_name)
    error_with_vec(called_nums[!called_nums %in% arg_nums],
                   "Arguments used in macro definition but not provided; positions:",
                   .name = macro_name)
  }
  arg_call_txt = c(called_nums_il_txt, called_nums_wl_txt)
  arg_val_txt = macro_args[c(called_nums_il, called_nums_wl)]
  # Identify args that contain $(n) in addition to other text; these will be
  # Passing args unless the macro they were passed to had literally nothing else in it
  #### I THINK this part of the code is superceded?
  # args_with_passing = str_detect(arg_val_txt, '\\$\\([0-9]+\\)') &
  #   str_detect(arg_val_txt, '^\\$\\([0-9]+\\)$', negate = TRUE)
  # # Replace the passed arguments with their values
  # arg_val_txt[args_with_passing] = arg_val_txt[args_with_passing] |>
  #   str_replace_fixed_multi(arg_call_txt, arg_val_txt)
  #### End superceded part?
  # Replace everything
  macro_text |>  ordered_str_replace_multi(
    arg_call_txt, arg_val_txt,
    ordered_masks = '^\\$\\([0-9]+\\)$') |> # replace $(n) args after others to replacing pass-through params
    ## Un-protect $(n)
    unprotect_passed_args()
}

#' Parses the arguments of a macro
#' @param split_arg_lines a list of argument lines, split by `extract_macro_args()`
#' @param macro_list a named list of previously defined macros; all args in split_arg lines should be in the list
#' @param chunk_name name of the chunk; for error reporting purposes
#' @returns a vector of collapsed macro text, with args integrated into it
parse_macro_args = \(split_arg_lines, macro_list = list(),  chunk_name = '??') {
  names = map_chr(split_arg_lines, 1) # first arg is always macro name
  # if(any(names %in% c('forward_scaffold_template', 'sim_ode_adult_forward'))) browser()
  # browser()
  args = map(split_arg_lines, \(x_raw) {
    # browser()
    # Recurse over nested macro args, evaluating them until they're out of arguments
    x = x_raw[-1] # drops the name
    is_macro = str_sub(x, 1, 1) == '@'
    # if(any(is_macro)) browser()
    if(any(is_macro)) x[is_macro] = parse_macro_line(x[is_macro], macro_list, chunk_name) |> unlist()
    x
  })
  macros_to_splice = macro_list[names] |> unlist()
  # if(any(names %in% "generic_scaffold_template")) browser()
  spliced_macro_txt = list(macro_text = macros_to_splice,
                           macro_args = args,
                           macro_name = names) |>
    pmap_chr(splice_macro_args)
  # browser()
  spliced_macro_txt # return
}

#' Parses macro calls within a stan file
#' Can also parse a nested macro, as the syntax is the same
#' @param macro_lines A vector of macro calls, as read from the stan file
#' @param macro_list list of previously defined macros; all macros called must be previously defined
#' @param chunk_name, used for error reporting
#' @returns a named list of macro texts, with arguments parsed
parse_macro_line = \(macro_lines, macro_list, chunk_name = '??') {
  # if(macro_lines == "@(c: $(3) @adjoint_shorten_ode_ctrl)") browser()
  split_lines =  macro_lines |> strip_macro_at() |> extract_macro_args()
  macro_names = map_chr(split_lines, 1)
  error_with_vec(error_cases = macro_names[!macro_names %in% names(macro_list)],
                 prefix = 'macro(s) called without being defined: ',
                 .name = chunk_name)

  out_macros = macro_list[macro_names]

  if(any(lengths(out_macros) > 1)) stop(
    "Some macros called in ",chunk_name," haven't been collapsed to ",
    "length 1. This will break things.", call. = FALSE)

  # Get an index of macros that have arguments
  pats = list(
    has_args = '^\\@\\(?[[:alpha:]][[:alnum:]_\\-]*\\:', ## colon after macro name
    wl_args = '^(\\s)*\\$(\\@|[0-9]+)', # An argument being called; run with regex(multiline = TRUE)
    il_args = '\\$\\([0-9]+\\)'
  )
  # if(any(macro_names %in% c('forward_scaffold_template', 'sim_ode_adult_forward'))) browser()
  # If a macro is defined with arguments, make sure they're called (and vice versa)
  # This doesn't check that the args line up (that's done later)
  args_called = macro_lines |> str_detect(pats$has_args)
  wl_args_needed = out_macros |> str_detect(pattern = regex(pats$wl_args, multiline = TRUE))
  il_args_needed = out_macros |> str_detect(pattern = pats$il_args)
  args_needed = wl_args_needed | il_args_needed
  error_with_vec(macro_names[args_called & (!args_needed)],
                 "Arguments provided to macros that don't need them: ", .name = chunk_name)
  error_with_vec(macro_names[(!args_called) & (args_needed)],
                 "Macro require arguments, but none provided; make sure there's a colon after the name: ", .name = chunk_name)
  # browser()
  # Now onto the parsing...
  out_macros[args_called] <- parse_macro_args(split_lines[args_called], macro_list, chunk_name)
  out_macros
}

#' Parses a
# All macros must have been defined before they're used.
#' @param x the text of the macro, split by line
#' @param chunk_name the name of the macro
#' @param remove_comments if `TRUE`, remove the comments from the macro
#' @param macro_envir environment in which previously defined macros have been saved
#' @return the fully parsed macro
parse_stan_macro = \(x, chunk_name, remove_comments = TRUE, macro_envir = macro_env) {
  # Detect macro calls
  pats = list(
    wl_macro_start = '^\\s*\\@[^(]',
    il_macro_start = paste0(
      '(^|\\n)(?!', '\\s*\\@[^(]', ')', # Negative lookahead; block wl-macros starting at beginning of line
      '.*?', #include whatever, lazily
      '\\@(\\((?>[^()]|(?2))*\\))' ),# The actual il macro ; note that the required recursion has changed to group 2
    il_leading_cruft = '(^|\\n).*?@', # everything up to the first @
    comments='(#|//).*($|\\n)'  )
  if(isTRUE(remove_comments)) x = strip_comments(x)
  macro_list = as.list(macro_envir)
  # if(chunk_name == 'forward_scaffold_adult') browser()
  x = collapse_lines(x)
  # Now detect inline macros (collapsing first lets them be split across lines
  il_macro_text = x |>
    str_extract_recursive(pats$il_macro_start) |> unlist() |>
    str_replace(pats$il_leading_cruft, '@')
  # This can accidentially pick up whole line macros that call nested macros with @()
  # Not ideal

  if(length(il_macro_text) > 0) {
    # browser()
    il_macro_parsed = parse_macro_line(il_macro_text, macro_list, chunk_name) |>
      unlist()
    x = x |> str_replace_fixed_multi(patterns = il_macro_text,
                                     replacements = il_macro_parsed)
  }
  x = x |> split_line()
  wl_macros = str_detect(x, pats$wl_macro_start)
  # if(chunk_name %in% c('adult_def_init', 'sim_ode_adult_adjoint')) browser()
  if(any(wl_macros)) {
    # browser()
    wl_macro_lines = x[wl_macros] |>
      str_remove('^\\s*') |> str_remove(pats$comments) |>  # In case strip_comments is false
      parse_macro_line(macro_list, chunk_name) |> unlist()
    # Splice them in; for whole-lines, that's just direct replacement
    x[wl_macros] = wl_macro_lines # Double Check This!!!
  }
  # Collapse the new text
  x |> collapse_lines()

}

