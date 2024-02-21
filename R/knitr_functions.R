## The Knitr Parts ####
# This part will need revision when it's turned into a package.
# Eventual goal: some sort of setup_stan_compose function factory that is run on knit.
# It creates the macro/scaffold environments in it, creates the stan_macro_engine function and sets it
# and sets up the write_stan_files function as the output/rendering function.
# would be nice to also have some sort of metadata environment that processed defined metadata about the scaffolds
# And some way to set options in the yaml for rendering

# Define a replacement knitr engine for stan, which just returns a character vector of the input
stan_macro_engine = \(options) {
  # Options we care about:
  # Code (the code)
  # Label (the chunk name)
  # remove_comments
  if(isFALSE(options$eval)) return(invisible(''))
  if(purrr::is_null(options$label)) stop('All Stan chunks must have labels.', call. = FALSE)
  remove_comments = ifelse(isFALSE(options$remove_comments), FALSE, TRUE)

  parsed_code = parse_stan_macro(options$code, options$label, remove_comments, macro_env)
  # scaffold (boolean); if TRUE this is a scaffold, otherwise a macro
  env = (if(isTRUE(options$scaffold)) scaffold_env else  macro_env)
  # Read the code & assign it to the right environment
  assign(options$label, parsed_code, envir = env)

  # Add something here for metadata collection
  invisible('')
}
knitr::knit_engines$set(stan = stan_macro_engine)

# Write the stan files
write_stan_files = \(out_direc = knitr::current_input(dir = TRUE) |> stringr::str_replace("\\...?.?$", '_models')) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  # browser()
  scaffolds = as.list(scaffold_env) #|> imap(parse_stan_macros, macro_list = macros)
  files = file.path(out_direc, paste0(names(scaffolds), '.stan'))
  walk2(scaffolds, files, readr::write_lines)
  invisible(files)
}
