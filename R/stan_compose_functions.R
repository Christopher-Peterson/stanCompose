suppressPackageStartupMessages({
  library(purrr)
  library(stringr)
})

# This is a wrapper script that calls everything else in order; eventually, it will be phased out in preference of a packaged version

# Environments to save the chunk text/name in ####
macro_env = rlang::new_environment()
scaffold_env = rlang::new_environment()

source('util.R', chdir = TRUE)
source('text_strip.R', chdir = TRUE)
source('macro_functions.R', chdir = TRUE)
source('knitr_functions.R', chdir = TRUE)
