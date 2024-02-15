
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The Stan-Compose framework

This is not yet a real R package, but eventually it will be. For now,
source `R/stan_compose_functions.R` at the top of your RMD file, and
include `write_stan_files()` after you’ve defined everything.

## Introduction

Stan-Compose allows for the definition of several similar Stan models
with slight variants, all within a single R Markdown document. Any
element that is common between more than one variant will be written as
a separate chunk in the document; these are effectively text macros.

Terminology: - *Stan chunk*: A markdown chunk of stan code. Chunks can
be Scaffolds or Macros. - *Scaffold*: A Stan chunk that will be rendered
into an independent Stan file. Macros can be called within scaffolds,
but scaffolds cannot be called by other chunks. - *Macro*: A Stan chunk
that will *not* be compiled to an independent Stan file. Macros can be
called by scaffolds, other macros, or as arguments. - *Literal*: Stan
code does not call any arguments or macros.  
- *Argument*: extra options passed to a macro when it is called.
Arguments can be either Macros or Literals. - *Macro/Argument call*:
Text within a Stan chunk that triggers the splicing of an argument or
the parsing and splicing of a macro. - *Parsing*: The conversion of a
macro into a literal. - *Splicing*: The process by which literals
replace a Macro or Argument call.

## Defining macros

The components of the stan model are written in named Stan chunks.
Chunks can be macros (re-usable components) or scaffolds (which are the
outermost layer and correspond to a model variant). Scaffolds should
have the chunk option `scaffold = TRUE`; otherwise, they are assumed to
be macros. Both scaffolds and macros need chunk labels (names); names
allow macros to be called and define the output filename of scaffolds.

All completed stan files will be written out to a newly-created diretory
based on the stanCompose rmd file’s name.

### Chunk Options

Required: - `label` (the first argument, can be unnamed); this is the
name of the macro or the filename of the scaffold.

Optional: - `scaffold`: Set to `TRUE` if the chunk is a scaffold
(otherwise the chunk is a macro) - `removecomments`: Set to `FALSE` to
*not* remove comments from the chunk during parsing. - `args`: This
doesn’t do anything (yet), but is a useful way to make macro arguments
visible when the chunk is collapsed.

## Calling Macros

Previously defined macros can be called inside a chunk in one of two
ways: - Whole-line macros: A line beginning with `@macro_name` (leading
whitespace is fine) is replaced with the macro; and subsequent text on
the line will be removed. - Inline macros: Macro is spliced at
`@(macro_name)`, without removing the rest of the line.

Macros can be called within other macros.

## Arguments

Macros can have positional arguments. Macros called with arguments have
a colon after the name. Arguments are space-separated. Arguments can be
literals (plain text) or other macros.

### Using Arguments in Macross

Literal arguments (and parsed macro arguments) are referenced by
position within a chunk: - Whole-line arguments: A line beginning with
`$1` is replaced with the first argument. - Inline arguments: The first
argument is spliced at `$(1)` (second with `$(2)`, etc); the rest of the
line isn’t removed.

To insert all arguments in the order called, use `$@`; This can only be
called as a line macro, and will place a newline between each. To
dynamically combine a set of arguments, use the `@(c: arg1 arg2 arg3)`
macro

#### Argument checks during macro calling:

If `$@` is called, check that at least one argument has been defined.

Otherwise, identify all whole-line arguments (`"^\\w*$[0-9]+$"`) and
inline arguments (figure out regex), extract the numbers from them, and
make sure that A) nothing is missing and B) the number of positions
matches the number of called arguments.

### Calling a Macro with Arguments

Calling a macro with arguments: - Basic literals:
`macro_name: literal1 literal2` (whole line) or
`@(macro_name: literal1 literal2)` (inline) - Complex literals:
`macro_name: literal1 "multi-word literal" literal3` - Basic Macro arg:
`macro_name: literal1 @macro_1 "multi-word literal" @macro_2` - Nested
Macro arg:
`macro_name: literal1 @(sub_macro: nested_lit_arg @(more_nested_macro: "doubly nested arg" ) final_literal)` -
Passing an arg to a nested macro arg:
`macro_name: @(sub_macro: arg1 $(5) )`

Macro arguments are parsed into literals, then treated like literal
arguments.

Whole-line macros will have trailing comments removed prior to checking
for arguments; all other text after the colon will be interpreted as an
argument.  
Literal arguments should not include line comments, as they will be
removed during this process and that will likely break things.

## Comments

By default, the comments of a chunk will be removed; this is to reduce
the chance of inline arguments / macros triggering when commented out.
Block comments (`/* */`) are removed first, followed by Line comments
(`//`).  
This behavior can be disabled with the chunk option
`remove_comments = FALSE`.

<!-- ## TODO List: -->
<!-- 1. ERROR: Including two @() calls on the same line doesn't seem to work.  Test case is the arguments calling @(fit_ode_common_args); if it's on the same line as @(sim_fit_ode_common_args), it doesn't get replaced. -->
<!-- 2. Make a rendering target for stan_compose documents. -->
<!-- 3. Add tests for the rendering/engine functions. -->
<!-- 4. Make into a package. -->
