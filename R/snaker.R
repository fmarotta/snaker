# TODO: unnamed snakemake options are arguments, names ones are Options.

#' snaker
#'
#' This function is equivalent to docopt::docopt, except that it can also handle
#' arguments passed by Snakemake. If the `Snakemake' object exists, then the
#' arguments are taken from there, otherwise from the command line.
#'
#' @inheritParams docopt::docopt
#'
#' @return The list of named arguments and parameters.
#' @export
snaker <- function(doc, name = NULL, help = TRUE, version = NULL) {
    if (exists("snakemake", where = .GlobalEnv, inherits = FALSE))
        snakemake = get("snakemake", envir = .GlobalEnv)
    else
        snakemake = NULL

    if (class(snakemake) == "Snakemake") {
        usage <- docopt:::printable_usage(doc, name)
        pot_options <- docopt:::parse_doc_options(doc)
        opt_names <- sapply(pot_options$options, function(o) {
            gsub("(^<)|(^\\-\\-?)|(>$)", "", o$name())
        })

        # We have to check two things: that all the options have a counterpart in the snakemake object,
        # and that all snakemake named arguments are valid options.
        argv <- list()
        for (s in c("input", "output", "params", "wildcards", "log", "resources")) {
            snake_slot <- methods::slot(snakemake, s)
            slot_names <- names(snake_slot)[names(snake_slot) != ""]
            if (!all(slot_names %in% opt_names))
                stop(usage, call. = FALSE)
            argv <- append(argv, snake_slot[slot_names])
        }
        if (!all(names(args) %in% opt_names))
            stop(usage, call. = FALSE)
    } else {
        argv <- docopt::docopt(doc, args = commandArgs(TRUE), name = name, help = help, version = version,
                               strict = FALSE, strip_names = TRUE, quoted_args = TRUE)
    }
    argv
}
