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
        snakemake <- get("snakemake", envir = .GlobalEnv)
    else
        snakemake <- NULL

    if (class(snakemake) == "Snakemake") {
        usage <- docopt:::printable_usage(doc, name)

        # parse options
        pot_options <- docopt:::parse_doc_options(doc)
        opt_names <- sapply(pot_options$options, function(o) {
            gsub("(^<)|(^\\-\\-?)|(>$)", "", o$name())
        })
        opt_names_mand <- opt_names[sapply(pot_options$options, function(o) {
            is.null(o$value)
        })]
        # We have to check two things: that all the mandatory options have a counterpart in the snakemake object,
        # and that all snakemake named arguments are valid options.
        options <- list()
        for (s in c("input", "output", "params", "wildcards", "log", "resources")) {
            snake_slot <- methods::slot(snakemake, s)
            slot_names <- names(snake_slot)[names(snake_slot) != ""]
            # if (!all(slot_names %in% opt_names))
                # stop(usage, call. = FALSE) # adding options that don't exist
            options <- append(options, snake_slot[slot_names])
        }
        if (!all(opt_names_mand %in% names(options)))
            stop(usage, call. = FALSE) # a mandatory option is missing

        # parse arguments (i.e. unnamed entries in snakemake)
        pattern <- docopt:::parse_pattern(docopt:::formal_usage(usage), pot_options)
        pot_arguments <- pattern$flat()[sapply(pattern$flat(), class) == "Argument"]
        arg_names <- unique(sapply(pot_arguments, function(a) {
            gsub("(^<)|(^\\-\\-?)|(>$)", "", a$name())
        }))
        arguments <- list()
        for (s in c("input", "output")) {
            snake_slot <- methods::slot(snakemake, s)
            slot_names <- names(snake_slot)[names(snake_slot) != ""]
            snake_slot[slot_names] <- NULL
            snake_slot[(length(snake_slot) - length(slot_names) + 1):length(snake_slot)] <- NULL
            arguments <- append(arguments, snake_slot)
        }
        if (length(arguments) != length(arg_names))
            stop(usage, call. = FALSE)
        names(arguments) <- arg_names

        # parse commands
        pot_commands <- pattern$flat()[sapply(pattern$flat(), class) == "Command"]
        cmd_names <- unique(sapply(pot_commands, function(c) {
            gsub("(^<)|(^\\-\\-?)|(>$)", "", c$name())
        }))
        snake_slot <- snakemake@params[names(snakemake@params) == ""]
        slot_names <- names(snakemake@params)[names(snakemake@params) != ""]
        commands <- snake_slot[1:(length(snake_slot) - length(slot_names))]
        if (length(commands) != length(cmd_names))
            stop(usage, call. = FALSE)
        names(commands) <- cmd_names

        argv <- c(options, arguments, commands)
    } else {
        argv <- docopt::docopt(doc, args = commandArgs(TRUE), name = name, help = help, version = version,
                               strict = FALSE, strip_names = TRUE, quoted_args = TRUE)
    }
    argv
}
