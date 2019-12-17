#' snaker
#'
#' This function is equivalent to docopt::docopt, except that it can also handle
#' arguments passed by Snakemake. If the `snakemake' object exists, then the
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

    doc <- gsub(pattern = "\n[:blank:]{2,}", replacement = " ", x = doc)

    # If there exists the snakemake object and it is of the right class,
    # take the arguments from there.
    if (class(snakemake) == "Snakemake") {
        # Parse the commands
        snake_slot <- snakemake@params[names(snakemake@params) == ""]
        slot_names <- names(snakemake@params)[names(snakemake@params) != ""]
        commands <- snake_slot[seq_len(length(snake_slot) - length(slot_names))]

        options_fields <- c("input", "output", "params", "resources")
        args_fields <- c("input", "output")
        options <- list()
        arguments <- list()

        # Parse the log
        if (length(snakemake@log) > 1) {
            options_fields <- c(options_fields, "log")
            args_fields <- c(args_fields, "log")
        } else if (length(snakemake@log) == 1) {
            if (is.null(names(snakemake@log)))
                options <- append(options, paste("--log", snakemake@log, sep = "="))
            else
                options_fields < c(options_fields, "log")
        }

        # Parse the options
        for (s in c("input", "output", "params", "resources")) {
            snake_slot <- methods::slot(snakemake, s)
            slot_names <- names(snake_slot)[names(snake_slot) != ""]
            if (length(slot_names)) {
                dash <- ifelse(nchar(snake_slot[slot_names]) == 1, "-", "--")
                options <- append(options, paste(paste0(dash, slot_names), snake_slot[slot_names], sep = "="))
            }
        }

        # Parse the arguments
        for (s in args_fields) {
            snake_slot <- methods::slot(snakemake, s)
            slot_names <- names(snake_slot)[names(snake_slot) != ""]
            snake_slot[slot_names] <- NULL
            snake_slot[(length(snake_slot) - length(slot_names) + 1):(length(snake_slot) + 1)] <- NULL
            arguments <- append(arguments, snake_slot)
        }

        args <- c(unlist(commands), unlist(options), unlist(args))
    } else {
        args <- commandArgs(TRUE)
    }

    argv <- docopt::docopt(doc, args = args, name = name, help = help, version = version,
                           strict = FALSE, strip_names = TRUE, quoted_args = TRUE)

    argv
}
