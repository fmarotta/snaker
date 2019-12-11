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
snaker <- function(doc, args = commandArgs(TRUE), name = NULL, help = TRUE,
                   version = NULL, strict = FALSE, strip_names = !strict,
                   quoted_args = !strict) {
    if (exists("snakemake") && class(snakemake) == "Snakemake") {
        argv <- list()
        for (s in c("input", "output", "params", "wildcards", "log", "resources"))
           argv <- append(argv, methods::slot(snakemake, s)[names(methods::slot(snakemake, s)) != ""])
    } else {
        argv <- docopt::docopt(doc, args, name, help, version, strict,
                               strip_names, quoted_args)
    }
    argv
}
