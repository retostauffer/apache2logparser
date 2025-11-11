

#' Parsing Logfile
#'
#' @param file character, path to the logfile to be parsed.
#' @param n positive numeric, number of lines to parse in one batch.
#' @param type either `NULL` (auto-detect) or one of `"error"` (when parsing
#'        error logfiles) or `"access"` (if parsing access logs). If set
#'        `NULL` it will be derived from the file name (if the file name
#'        contains `"error"` or `"access"`) or stops.
parse_file <- function(file, n = 10L, type = NULL, ...) {

    stopifnot(
        "Can't find file" = isTRUE(file.exists(file)),
        "type must be NULL or one of 'error'/'access'" = 
            is.null(type) || type %in% c("error", "access"),
        "argument 'n' must numeric" = is.numeric(n) && length(n) >= 1L
    )
    # Evaluating 'n'
    if (length(n) > 1L) warning("Only first element of 'n' is used.")
    n <- as.integer(n)[[1L]]
    stopifnot("argument 'n' must be > 0L" = n > 0)

    # If type is NULL try to derive it from the file name
    if (is.null(type)) {
        check <- tolower(unique(regmatches(file, gregexpr("(access|error)", file,
                                ignore.case = TRUE))[[1]]))
        if (length(check) != 1L)
            stop("Cannot auto-detect 'type' from file name")
        type <- check
    }

    # Maxbatch
    args <- list(...)
    print(args)
    maxbatches <- if (!is.null(args[["maxbatches"]])) as.integer(args[["maxbatches"]]) else Inf

    # Parsing the file
    fid     <- file(FILE, "r") # open file connection
    counter <- 0
    nlines  <- 0
    repeat {
        counter <- counter + 1
        raw     <- readLines(fid, n = n)
        nlines  <- nlines + length(raw)
        if (length(raw) == 0L) break
        cat("Read n =", length(raw), "lines from file\n")

        # Parsing the data
        tmp <- parse_logs(raw)

        if (counter >= maxbatches) {
            cat("Reached maximum number of batches to be read, exiting\n")
            break
        }
    }
    cat("Read/parsed", nlines, "number of lines.\n")
    close(fid)
}

#' @importFrom stringr str_match
parse_logs <- function(x) {
    stopifnot(is.character(x) || length(x) > 0)
    print(x)

    x <- str_match(x, "^([0-9\\.]+)[\\s-]+(?!=\\[)(.*)(?<=\\])\\s(?!=\\\")(.*?)(?!=\\\")\\s([0-9-]+)\\s([0-9-]+)")
    if (any(is.na(x))) {
        print(line)
        stop('parsing issue')
    }
    res <- data.frame(ip = x[, 2],
                      time =  as.POSIXct(x[, 3], format = "[%d/%b/%Y:%H:%M:%S %z]", tz = "UTC"),
                      msg  = gsub("\\\"$", "", gsub("^\\\"", "", x[, 4])),
                      code = as.integer(x[, 5]), # Causes warnings if == '-'
                      something = as.integer(x[, 6]))
    print(res)
    stop('---dev---')
}
