

#' Parsing Logfile
#'
#' @param file character, path to the logfile to be parsed.
#' @param con either `NULL` (just returns a data.frame) or
#'        an object of class `SQLiteConnection`. In this case
#'        the data are not returned but stored in the SQLite database.
#' @param n positive numeric, number of lines to parse in one batch.
#' @param type either `NULL` (auto-detect) or one of `"error"` (when parsing
#'        error logfiles) or `"access"` (if parsing access logs). If set
#'        `NULL` it will be derived from the file name (if the file name
#'        contains `"error"` or `"access"`) or stops.
#'
#' @importFrom RSQLite dbWriteTable
parse_file <- function(file, con = NULL, n = 10L, type = NULL, ...) {

    stopifnot(
        "Can't find file" = isTRUE(file.exists(file)),
        "con must be NULL or an object of class 'SQLiteConnection'" =
            is.null(con) || inherits(con, "SQLiteConnection"),
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
        tmp$type <- type

        # Write to database
        if (!is.null(con)) {
            # Adding messages 
            query <- "INSERT OR IGNORE INTO messages (message) VALUES (?)"
            dbExecute(con, "BEGIN TRANSACTION")
            for (m in unique(tmp$message)) {
              dbExecute(con, query, params = list(m))
            }
            dbExecute(con, "COMMIT")

            # Getting all messages
            msgs <- dbGetQuery(con, "SELECT * FROM messages")
            tmp <- merge(msgs, tmp, by = "message")

            # Logs
            query <- "INSERT INTO logs (message_id, ip, timestamp, code, size, type)
                      VALUES (?, ?, ?, ?, ?, ?)"
            dbExecute(con, "BEGIN TRANSACTION")

            for (i in seq_len(nrow(tmp))) {
                y <- list(tmp$message_id[i],
                          tmp$ip[i],
                          tmp$timestamp[i],
                          tmp$code[i],
                          tmp$size[i],
                          substr(tmp$type[1], 0, 1))
                dbExecute(con, query, params = y)
            }
            dbExecute(con, "COMMIT")
        }

        if (counter >= maxbatches) {
            cat("Reached maximum number of batches to be read, exiting\n")
            break
        }
    }
    close(fid)
    invisible(nlines)
}

#' @importFrom stringr str_match
parse_logs <- function(x) {
    stopifnot(is.character(x) || length(x) > 0)

    x <- str_match(x, "^([0-9\\.]+)[\\s-]+(?!=\\[)(.*)(?<=\\])\\s(?!=\\\")(.*?)(?!=\\\")\\s([0-9-]+)\\s([0-9-]+)")
    if (any(is.na(x))) {
        cat(paste(line[which(is.na(x))], collapse = "\n"))
        stop('parsing issue')
    }
    x[grep("^-$", x[, 6]), 6] <- "0"
    res <- data.frame(ip = x[, 2],
                      timestamp =  as.numeric(as.POSIXct(x[, 3], format = "[%d/%b/%Y:%H:%M:%S %z]", tz = "UTC")),
                      message   = gsub("\\\"$", "", gsub("^\\\"", "", x[, 4])),
                      code      = as.integer(x[, 5]), # Causes warnings if == '-'
                      size      = as.integer(x[, 6]))

    # 'Stripping' messages
    res$message <- gsub("^GET\\s", "", res$message)
    res$message <- gsub("HTTP/1\\.1$", "", res$message)
    return(res)
}
