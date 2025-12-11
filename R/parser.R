

#' Parsing Logfile
#'
#' @param file character, path to the logfile to be parsed.
#' @param con either `NULL` (just returns a data.frame) or
#'        an object of class `SQLiteConnection`. In this case
#'        the data are not returned but stored in the SQLite database.
#' @param n positive numeric, number of lines to parse in one batch
#'        from the logfile and written to the database.
#' @param maxbatches Maximum number of batches of `n` rows to be read.
#' @param type either `NULL` (auto-detect) or one of `"error"` (when parsing
#'        error logfiles) or `"access"` (if parsing access logs). If set
#'        `NULL` it will be derived from the file name (if the file name
#'        contains `"error"` or `"access"`) or stops.
#' @param verbose If set `TRUE`, some output will be printed.
#' @param warn Logical, defaults to `TRUE`. Can be suppressed by setting
#'        it `FALSE`.
#'
#' @return Invisibly returns the number of parsed lines.
#'
#' @examples
#' \dontrun{
#'   FILE   <- "my_access_log"
#'   con    <- open_database("foo.sqlite3")
#'   nlines <- parse_file(FILE, con = con, n = 1e5, verbose = TRUE)
#'   msg    <- dbGetQuery(con, "SELECT * FROM messages")
#'   log    <- dbGetQuery(con, "SELECT * FROM logs")
#'   dbDisconnect(con)
#' }
#'
#' @importFrom RSQLite dbWriteTable dbBind dbClearResult
parse_file <- function(file, con = NULL, n = 10L, type = NULL, verbose = FALSE, maxbatches = .Machine$integer.max, warn = TRUE) { 

    verbose <- as.logical(verbose)[1L]
    warn    <- as.logical(warn)[1L]
    if (length(maxbatches) > 1)
        warning("argument 'maxbatches' is of length > 1, only the first element will be used")
    maxbatches <- as.integer(maxbatches)[1L]

    stopifnot(
        "Can't find file" = isTRUE(file.exists(file)),
        "con must be NULL or an object of class 'SQLiteConnection'" =
            is.null(con) || inherits(con, "SQLiteConnection"),
        "type must be NULL or one of 'error'/'access'" = 
            is.null(type) || type %in% c("error", "access"),
        "argument 'n' must numeric" = is.numeric(n) && length(n) >= 1L,
        "argument 'verbose' must evaluate to TRUE or FALSE" = isTRUE(verbose) || isFALSE(verbose),
        "argument 'maxbatches' must evaluate to integer" = is.integer(maxbatches) && length(maxbatches) == 1L,
        "argument 'maxbatches' must be positive" = maxbatches > 0L,
        "argument 'warn' must evaluate to TRUE or FALSE" = isTRUE(warn) || isFALSE(warn)
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

    # Parsing the file
    fid     <- file(FILE, "r") # open file connection
    counter <- 0
    nlines  <- 0
    repeat {
        counter <- counter + 1
        raw     <- readLines(fid, n = n)
        nlines  <- nlines + length(raw)
        l1 <- nlines - length(raw) + 1
        if (length(raw) == 0L) break

        if (verbose) {
            fmt <- "Reading line %d-%d (n = %d) lines from file"
            cat(sprintf(fmt, l1, nlines, length(raw)))
        }

        # Parsing the data
        tmp <- parse_logs(raw, warn = warn)
        if (is.null(tmp)) {
            if (verbose) cat(" .. all lines had incorrect format, continue\n")
            next
        }
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
            tmp$type <- substr(tmp$type, 0, 1)

            # Logs
            query <- "INSERT INTO logs (message_id, ip, timestamp, code, size, type)
                      VALUES (:message_id, :ip, :timestamp, :code, :size, :type)"
            dbExecute(con, "BEGIN TRANSACTION")

            if (verbose) cat(" and write them to DB\n")
            stmt <- dbSendStatement(con, query)
            dbBind(stmt, tmp[c("message_id", "ip", "timestamp", "code", "size", "type")])
            dbClearResult(stmt)

            dbExecute(con, "COMMIT")
        }

        if (counter >= maxbatches) {
            if (warn) warning("Reached maximum number of lines to read before parsing the entire file! Consider increasing `n` and/or `maxbatches`.\n")
            break
        }
    }
    close(fid)
    invisible(nlines)
}

#' @importFrom stringr str_match
parse_logs <- function(x, warn = TRUE) {
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
    n <- nrow(res)
    res <- na.omit(res)
    if (nrow(res) != n & warn)
        warning("Dropped ", n - nrow(res), " lines (not parsable; incorrect format)")
    if (nrow(res) == 0) return(NULL)

    # 'Stripping' messages
    res$message <- gsub("^GET\\s", "", res$message)
    res$message <- gsub("HTTP/1\\.1$", "", res$message)
    return(res)
}
