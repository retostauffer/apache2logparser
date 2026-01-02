
#' Parsing Apache2 Logfile
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
#'   FILE_A <- "my_access_log"
#'   FILE_E <- "my_error_log"
#'   con    <- open_database("test.sqlite3")
#'   nlines <- parse_file(FILE_A, con = con, n = 1e5, verbose = TRUE)
#'   nlines <- parse_file(FILE_E, con = con, n = 1e5, verbose = TRUE)
#'   msg    <- dbGetQuery(con, "SELECT * FROM messages")
#'   alog   <- dbGetQuery(con, "SELECT * FROM access_logs")
#'   elog   <- dbGetQuery(con, "SELECT * FROM error_logs")
#'   dbDisconnect(con)
#' }
#'
#' @export
#' @author Reto
#'
#' @importFrom RSQLite dbWriteTable dbBind dbSendStatement dbClearResult dbGetQuery
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
    fid     <- file(file, "r") # open file connection
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
        tmp <- parse_logs(raw, type = type, warn = warn)
        if (is.null(tmp)) {
            if (verbose) cat(" .. all lines had incorrect format, continue\n")
            next
        }

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
            msgs  <- dbGetQuery(con, "SELECT * FROM messages")
            tmp0 <- tmp
            tmp   <- merge(msgs, tmp, by = "message")

            k <- msgs$message[duplicated(msgs$message)]
            if (length(k) > 0) {
                k <- table(subset(msgs, message %in% k)$message)
                print(k)
            }
            if (nrow(tmp) > nrow(tmp0)) browser()

            # Logs
            vars  <- names(tmp)[!names(tmp) == "message"]
            query <- sprintf("INSERT INTO %s_logs (%s) VALUES (%s)",
                             type, paste(vars, collapse = ", "),
                             paste(paste0(":", vars), collapse = ", "))

            dbExecute(con, "BEGIN TRANSACTION")

            if (verbose) cat(" and write them to DB\n")
            stmt <- dbSendStatement(con, query)
            dbBind(stmt, tmp[vars])
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
#' @importFrom stats line na.omit
parse_logs <- function(x, type, warn = TRUE) {
    stopifnot(is.character(x) || length(x) > 0)

    type <- match.arg(tolower(type), c("error", "access"))

    pattern <- if (type == "access") {
        "^([0-9\\.]+)[\\s-]+(?!=\\[)(.*)(?<=\\])\\s(?!=\\\")(.*?)(?!=\\\")\\s([0-9-]+)\\s([0-9-]+)"
    } else {
        # GPT
        "\\[(.*?)\\] \\[(.*?)\\] \\[pid ([0-9]+):tid ([0-9]+)\\] \\[client ([0-9.]+):([0-9]+)\\] (.*?): (.*)"

    }
    x <- as.data.frame(str_match(x, pattern))
    if (all(is.na(x))) {
        stop("All lines evaluated to `NA` (unexpected format of logs)")
    } else if (any(is.na(x))) {
        cat(paste(line[which(is.na(x))], collapse = "\n"))
        stop('parsing issue')
    }

    # Rows with missing values
    narows <- which(rowSums(is.na(x)) > 0)

    if (type == "access") {
        x[grep("^-$", x[, 6]), 6] <- "0"
        res <- data.frame(ip = x[, 2],
                          timestamp     =  as.numeric(as.POSIXct(x[, 3], format = "[%d/%b/%Y:%H:%M:%S %z]", tz = "UTC")),
                          message       = gsub("\\\"$", "", gsub("^\\\"", "", x[, 4])),
                          code          = as.integer(x[, 5]), # Causes warnings if == '-'
                          size          = as.integer(x[, 6]))
    } else {
        x <- setNames(x[, -1], c("date", "type", "process_id", "thread_id",
                                 "ip", "client_port", "message", "url"))
        res <- data.frame(ip            = x$ip,
                          timestamp     = as.numeric(as.POSIXct(x$date, format = "%a %b %d %H:%M:%OS %Y", locale = "UTC", locale = "C")),
                          message       = x$url,
                          error_message = paste(x$type, x$message, sep = " -- "),
                          process_id    = x$process_id,
                          thread_id     = x$thread_id,
                          client_port   = x$client_port)

    }


    if (length(narows) > 0) res <- res[-narows, ]
    if (length(narows) > 0 & warn)
        warning("Dropped ", length(narows), " lines (not parsable; incorrect format)")
    if (nrow(res) == 0) return(NULL)

    # 'Stripping' messages
    res$message <- gsub("^GET\\s", "", res$message)
    res$message <- gsub("HTTP/1\\.1$", "", res$message)
    return(res)
}
