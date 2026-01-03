
#' Loading Logs From Database
#'
#' After the raw log files have been parsed an put into
#' a sqlite3 database via [parse_file()] this function
#' can be called to read the data from that database
#' and prepare the data for further analysis.
#'
#' @param con object of class `SQLiteConnection`
#' @param ips `NULL` (loads logs for all IPs) or a character
#'        vector with valid IPs to restrict the logs loaded
#'        to specific IP addresses.
#' @param start either `NULL` or POSIXt, defines the beginning
#'        of the logs to be returned if set.
#' @param end either `NULL` or POSIXt, defines the beginning
#'        of the logs to be returned if set.
#' @param limit `NULL` (default) or positive integer, will limit the
#'        number of logs (useful for testing).
#' @param quiet Logical, defaults to `FALSE`.
#'
#' @details The arguments `start` and `end` allow to limit the logs
#' loaded to a specific period. For convenience `start >=` and `end <`
#' is used, so one can retrieve logs for one specific day by supplying
#' e.g., `start = "2026-01-03"` and `end = "2026-01-04"` which will
#' include all logs for 2026-01-03.
#'
#' @return Returns an object of class `apachelogs`, a list of length two with
#' two data frames containing the access logs as well as the error logs.
#' This object can be handed over to the [analyze_logs()] function to retrieve
#' the log statistics.
#'
#' @export
#' @author Reto
load_logs <- function(con, ips = NULL, start = NULL, end = NULL, limit = NULL, quiet = FALSE) {

    if (!is.null(limit) & length(limit) > 0L) limit <- as.integer(limit[1L])
    if (!is.null(start) & length(start) > 0L) start <- as.POSIXct(start[1L])
    if (!is.null(end) & length(end) > 0L)     end <- as.POSIXct(end[1L])

    stopifnot(
        "con must be NULL or an object of class 'SQLiteConnection'" =
            is.null(con) || inherits(con, "SQLiteConnection"),
        "ips must be NULL or character vector of length > 0" =
            is.null(ips) || (is.character(ips) && length(ips) > 0L),
        "start must be NULL or evaluate to POSIXct" =
            is.null(start) || inherits(start, "POSIXct"),
        "end must be NULL or evaluate to POSIXct" =
            is.null(end) || inherits(end, "POSIXct"),
        "limit must be NULL or positive integer" =
            is.null(limit) || (is.integer(limit) & limit > 0L),
        "quiet must be logical TRUE or FALSE" = isTRUE(quiet) || isFALSE(quiet)
    )

    # Checking IP filter if set
    if (!is.null(ips)) {
        check <- grepl("^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$", ips)
        if (!all(check)) {
            error("The following are no valid IPs: ", paste(ips[!check], collapse = ", "))
        }
    }

    # Checking start and end parameters
    if (!is.null(start) && !is.null(end))
        stopifnot("if start and end are provided, end must be larger than start" =
                  end > start)

    # Loading all messages; required to merge to the logs later
    msg <- dbGetQuery(con, "SELECT * FROM messages")

    # Loading access logs
    get_query <- function(table, ips, start, end, limit) {
        sql <- paste("SELECT * FROM", table)
        where <- list()
        if (!is.null(start))
            where <- c(where, paste("timestamp >=", as.integer(start)))
        if (!is.null(end))
            where <- c(where, paste("timestamp <",  as.integer(end)))
        if (!is.null(ips))
            where <- c(where, sprintf("ip in (%s)", paste(sprintf("\"%s\"", ips), collapse = ", ")))
        if (length(where) > 0)
            sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
        sql <- paste(sql, "ORDER BY timestamp")
        if (!is.null(limit)) sql <- paste(sql, "LIMIT", limit)

        return(sql)
    }

    # Getting data
    sql  <- get_query("access_logs", ips, start, end, limit)
    alog <- dbGetQuery(con, sql)
    alog <- merge(alog, msg, by = "message_id", all.x = TRUE, all.y = FALSE)
    rm(sql)

    sql  <- get_query("error_logs", ips, start, end, limit)
    elog <- dbGetQuery(con, sql)
    elog <- merge(elog, msg, by = "message_id", all.x = TRUE, all.y = FALSE)
    rm(sql)

    # Quick message if quiet = FALSE
    if (!quiet) {
        fmt <- "%Y-%m-%d %H:%M:%S"
        message("Messages retrieved:")
        message(sprintf("    Access logs:   %10d    (%s  to  %s)", nrow(alog),
                        format(min(alog$timestamp), format = fmt),
                        format(max(alog$timestamp), format = fmt)))
        message(sprintf("    Error logs:    %10d    (%s  to  %s)", nrow(elog),
                        format(min(elog$timestamp), format = fmt),
                        format(max(elog$timestamp), format = fmt)))
    }

    res <- list(access = alog, error = elog)
    class(res) <- "apachelogs"
    return(res)
}


#' Analyzing Apache Logs
#'
#' @param x object of class apachelogs as returned by the function
#'        [load_logs()].
#' @param unit character, defines on which temporal level the data
#'        are aggregated.
#'
#' @export
#' @author Reto
analyze_logs <- function(x, unit = c("hours", "days", "minutes", "seconds"), ...) {

    unit <- match.arg(unit)
    stopifnot(
        "x must be an object of class 'apachelogs'" = inherits(x, "apachelogs")
    )

    # Just to play safe
    n <- nrow(x$access) + nrow(x$error)
    if (n == 0L) stop("Well, there are no logs in 'x' (empty).")

    # Defines the temporal aggregation function
    if (unit == "hours") {
        timefun <- function(x) as.POSIXct(ceiling(as.integer(x) / 3600) * 3600, tz = attr(x, "tz"))
    } else if (unit == "minutes") {
        timefun <- function(x) as.POSIXct(ceiling(as.integer(x) / 60) * 60, tz = attr(x, "tz"))
    } else if (unit == "days") {
        timefun <- as.Date
    } else {
        # Default; seconds
        timefun <- function(x) as.POSIXct(ceiling(as.integer(x)), tz = attr(x, "tz"))
    }

    # Aggregating number of logged calls per IP
    agg <- function(x, name) {
        x$timestamp <- timefun(x$timestamp)
        res <- aggregate(ip ~ as.character(ip) + timefun(timestamp),
                         data = x, FUN = length)
        return(setNames(res, c("ip", "timestamp", name)))
    }
    aagg <- agg(x$access, "access_count")
    eagg <- agg(x$error,  "error_count")

    return(merge(aagg, eagg, by = c("ip", "timestamp"), all = TRUE))
}



