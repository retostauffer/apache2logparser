
#' Loading Logs From Database
#'
#' After the raw log files have been parsed an put into
#' a sqlite3 database via [parse_file()] this function
#' can be called to read the data from that database
#' and prepare the data for further analysis.
#'
#' @param con object of class `SQLiteConnection`
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
#' @return Returns a list of length two with two
#' data frames containing the access logs as well
#' as the error logs.
load_logs <- function(con, start = NULL, end = NULL, limit = NULL, quiet = FALSE) {

    if (!is.null(limit) & length(limit) > 0L) limit <- as.integer(limit[1L])
    if (!is.null(start) & length(start) > 0L) start <- as.POSIXct(start[1L])
    if (!is.null(end) & length(end) > 0L)     end <- as.POSIXct(end[1L])

    stopifnot(
        "con must be NULL or an object of class 'SQLiteConnection'" =
            is.null(con) || inherits(con, "SQLiteConnection"),
        "start must be NULL or evaluate to POSIXct" =
            is.null(start) || inherits(start, "POSIXct"),
        "end must be NULL or evaluate to POSIXct" =
            is.null(end) || inherits(end, "POSIXct"),
        "limit must be NULL or positive integer" =
            is.null(limit) || (is.integer(limit) & limit > 0L),
        "quiet must be logical TRUE or FALSE" = isTRUE(quiet) || isFALSE(quiet)
    )
    if (!is.null(start) && !is.null(end))
        stopifnot("if start and end are provided, end must be larger than start" =
                  end > start)

    # Loading all messages; required to merge to the logs later
    msg <- dbGetQuery(con, "SELECT * FROM messages")

    # Loading access logs
    get_query <- function(table, start, end, limit) {
        sql <- paste("SELECT * FROM", table)
        if (!is.null(start) || !is.null(end)) {
            tmp <- c("timestamp >=" = as.integer(start), "timestamp <" = as.integer(end))
            sql <- paste(sql, "WHERE", paste(paste(names(tmp), tmp), collapse = " AND "))
        }
        sql <- paste(sql, "ORDER BY timestamp")
        if (!is.null(limit)) sql <- paste(sql, "LIMIT", limit)
        return(sql)
    }

    # Getting data
    sql  <- get_query("access_logs", start, end, limit)
    alog <- dbGetQuery(con, sql)
    alog <- merge(alog, msg, by = "message_id", all.x = TRUE, all.y = FALSE)
    rm(sql)

    sql  <- get_query("error_logs", start, end, limit)
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


