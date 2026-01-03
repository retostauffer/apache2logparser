
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
#' @return Returns an object of class `apachelogs`, a list of length three with
#' three data frames containing basic statistics (`stats`) showing the total number
#' of requests per IP per day as well as the detailed `access` and `error` logs.
#' This object can be handed over to the [analyze_logs()] function to retrieve
#' the log statistics.
#'
#' @examples
#' \dontrun{
#' ## This is a NON-WORKING example just to have a template
#' con  <- open_database("mydatabase.sqlite3")
#'
#' ## Loading summary statistics, total log counts per IP.
#' ## Can be used to identify bad actors to limit the logs
#' ## analyzed later (see load_logs() function, argument 'ips').
#' stats <- load_stats(con)
#' stats <- load_stats(con, start = "2026-01-03", end = "2026-01-04")
#'
#' ## Loading logs (all logs; can be very demanding depending on the DB)
#' logs <- load_logs(con)
#'
#' ## Loading logs for a specific day
#' logs <- load_logs(con, start = "2026-01-03", end = "2026-01-04")
#'
#' ## Loading logs for specific IP addresses
#' logs <- load_logs(con, ips = c("127.0.0.1", "192.168.0.1"))
#'
#' ## Limit number of logs (mainly for testing)
#' logs <- load_logs(con, limit = 1000)
#'
#' ## Visualize the worst n = 6 IPs in terms of number of access.
#' plot(logs) # Uses n = 6 by default
#' }
#'
#' @export
#' @author Reto
#'
#' @importFrom stats aggregate setNames
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

    # Basic stats
    astat <- setNames(aggregate(timestamp ~ ip + as.days(timestamp),
                                FUN = length, data = alog),
                      c("ip", "date", "access_count"))
    estat <- setNames(aggregate(timestamp ~ ip + as.days(timestamp),
                                FUN = length, data = elog),
                      c("ip", "date", "error_count"))
    stats <- merge(astat, estat, by = c("ip", "date"), all = TRUE)
    stats[is.na(stats)] <- 0
    rm(astat, estat)

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

    res <- list(stats = stats, access = alog, error = elog)
    class(res) <- "apachelogs"
    return(res)
}

# ===================================================================

#' @export
#' @author Reto
#'
#' @importFrom hms hms
#' @rdname load_logs
load_stats <- function(con, start = NULL, end = NULL) {
    if (!is.null(start) & length(start) > 0L) start <- as.POSIXct(start[1L])
    if (!is.null(end) & length(end) > 0L)     end <- as.POSIXct(end[1L])

    stopifnot(
        "con must be NULL or an object of class 'SQLiteConnection'" =
            is.null(con) || inherits(con, "SQLiteConnection"),
        "start must be NULL or evaluate to POSIXct" =
            is.null(start) || inherits(start, "POSIXct"),
        "end must be NULL or evaluate to POSIXct" =
            is.null(end) || inherits(end, "POSIXct")
    )

    # Checking start and end parameters
    if (!is.null(start) && !is.null(end))
        stopifnot("if start and end are provided, end must be larger than start" =
                  end > start)

    # Helper function to grate the query
    get_query <- function(table, start, end) {
        sql <- sprintf("SELECT count(ip) AS %1$s_count, ip FROM %1$s_logs", table)
        where <- list()
        if (!is.null(start))
            where <- c(where, paste("timestamp >=", as.integer(start)))
        if (!is.null(end))
            where <- c(where, paste("timestamp <", as.integer(end)))
        if (length(where) > 0)
            sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
        sql <- paste(sql, "GROUP BY ip")
        print(sql)
        return(sql)
    }

    sql <- get_query("access", start, end)
    astat <- dbGetQuery(con, sql)

    sql <- get_query("error", start, end)
    estat <- dbGetQuery(con, sql)

    stats <- merge(astat, estat, by = "ip", all = TRUE)
    stats[is.na(stats)] <- 0
    stats$total <- stats$access_count + stats$error_count
    stats <- stats[order(stats$total, decreasing = TRUE), ]

    return(stats)
}

# ===================================================================

#'
#' @exportS3Method plot apachelogs
#'
#' @importFrom grDevices hcl.colors
#' @importFrom graphics barplot par text
#' @importFrom stats aggregate setNames xtabs
plot.apachelogs <- function(x, n = 6, sqrt = TRUE, ...) {
    n <- as.integer(n)[1L]
    stopifnot(
        "n must be positive integer" = is.integer(n) & length(n) == 1L & n > 0,
        "sqrt must be logical TRUE or FALSE" = isTRUE(sqrt) || isFALSE(sqrt)
    )
    x <- x$stats

    # If the number of different IPs is <= n, no subsetting is needed.
    if (length(unique(x$ip)) > n) {
        # Find the worst 'n' IPs
        tmp <- aggregate(cbind(access_count, error_count) ~ ip,
                         data = x, FUN = sum)
        tmp$total <- tmp$access_count + tmp$error_count
        limit   <- sort(tmp$total, decreasing = TRUE)[n]
        tmp     <- subset(tmp, total >= limit)
        x       <- x[x$ip %in% tmp$ip, ]
    }

    # Aggregating the data again for plotting
    atab <- xtabs(access_count ~ ip + date, data = x)
    etab <- xtabs(error_count ~ ip + date, data = x)
    if (sqrt) { atab <- sqrt(atab); etab <- sqrt(etab) }


    hold <- par(no.readonly = TRUE); on.exit(par(hold))

    par(mfrow = c(1, 2))
    bp <- barplot(atab, beside = TRUE, main = "Daily access logs",
                  ylab = if (sqrt) "sqrt counts" else "counts",
                  col = hcl.colors(nrow(atab), "Greens"))
    for (i in seq_len(ncol(bp)))
        text(bp[, i], max(atab) * 0.95, rownames(atab), adj = 1, srt = 90)

    bp <- barplot(etab, beside = TRUE, main = "Daily error logs",
                  ylab = if (sqrt) "sqrt counts" else "counts",
                  col = hcl.colors(nrow(etab), "Reds"))
    for (i in seq_len(ncol(bp)))
        text(bp[, i], max(etab) * 0.95, rownames(etab), adj = 1, srt = 90)

    invisible(x)
}

# ===================================================================

#' Reduce Timestamp to Days
#'
#' Small utility function which converts a timestamp/POSIXt to
#' day only, accounting for time zone if set.
#'
#' @param x object which can be converted to Date.
#'
#' @return Returns an object of same length as argument `x` reduced
#' to day only.
#'
#' @author Reto
as.days <- function(x) as.Date(x, tz = attr(x, "tz"))

# ===================================================================

#' Analyzing Apache Logs
#'
#' @param x object of class apachelogs as returned by the function
#'        [load_logs()].
#' @param unit character, defines on which temporal level the data
#'        are aggregated.
#'
#' @export
#' @author Reto
#'
#' @importFrom stats aggregate setNames
analyze_logs <- function(x, unit = c("hours", "days", "minutes", "seconds")) {

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
        timefun <- as.days
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



