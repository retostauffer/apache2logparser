

#' Open (or create) SQLite Database 
#'
#' @param dbfile path to the SQLite database.
#'
#' @return Returns SQLite database connection.
#'
#' @export
#' @author Reto
#'
#' @importFrom RSQLite dbConnect SQLite dbExecute dbCommit
open_database <- function(dbfile) {
    stopifnot(
        "argument 'dbfile' must be character" =
            is.character(dbfile) && length(dbfile) == 1L
    )

    exists <- isTRUE(file.exists(dbfile))

    con <- dbConnect(SQLite(), dbfile, extended_types = TRUE)

    # Newly created? Add tables
    if (!exists) {
        # Messages table
        sql <- "CREATE TABLE messages (
            message_id INTEGER PRIMARY KEY AUTOINCREMENT,
            message VARCHAR(200),
            UNIQUE(message)
        )
        "
        tryCatch(dbExecute(con, sql),
                 error = function(e) stop("Error creating messages DB table:", e))

        # access logs table
        sql <- "CREATE TABLE access_logs (
            message_id INTEGER NOT NULL,
            ip VARCHAR(15) NOT NULL,
            timestamp DATETIME NOT NULL,
            code INTEGER,
            size INTEGER,
            FOREIGN KEY(message_id) REFERENCES messages(message_id)
        )
        "
        tryCatch(dbExecute(con, sql),
                 error = function(e) stop("Error creating access_logs DB table:", e))
        # logs table
        sql <- "CREATE TABLE error_logs (
            message_id INTEGER NOT NULL,
            ip VARCHAR(15) NOT NULL,
            timestamp DATETIME NOT NULL,
            error_message VARCHAR(100),
            process_id INTEGER,
            thread_id INTEGER,
            client_port INTEGER,
            FOREIGN KEY(message_id) REFERENCES messages(message_id)
        )
        "
        tryCatch(dbExecute(con, sql),
                 error = function(e) stop("Error creating error_logs DB table:", e))
    }

    return(con)
}


