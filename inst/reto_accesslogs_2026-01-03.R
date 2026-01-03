#!/usr/bin/env Rscript
library("apache2logparser")

dir <- "/home/retos/Documents/2025-11-05_jss_varfull/logs_2026-01-03"
files <- grep("_access_log", list.files(dir), value = TRUE)
files <- file.path(dir, files)
stopifnot(length(files) > 0L)

# Create random file name for sqlite3 database.
DBFILE <- "logs_2026-01-03.sqlite3"

# Create database if it does not yet exist, else we assume we have
# already processed the logs and filled the database. That step
# does take quite some time given we process > 7 million logs.
filldb <- !file.exists(DBFILE)

# Connecting to database
con <- open_database(DBFILE)

# -------------------------------------------------------------------
# Now we process all logfiles if required
# -------------------------------------------------------------------
if (filldb) {
    tmp <- lapply(files, parse_file, con = con, n = 10000, verbose = TRUE)
}

# -------------------------------------------------------------------
# Analyzing data
# -------------------------------------------------------------------
stats <- load_stats(con)
print(head(stats))


# Closing database connection
dbDisconnect(con)
