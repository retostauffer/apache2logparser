

library("devtools")
library("RSQLite")
f <- function(...) { document(); load_all() }

FILE <- "/home/retos/Documents/2025-11-05_jss_varfull/var_log_moved_20251105/test_error_log"

FILE <- "/home/retos/Documents/2025-11-05_jss_varfull/var_log_moved_20251105/000-www.jstatsoft.org_access_log"

f(); library("apache2logparser")

dbDisconnect(con)
if (file.exists("foo.sqlite3")) file.remove("foo.sqlite3")
f();
con <- open_database("foo.sqlite3")
f(); k <- parse_file(FILE, con = con, n = 10, maxbatches = 1)
dbGetQuery(con, "SELECT * FROM messages")
dbGetQuery(con, "SELECT * FROM logs")
#head(k)




dbDisconnect(con)

