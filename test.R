

library("devtools")
f <- function(...) { document(); load_all() }

FILE <- "/home/retos/Documents/2025-11-05_jss_varfull/var_log_moved_20251105/test_error_log"

FILE <- "/home/retos/Documents/2025-11-05_jss_varfull/var_log_moved_20251105/000-www.jstatsoft.org_access_log"

library("apache2logparser")

f(); parse_file(FILE, 10, maxbatches = 1)

