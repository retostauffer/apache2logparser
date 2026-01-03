#!/usr/bin/env Rscript
library("apache2logparser")
library("tinyplot")
library("devtools")
library("zoo")
f <- function(...) { document("../"); load_all("../") }

# Create random file name for sqlite3 database.

# Connecting to database
con <- open_database("DOS.sqlite3")


#start <- as.POSIXct("2025-11-02", tz = "UTC")
start <- as.POSIXct("2025-11-05", tz = "UTC")
end   <- start + 86400 * 5

f(); xxx <- load_logs(con, start = start, end = end, limit = 200, ips = c("173.15.14.29"))
f(); xxx <- load_logs(con, start = start, end = start + 86400 * 3)
f(); k <- plot(xxx, n = 10)

f(); s <- load_stats(con)
f(); s1 <- load_stats(con, start = start, end = start + 86400)
head(s1)




subset(xxx, access_count > 1000)
table(xxx$access$ip)
names(xxx); xxx$stats

f(); res <- load_logs(con, start = start, end = end)
f(); agg <- analyze_logs(res, unit = "hours")
summary(agg)

plt(access_count ~ timestamp | ip, type = "o",
    data = subset(agg, access_count > 50),
    main = "Access counts")
plt(error_count ~ timestamp | ip, type = "o",
    data = subset(agg, access_count > 10),
    main = "Error counts")

f(); aggm <- analyze_logs(res, unit = "minutes")
summary(aggm)

plt(access_count ~ timestamp | ip, type = "o",
    data = subset(aggm, access_count > 50),
    main = "Access counts")
plt(error_count ~ timestamp | ip, type = "o",
    data = subset(aggm, access_count > 10),
    main = "Error counts")



badman <- subset(aggm, ip == "173.15.14.29")
head(badman)
badman <- zoo(badman[, grepl("_count$", names(badman))], badman$timestamp)
plot(badman, screen = 1, col = c(3, 2))



# Disconnecting from database
dbDisconnect(con)

