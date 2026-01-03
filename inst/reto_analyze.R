#!/usr/bin/env Rscript
library("apache2logparser")
library("tinyplot")

# Connecting to database
con   <- open_database("DOS.sqlite3")
stats <- load_stats(con)
stats <- subset(stats, total >= sort(stats$total, decreasing = TRUE)[30])

# Loading logs for the selected 'bad IPs'
logs <- load_logs(con, ips = stats$ip)

#agg <- analyze_logs(logs, unit = "minutes")
agg <- analyze_logs(logs, unit = "seconds")

plt(access_count ~ timestamp | ip, type = "l", data = agg, main = "Access counts")
plt(error_count ~ timestamp | ip,  type = "l", data = agg, main = "Error counts")

# Disconnecting from database
dbDisconnect(con)

