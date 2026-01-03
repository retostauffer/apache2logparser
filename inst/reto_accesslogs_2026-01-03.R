#!/usr/bin/env Rscript
library("apache2logparser")
library("tinyplot")

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

# Loading blocked IPs
mfile <- "/home/retos/snap/thunderbird/common/.thunderbird/o8ihzxjc.default/Mail/Local Folders/__JSS-Firewall"
tmp <- readLines(mfile)
tmp <- tmp[grepl("^Blocked IP.*mod_evasive.*", tmp)]

bad_ip <- regmatches(tmp, regexpr("(?<=(IP\\s))[0-9\\.]{6,15}", tmp, perl = TRUE))
bad_ts <- regmatches(tmp, regexpr("(?<=(mod_evasive\\sat\\s)).*", tmp, perl = TRUE))
bad_ts <- as.POSIXct(bad_ts, format = "%a %b %d %H:%M:%S CET %Y")
if (any(is.na(bad_ts))) stop("Issued decoding 'when blocked' from email")

blocked <- data.frame(ip = bad_ip, blocked = bad_ts)
blocked <- subset(blocked, ip %in% res$ip)
blocked <- aggregate(blocked ~ ip, data = blocked, max)
rm(bad_ip, bad_ts)

cat("Number of blocked IPs found in logs:", nrow(blocked), "\n")

# Loading the detailed logs for these IPs
devtools::load_all("../")
logs <- load_logs(con, ips = blocked$ip)
names(logs)
plot(logs, n = 3, sqrt = FALSE)

mtab <- sort(table(logs$access$message_id), decreasing = TRUE)
kkk <- subset(logs$messages, message_id %in% as.integer(names(head(mtab, n = 20))))
kkk
proportions(table(grepl("wp-content", logs$access$message)))

# Calculating statistics for these bad actors
res <- analyze_logs(logs, unit = "15minutes")
head(res)
res <- merge(res, blocked, by = "ip")


when_blocked <- function(x) as.integer(aggregate(blocked ~ ip, data = x, max)$blocked)
pdf(file = "__bad_actors.pdf", width = 20, height = 50)
    tinytheme("clean")
    tinyplot(access_count ~ timestamp | ip, data = res,
        type = "o", cex = 0.4, pch = 19, facet = "by",
        facet.args = list(ncol = 5), lwd = 2, col = 1,
        main = "Bad actor access logs (15min)")
    tinyplot_add(type = type_hline(1000), lty = 2,
                 col = "limegreen", lwd = 1)
    tinyplot_add(type = type_vline(when_blocked(res)), lty = 2,
             col = "tomato", lty = 3, lwd = 2)
dev.off()


# -------------------------------------------------------------------
# Trying to inspect a few users in more detail
# -------------------------------------------------------------------

# ---- clearly a bot, scanning for vulnerabilities
TARGET <- "172.192.18.188"
k <- subset(res, ip == TARGET)
l <- subset(logs$access, ip == TARGET)
tinyplot(access_count ~ timestamp, data = k, typ = "o")
tinyplot_add(type = type_vline(as.integer(k$blocked[1])), col = 2, lwd = 3)
names(table(l$message))

tmp  <- load_logs(con, ips = TARGET)
tmp <- analyze_logs(tmp, unit = "minutes")
plot(access_count ~ timestamp, data = tmp, type = "o", col = 2, lwd = 3)

# ---- also a bot scanning
TARGET <- "159.65.201.86"
k <- subset(res, ip == TARGET)
l <- subset(logs$access, ip == TARGET)
tinyplot(access_count ~ timestamp, data = k, typ = "o")
tinyplot_add(type = type_vline(as.integer(k$blocked[1])), col = 2, lwd = 3)
names(table(l$message))

tmp  <- load_logs(con, ips = TARGET)
tmp <- analyze_logs(tmp, unit = "minutes")
plot(access_count ~ timestamp, data = tmp, type = "o", col = 2, lwd = 3)

# ---- legit URIs, but seems to scrape
TARGET <- "74.7.227.21"
k <- subset(res, ip == TARGET)
l <- subset(logs$access, ip == TARGET)
tinyplot(access_count ~ timestamp, data = k, typ = "o")
tinyplot_add(type = type_vline(as.integer(k$blocked[1])), col = 2, lwd = 3)
names(table(l$message))
tail(l, n = 10)
plot(sort(l$timestamp))


# ---- also legit URIs but scraping?
TARGET <- "74.7.243.249"
k <- subset(res, ip == TARGET)
l <- subset(logs$access, ip == TARGET)
tinyplot(access_count ~ timestamp, data = k, typ = "o")
tinyplot_add(type = type_vline(as.integer(k$blocked[1])), col = 2, lwd = 3)
names(table(l$message))
tail(l, n = 10)
plot(sort(l$timestamp))

# ---- also legit URIs but scraping?
TARGET <- "74.7.243.214"
k <- subset(res, ip == TARGET)
l <- subset(logs$access, ip == TARGET)
tinyplot(access_count ~ timestamp, data = k, typ = "o")
tinyplot_add(type = type_vline(as.integer(k$blocked[1])), col = 2, lwd = 3)
names(table(l$message))
tail(l, n = 10)
plot(sort(l$timestamp))

# ---- 
TARGET <- "216.73.216.91"
k <- subset(res, ip == TARGET)
l <- subset(logs$access, ip == TARGET)
l <- l[order(l$timestamp), ]
head(l, n = 100)
tinyplot(access_count ~ timestamp, data = k, typ = "o")
tinyplot_add(type = type_vline(as.integer(k$blocked[1])), col = 2, lwd = 3)
names(table(l$message))
tail(l, n = 10)
plot(sort(l$timestamp))


# -------------------------------------------------------------------
# Closing database connection
# -------------------------------------------------------------------
dbDisconnect(con)



