# SkyBoard R visual 3 - Trend vs season in baggage complaints (STL decomposition)
# Separates the long-run trend from the repeating yearly pattern and the one-off noise.
# Pools the airlines currently selected: monthly rate = complaints / passengers x 1,000.
# Fields needed: FactBaggage[Airline], FactBaggage[MonthStart], [Baggage Complaints], [Passengers Enplaned]
library(ggplot2)
panel <- "#25282B"; ink <- "#ECEFEC"; mute <- "#A9B1AC"; amber <- "#5FD08F"; cyan <- "#4CC38A"; coral <- "#E0736A"; trend <- "#8FD6AE"

blank_msg <- function(msg) {
  op <- par(bg = panel, mar = c(0, 0, 0, 0)); on.exit(par(op))
  plot.new(); text(0.5, 0.5, msg, col = ink, cex = 1.05)
}

d <- dataset
m <- data.frame(month = as.Date(character()))                       # empty until proven otherwise
if (nrow(d) > 0) {
  d$month <- as.Date(format(as.Date(d$MonthStart), "%Y-%m-01"))
  m <- aggregate(cbind(c = d[["Baggage Complaints"]], p = d[["Passengers Enplaned"]]) ~ month, data = d, FUN = sum)
  m <- m[order(m$month), ]
  m$rate <- m$c / m$p * 1000
  # fill missing calendar months (e.g. after a filter) so the monthly time series stays aligned
  m <- merge(data.frame(month = seq(min(m$month), max(m$month), by = "month")), m, by = "month", all.x = TRUE)
  if (anyNA(m$rate)) m$rate <- approx(seq_len(nrow(m)), m$rate, seq_len(nrow(m)), rule = 2)$y
}

if (nrow(m) < 24) {   # also covers an empty selection
  blank_msg("Needs at least 24 months for a seasonal split\n(widen the year slicer)")
} else {
  y0 <- as.integer(format(m$month[1], "%Y")); mo0 <- as.integer(format(m$month[1], "%m"))
  ts_rate <- ts(m$rate, start = c(y0, mo0), frequency = 12)
  st <- stl(ts_rate, s.window = "periodic", robust = TRUE)
  comp <- as.data.frame(st$time.series)
  df <- rbind(
    data.frame(month = m$month, panel = "Observed rate  +  trend", value = m$rate,   kind = "obs"),
    data.frame(month = m$month, panel = "Observed rate  +  trend", value = comp$trend, kind = "trend"),
    data.frame(month = m$month, panel = "Seasonal effect (same every year)", value = comp$seasonal, kind = "seas"),
    data.frame(month = m$month, panel = "What is left over", value = comp$remainder, kind = "rem"))
  df$panel <- factor(df$panel, levels = c("Observed rate  +  trend", "Seasonal effect (same every year)", "What is left over"))
  seas_by_month <- tapply(comp$seasonal, as.integer(format(m$month, "%m")), mean)
  pk <- month.name[as.integer(names(which.max(seas_by_month)))]
  lo <- month.name[as.integer(names(which.min(seas_by_month)))]
  sw <- max(seas_by_month) - min(seas_by_month)

  p <- ggplot(df, aes(month, value)) +
    geom_hline(data = data.frame(panel = factor(c("Seasonal effect (same every year)", "What is left over"),
                                                levels = levels(df$panel)), y = 0),
               aes(yintercept = y), colour = "#3C4247", linewidth = 0.3) +
    geom_line(data = subset(df, kind == "obs"),   colour = "#6F7873", linewidth = 0.5) +
    geom_line(data = subset(df, kind == "trend"), colour = trend,     linewidth = 1.1) +
    geom_line(data = subset(df, kind == "seas"),  colour = cyan,      linewidth = 0.8) +
    geom_col(data = subset(df, kind == "rem"),    fill = coral, width = 20) +
    facet_wrap(~ panel, ncol = 1, scales = "free_y") +
    labs(title = "Trend vs season in baggage complaints",
         subtitle = sprintf("Seasonal swing of %.1f complaints per 1,000 passengers: worst in %s, best in %s",
                            sw, pk, lo), x = NULL, y = "Complaints per 1,000 passengers") +
    theme_minimal(base_size = 9) +
    theme(plot.background = element_rect(fill = panel, colour = NA),
          panel.background = element_rect(fill = panel, colour = NA),
          panel.grid.major = element_line(colour = "#343A3E", linewidth = 0.3),
          panel.grid.minor = element_blank(),
          strip.text = element_text(colour = ink, size = 8, face = "bold", hjust = 0),
          plot.title = element_text(colour = amber, face = "bold", size = 12),
          plot.subtitle = element_text(colour = mute, size = 8),
          axis.text = element_text(colour = ink, size = 7.5),
          axis.title.y = element_text(colour = mute, size = 7.5),
          plot.margin = margin(6, 10, 4, 6))
  print(p)
}
