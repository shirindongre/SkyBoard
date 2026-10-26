# SkyBoard R visual 2 - What actually drives satisfaction? (adjusted logistic regression)
# DAX can show raw gaps between happy and unhappy passengers; only a model can separate the
# effect of one service item from the others and from class, trip purpose, loyalty, age,
# distance and delay at the same time.
# Fields needed: FactSurvey[SurveyID], Satisfied, the 14 rating columns, TravelClass, TravelType,
#                CustomerType, Age, FlightDistance, ArrivalDelay
library(ggplot2)
panel <- "#25282B"; ink <- "#ECEFEC"; mute <- "#A9B1AC"; amber <- "#5FD08F"; cyan <- "#4CC38A"; grey <- "#6F7873"

rate_lab <- c(Wifi = "Inflight wifi", TimeConvenience = "Departure/arrival time",
  OnlineBooking = "Online booking", GateLocation = "Gate location", FoodDrink = "Food & drink",
  OnlineBoarding = "Online boarding", SeatComfort = "Seat comfort", Entertainment = "Entertainment",
  OnboardService = "On-board service", LegRoom = "Leg room", BaggageHandling = "Baggage handling",
  Checkin = "Check-in", InflightService = "Inflight service", Cleanliness = "Cleanliness")

blank_msg <- function(msg) {
  op <- par(bg = panel, mar = c(0, 0, 0, 0)); on.exit(par(op))
  plot.new(); text(0.5, 0.5, msg, col = ink, cex = 1.05)
}

d <- dataset
rate_cols <- intersect(names(rate_lab), names(d))
num_ctl   <- intersect(c("Age", "FlightDistance", "ArrivalDelay"), names(d))
cat_ctl   <- intersect(c("TravelClass", "TravelType", "CustomerType"), names(d))
for (v in cat_ctl) d[[v]] <- factor(d[[v]])
cat_ctl <- cat_ctl[sapply(cat_ctl, function(v) nlevels(droplevels(d[[v]])) > 1)]   # drop single-level controls
for (v in num_ctl) d[[v]] <- as.numeric(scale(d[[v]]))
d <- d[complete.cases(d[, c("Satisfied", rate_cols, num_ctl, cat_ctl)]), ]
d <- d[, c("Satisfied", rate_cols, num_ctl, cat_ctl)]

if (nrow(d) < 200 || length(unique(d$Satisfied)) < 2) {
  blank_msg("Needs a mix of satisfied and unhappy passengers\n(try clearing a slicer)")
} else {
  fit <- tryCatch(suppressWarnings(glm(reformulate(setdiff(names(d), "Satisfied"), "Satisfied"),
                                       data = d, family = binomial())), error = function(e) NULL)
  if (is.null(fit)) {
    blank_msg("Model could not be fitted for this selection")
  } else {
    co <- coef(fit); ci <- suppressMessages(confint.default(fit))
    keep <- intersect(rate_cols, names(co))
    res <- data.frame(item = unname(rate_lab[keep]), OR = exp(co[keep]),
                      lo = exp(ci[keep, 1]), hi = exp(ci[keep, 2]), stringsAsFactors = FALSE)
    res$sig  <- res$lo > 1 | res$hi < 1
    res$item <- factor(res$item, levels = res$item[order(res$OR)])
    # AUC by rank (Mann-Whitney)
    pr <- fitted(fit); r <- rank(pr); n1 <- sum(d$Satisfied == 1); n0 <- sum(d$Satisfied == 0)
    auc <- (sum(r[d$Satisfied == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
    top <- as.character(res$item[which.max(res$OR)])
    p <- ggplot(res, aes(OR, item, colour = sig)) +
      geom_vline(xintercept = 1, colour = grey, linetype = "dashed") +
      geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.45, linewidth = 0.7) +
      scale_colour_manual(values = c(`TRUE` = cyan, `FALSE` = grey), guide = "none") +
      scale_x_continuous(trans = "log2", breaks = c(0.5, 0.75, 1, 1.5, 2, 3),
                         labels = function(x) paste0(x, "x")) +
      labs(title = "What really drives satisfaction?",
           subtitle = sprintf("Odds multiplier per +1 rating point, adjusted for class, trip, loyalty, age, distance, delay\n%s passengers - in-sample AUC %.2f - strongest: %s",
                              format(nrow(d), big.mark = ","), auc, top),
           x = "Odds of being satisfied (log scale; right of 1x helps, left hurts)", y = NULL) +
      theme_minimal(base_size = 9) +
      theme(plot.background = element_rect(fill = panel, colour = NA),
            panel.background = element_rect(fill = panel, colour = NA),
            panel.grid.major = element_line(colour = "#343A3E", linewidth = 0.3),
            panel.grid.minor = element_blank(),
            plot.title = element_text(colour = amber, face = "bold", size = 12),
            plot.subtitle = element_text(colour = mute, size = 7.5, lineheight = 1.1),
            axis.text = element_text(colour = ink, size = 8),
            axis.title.x = element_text(colour = mute, size = 7.5),
            plot.margin = margin(6, 10, 4, 6))
    print(p)
  }
}
