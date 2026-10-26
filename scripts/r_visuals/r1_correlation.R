# SkyBoard R visual 1 - What moves together?
# Correlation matrix of the 14 service ratings, arrival delay, distance, age and satisfaction.
# Reacts to every slicer on the page (class, travel type, loyalty, age band).
# Fields needed: FactSurvey[SurveyID] + the 14 rating columns + Satisfied + ArrivalDelay + FlightDistance + Age
library(ggplot2)
panel <- "#25282B"; ink <- "#ECEFEC"; mute <- "#A9B1AC"; amber <- "#5FD08F"

labels <- c(Wifi = "Inflight wifi", TimeConvenience = "Departure/arrival time",
  OnlineBooking = "Online booking", GateLocation = "Gate location", FoodDrink = "Food & drink",
  OnlineBoarding = "Online boarding", SeatComfort = "Seat comfort", Entertainment = "Entertainment",
  OnboardService = "On-board service", LegRoom = "Leg room", BaggageHandling = "Baggage handling",
  Checkin = "Check-in", InflightService = "Inflight service", Cleanliness = "Cleanliness",
  ArrivalDelay = "Arrival delay", FlightDistance = "Flight distance", Age = "Age",
  Satisfied = "\u2605 SATISFIED")

d <- dataset[, intersect(names(labels), names(dataset)), drop = FALSE]
d <- d[, sapply(d, function(x) sd(x, na.rm = TRUE) > 0), drop = FALSE]    # drop constant columns

if (nrow(d) < 30 || ncol(d) < 3) {
  par(bg = panel, mar = c(0, 0, 0, 0)); plot.new()
  text(0.5, 0.5, "Not enough respondents for this slicer selection", col = ink, cex = 1.1)
} else {
  cm  <- cor(d, use = "pairwise.complete.obs")
  ord <- hclust(as.dist(1 - cm), method = "average")$order
  cm  <- cm[ord, ord]
  nm  <- unname(labels[colnames(cm)])
  long <- expand.grid(x = seq_along(nm), y = seq_along(nm))
  long$r <- cm[cbind(long$x, long$y)]
  long$txt <- ifelse(long$x >= long$y, sprintf("%.2f", long$r), "")          # lower triangle only
  long$r_plot <- ifelse(long$x >= long$y, long$r, NA)

  p <- ggplot(long, aes(x, y, fill = r_plot)) +
    geom_tile(colour = panel, linewidth = 0.8) +
    geom_text(aes(label = txt, colour = abs(r) > 0.35), size = 2.5, show.legend = FALSE) +
    scale_colour_manual(values = c(`TRUE` = ink, `FALSE` = ink)) +
    scale_fill_gradient2(low = "#B85A52", mid = "#2F3438", high = "#2E8F5B", midpoint = 0,
                         limits = c(-1, 1), na.value = panel, name = "r") +
    scale_x_continuous(breaks = seq_along(nm), labels = nm, expand = c(0, 0)) +
    scale_y_continuous(breaks = seq_along(nm), labels = nm, expand = c(0, 0)) +
    labs(title = "What moves together?",
         subtitle = sprintf("Correlation across %s passengers - sorted so related items sit together",
                            format(nrow(dataset), big.mark = ","))) +
    theme_minimal(base_size = 9) +
    theme(plot.background  = element_rect(fill = panel, colour = NA),
          panel.background = element_rect(fill = panel, colour = NA),
          panel.grid       = element_blank(),
          plot.title       = element_text(colour = amber, face = "bold", size = 12),
          plot.subtitle    = element_text(colour = mute, size = 8),
          axis.title       = element_blank(),
          axis.text.x      = element_text(angle = 45, hjust = 1, size = 7.5, colour = ink),
          axis.text.y      = element_text(size = 7.5, colour = ink),
          legend.text      = element_text(colour = mute, size = 7),
          legend.title     = element_text(colour = mute, size = 8),
          plot.margin      = margin(6, 8, 4, 6))
  print(p)
}
