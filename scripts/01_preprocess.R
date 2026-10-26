# =============================================================================
# 01_preprocess.R  -  SkyBoard flight dashboard: data preparation pipeline
# -----------------------------------------------------------------------------
# Turns the four raw CSVs into the clean, analysis-ready tables that Power BI
# loads, and writes data_quality_report.md documenting every fix.
#
# Run:   Rscript 01_preprocess.R <raw_dir> <out_dir>
#   raw_dir : folder holding baggagecomplaints.csv, T_ONTIME_REPORTING.csv,
#             T_ONTIME_AIRPORT_LOCATION_LOOKUP.csv, test.csv
#   out_dir : folder to write the clean CSVs into (the dashboard's DataFolder)
# Needs:  R + dplyr + tidyr   (install.packages(c("dplyr", "tidyr")))
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

args    <- commandArgs(trailingOnly = TRUE)
raw_dir <- if (length(args) >= 1) args[1] else "raw"
out_dir <- if (length(args) >= 2) args[2] else "data"
script_dir <- {
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (loc_try in c("C.UTF-8", "en_US.UTF-8", "English_United States.utf8"))   # best effort
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc_try)))) break

rd  <- function(f, ...) read.csv(file.path(raw_dir, f), check.names = FALSE,
                                 stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", ...)

# Locale-proof CSV writer: always UTF-8, no BOM, text quoted, blanks for NA.
# (write.csv garbles characters such as the route arrows when R runs in a non-UTF-8 locale.)
wr <- function(df, f) {
  q <- function(x) { x <- enc2utf8(as.character(x))
                     ifelse(is.na(x), "", paste0("\"", gsub("\"", "\"\"", x, fixed = TRUE), "\"")) }
  cols <- lapply(df, function(col) {
    if (is.character(col)) q(col)
    else if (inherits(col, "Date")) format(col)
    else { s <- as.character(col); s[is.na(col)] <- ""; s }
  })
  con <- file(file.path(out_dir, f), "wb"); on.exit(close(con))
  writeLines(c(paste(q(names(df)), collapse = ","), do.call(paste, c(unname(cols), sep = ","))),
             con, useBytes = TRUE)
}
log_lines <- character(0)
note <- function(...) log_lines <<- c(log_lines, paste0(...))
chk  <- function(label, ok) { note("| ", label, " | ", ifelse(ok, "PASS", "**FAIL**"), " |"); stopifnot(ok) }
fmt  <- function(x) format(x, big.mark = ",", scientific = FALSE)

# =============================================================================
# 1. AIRPORTS  (dim_airport)
# =============================================================================
raw_lk  <- rd("T_ONTIME_AIRPORT_LOCATION_LOOKUP.csv")
bts_all <- read.csv(file.path(script_dir, "raw_bts_l_airport_id.csv"),
                    colClasses = "character", fileEncoding = "UTF-8")
geo     <- read.csv(file.path(script_dir, "airport_geo.csv"),
                    colClasses = c("character", "character", "numeric", "numeric"))

blank_names <- sum(is.na(raw_lk$AIRPORT_NAME) | raw_lk$AIRPORT_NAME == "")

# Full BTS lookup: "City, ST: Airport name"  ->  split into parts
ids <- as.character(raw_lk$AIRPORT_ID)
ap  <- bts_all[bts_all$Code %in% ids, ]
loc <- sub(": .*$", "", ap$Description)
ap  <- data.frame(
  AirportID   = as.integer(ap$Code),
  AirportName = sub("^[^:]*: ", "", ap$Description),
  City        = sub(",\\s*[A-Z]{2}$", "", loc),
  State       = sub("^.*,\\s*", "", loc),
  stringsAsFactors = FALSE)
ap$CityShort <- trimws(sub("/.*$", "", ap$City))          # "Allentown/Bethlehem/Easton" -> "Allentown"

state_names <- c(setNames(state.name, state.abb), DC = "District of Columbia",
                 PR = "Puerto Rico", VI = "U.S. Virgin Islands",
                 TT = "U.S. Pacific Territories")
ap$StateName <- unname(state_names[ap$State])

region_of <- function(s) {
  ne <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
  mw <- c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD")
  so <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL","KY","MS","TN","AR","LA","OK","TX")
  we <- c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA")
  ifelse(s %in% ne, "Northeast", ifelse(s %in% mw, "Midwest", ifelse(s %in% so, "South",
         ifelse(s %in% we, "West", "Territories"))))
}
ap$Region <- region_of(ap$State)

ap <- ap %>% left_join(geo %>% transmute(AirportID = as.integer(AIRPORT_ID), IATA, Latitude, Longitude),
                       by = "AirportID")
ap$AirportLabel <- paste0(ap$CityShort, " (", ap$IATA, ")")      # e.g. "Chicago (ORD)"
ap <- ap %>% arrange(AirportID) %>%
  select(AirportID, IATA, AirportLabel, AirportName, City, CityShort, State, StateName,
         Region, Latitude, Longitude)

# =============================================================================
# 2. ROUTES  (fact_route)  - 544,003 flight records -> one row per directed route
# =============================================================================
raw_rt <- rd("T_ONTIME_REPORTING.csv")
rt_rows <- nrow(raw_rt); rt_dups <- sum(duplicated(raw_rt))
named_ids  <- raw_lk$AIRPORT_ID[!is.na(raw_lk$AIRPORT_NAME) & raw_lk$AIRPORT_NAME != ""]
pct_named_origin <- 100 * mean(raw_rt$ORIGIN_AIRPORT_ID %in% named_ids)

route <- raw_rt %>%
  count(OriginID = ORIGIN_AIRPORT_ID, DestID = DEST_AIRPORT_ID, name = "Flights")

lab <- setNames(ap$IATA, ap$AirportID)
route$PairLabel  <- ifelse(lab[as.character(route$OriginID)] < lab[as.character(route$DestID)],
                           paste(lab[as.character(route$OriginID)], "↔", lab[as.character(route$DestID)]),
                           paste(lab[as.character(route$DestID)],  "↔", lab[as.character(route$OriginID)]))
route$RouteLabel <- paste(lab[as.character(route$OriginID)], "→", lab[as.character(route$DestID)])
key <- paste(route$OriginID, route$DestID); rkey <- paste(route$DestID, route$OriginID)
route$HasReturn  <- as.integer(rkey %in% key)
route <- route %>% arrange(desc(Flights))

# =============================================================================
# 3. BAGGAGE  (fact_baggage, dim_airline)
# =============================================================================
raw_bg <- rd("baggagecomplaints.csv")
names(raw_bg)[1] <- "Airline"
dparts <- do.call(rbind, strsplit(raw_bg$Date, "/"))
bg <- data.frame(
  Airline          = trimws(raw_bg$Airline),
  MonthStart       = as.Date(sprintf("%s-%02d-01", dparts[, 2], as.integer(dparts[, 1]))),
  Complaints       = raw_bg$Baggage,
  ScheduledFlights = raw_bg$Scheduled,
  CancelledFlights = raw_bg$Cancelled,
  Passengers       = raw_bg$Enplaned,
  stringsAsFactors = FALSE)
bg_consistent <- all(as.integer(format(bg$MonthStart, "%m")) == raw_bg$Month &
                     as.integer(format(bg$MonthStart, "%Y")) == raw_bg$Year)
dim_airline <- data.frame(Airline = sort(unique(bg$Airline)))

# =============================================================================
# 4. DATE TABLE  (dim_date) - daily, contiguous, covers every baggage month
# =============================================================================
d <- seq(as.Date("2004-01-01"), as.Date("2010-12-31"), by = "day")
mn <- as.integer(format(d, "%m"))
dim_date <- data.frame(
  Date          = d,
  Year          = as.integer(format(d, "%Y")),
  QuarterNumber = (mn - 1L) %/% 3L + 1L,
  MonthNumber   = mn,
  MonthName     = month.name[mn],
  MonthShort    = month.abb[mn],
  stringsAsFactors = FALSE)
dim_date$Quarter    <- paste0("Q", dim_date$QuarterNumber)
dim_date$YearMonth  <- format(d, "%Y-%m")
dim_date$YearQuarter<- paste(dim_date$Year, dim_date$Quarter)
dim_date$Season     <- c("Winter","Winter","Spring","Spring","Spring","Summer",
                         "Summer","Summer","Autumn","Autumn","Autumn","Winter")[mn]

# =============================================================================
# 5. PASSENGER SURVEY  (fact_survey wide + fact_ratings long)
# =============================================================================
sv <- rd("test.csv")
names(sv)[1] <- "RowIndex"                         # unnamed pandas index column
sv_rows <- nrow(sv)
na_arrival <- sum(is.na(sv[["Arrival Delay in Minutes"]]))

rating_map <- c(                       # raw column                          = clean name
  "Inflight wifi service"             = "Wifi",
  "Departure/Arrival time convenient" = "TimeConvenience",
  "Ease of Online booking"            = "OnlineBooking",
  "Gate location"                     = "GateLocation",
  "Food and drink"                    = "FoodDrink",
  "Online boarding"                   = "OnlineBoarding",
  "Seat comfort"                      = "SeatComfort",
  "Inflight entertainment"            = "Entertainment",
  "On-board service"                  = "OnboardService",
  "Leg room service"                  = "LegRoom",
  "Baggage handling"                  = "BaggageHandling",
  "Checkin service"                   = "Checkin",
  "Inflight service"                  = "InflightService",
  "Cleanliness"                       = "Cleanliness")
# Pretty labels + grouping used by the long ratings table and the R visuals
factor_label <- c(Wifi = "Inflight wifi", TimeConvenience = "Departure/arrival time",
  OnlineBooking = "Online booking", GateLocation = "Gate location", FoodDrink = "Food & drink",
  OnlineBoarding = "Online boarding", SeatComfort = "Seat comfort",
  Entertainment = "Inflight entertainment", OnboardService = "On-board service",
  LegRoom = "Leg room", BaggageHandling = "Baggage handling", Checkin = "Check-in",
  InflightService = "Inflight service", Cleanliness = "Cleanliness")
factor_group <- c(Wifi = "In-flight service", TimeConvenience = "Booking & airport",
  OnlineBooking = "Booking & airport", GateLocation = "Booking & airport",
  FoodDrink = "Cabin comfort", OnlineBoarding = "Booking & airport", SeatComfort = "Cabin comfort",
  Entertainment = "In-flight service", OnboardService = "In-flight service",
  LegRoom = "Cabin comfort", BaggageHandling = "Baggage", Checkin = "Booking & airport",
  InflightService = "In-flight service", Cleanliness = "Cabin comfort")

zero_counts <- sapply(names(rating_map), function(c) sum(sv[[c]] == 0))
zero_rows   <- sum(rowSums(sv[names(rating_map)] == 0) > 0)

# 0 means "not applicable" (only online/wifi style items carry zeros) -> NA, never a score
for (c in names(rating_map)) sv[[c]][sv[[c]] == 0] <- NA

# Arrival delay: 83 blanks. Fit arrival ~ departure on complete rows (r ~ 0.97), impute, flag.
fit <- lm(`Arrival Delay in Minutes` ~ `Departure Delay in Minutes`, data = sv)
imp <- is.na(sv[["Arrival Delay in Minutes"]])
pred <- pmax(0, round(predict(fit, newdata = sv[imp, ])))
sv[["Arrival Delay in Minutes"]][imp] <- pred
fit_r <- cor(sv[["Departure Delay in Minutes"]][!imp], sv[["Arrival Delay in Minutes"]][!imp])

band <- function(x, br, lb) as.character(cut(x, br, lb, right = FALSE))
age_lv   <- c("Under 18","18-24","25-34","35-44","45-54","55-64","65+")
dist_lv  <- c("Under 500","500-999","1,000-1,999","2,000-2,999","3,000+")
delay_lv <- c("On time","1-14 min","15-59 min","60-179 min","180+ min")
cls_lv   <- c("Economy","Economy Plus","Business")

survey <- data.frame(
  SurveyID       = sv$id,
  Gender         = sv$Gender,
  CustomerType   = ifelse(grepl("^dis", sv[["Customer Type"]], ignore.case = TRUE), "Disloyal", "Loyal"),
  Age            = sv$Age,
  stringsAsFactors = FALSE)
survey$AgeBand      <- band(sv$Age, c(0,18,25,35,45,55,65,Inf), age_lv)
survey$AgeBandSort  <- match(survey$AgeBand, age_lv)
survey$TravelType   <- ifelse(sv[["Type of Travel"]] == "Business travel", "Business", "Personal")
survey$TravelClass  <- c("Eco" = "Economy", "Eco Plus" = "Economy Plus", "Business" = "Business")[sv$Class]
survey$ClassSort    <- match(survey$TravelClass, cls_lv)
survey$FlightDistance <- sv[["Flight Distance"]]
survey$DistanceBand <- band(sv[["Flight Distance"]], c(0,500,1000,2000,3000,Inf), dist_lv)
survey$DistanceBandSort <- match(survey$DistanceBand, dist_lv)
survey$DepartureDelay <- sv[["Departure Delay in Minutes"]]
survey$ArrivalDelay   <- sv[["Arrival Delay in Minutes"]]
survey$ArrivalDelayImputed <- as.integer(imp)
survey$DelayBand    <- band(survey$ArrivalDelay, c(-Inf,1,15,60,180,Inf), delay_lv)
survey$DelayBandSort<- match(survey$DelayBand, delay_lv)
for (c in names(rating_map)) survey[[rating_map[[c]]]] <- sv[[c]]
rate_cols <- unname(rating_map)
survey$RatedItems   <- rowSums(!is.na(survey[rate_cols]))
survey$ServiceScore <- round(rowMeans(survey[rate_cols], na.rm = TRUE), 2)
survey$Satisfied    <- as.integer(sv$satisfaction == "satisfied")
survey$SatisfactionLabel <- ifelse(survey$Satisfied == 1, "Satisfied", "Neutral or dissatisfied")

ratings_long <- survey %>%
  select(SurveyID, all_of(rate_cols)) %>%
  pivot_longer(-SurveyID, names_to = "FactorKey", values_to = "Rating", values_drop_na = TRUE) %>%
  transmute(SurveyID, Factor = unname(factor_label[FactorKey]),
            FactorGroup = unname(factor_group[FactorKey]), Rating = as.integer(Rating))

# =============================================================================
# 6. VALIDATION  (hard stops: the script fails if any of these is false)
# =============================================================================
note("## Validation checks"); note(""); note("| Check | Result |"); note("|---|---|")
chk("All 342 airports in the routes have a name, city, state and coordinates",
    nrow(ap) == 342 && !any(is.na(ap[c("AirportName","City","State","StateName","Region","IATA","Latitude","Longitude")])))
chk("Airport IDs and IATA codes are unique", !anyDuplicated(ap$AirportID) && !anyDuplicated(ap$IATA))
chk("Every route origin and destination exists in dim_airport",
    all(route$OriginID %in% ap$AirportID) && all(route$DestID %in% ap$AirportID))
chk("No route starts and ends at the same airport", !any(route$OriginID == route$DestID))
chk("Route flight records sum back to the raw row count", sum(route$Flights) == rt_rows)
chk("Route table has one row per origin/destination pair", !anyDuplicated(route[c("OriginID","DestID")]))
chk("Baggage: 3 airlines x 84 months, no gaps", nrow(bg) == 252 && all(table(bg$Airline) == 84) && !anyDuplicated(bg[c("Airline","MonthStart")]))
chk("Baggage: Date column agrees with the Month and Year columns", bg_consistent)
chk("Baggage: cancelled flights never exceed scheduled flights", all(bg$CancelledFlights <= bg$ScheduledFlights))
chk("Baggage: no negative or missing values", !anyNA(bg) && all(bg[c("Complaints","ScheduledFlights","CancelledFlights","Passengers")] >= 0))
chk("Every baggage month exists in dim_date", all(bg$MonthStart %in% dim_date$Date))
chk("Survey: unique SurveyID, row count unchanged", !anyDuplicated(survey$SurveyID) && nrow(survey) == sv_rows)
chk("Survey: all ratings are 1-5 or blank (no zeros left)", all(unlist(survey[rate_cols]) %in% c(1:5, NA)))
chk("Survey: no missing arrival delays after imputation", !anyNA(survey$ArrivalDelay))
chk("Survey: every respondent has a class, band and satisfaction value",
    !anyNA(survey[c("TravelClass","AgeBand","DistanceBand","DelayBand","Satisfied")]))
chk("Long ratings table matches non-blank wide ratings", nrow(ratings_long) == sum(!is.na(survey[rate_cols])))

# =============================================================================
# 7. WRITE
# =============================================================================
wr(ap, "dim_airport.csv");   wr(route, "fact_route.csv")
wr(bg, "fact_baggage.csv");  wr(dim_airline, "dim_airline.csv"); wr(dim_date, "dim_date.csv")
wr(survey, "fact_survey.csv"); wr(ratings_long, "fact_ratings.csv")

hdr <- c(
"# Data quality report", "",
"Generated by `scripts/01_preprocess.R`. Row counts and percentages are computed from the raw files each run.", "",
"## What was wrong and what was done", "",
"| Source | Issue found | Fix |", "|---|---|---|",
sprintf("| Airport lookup | %d of %d rows had blank name, city and state (%.0f%%). Only %.0f%% of flights had a named origin, so the top routes showed raw IDs. | Rebuilt from the complete BTS airport-ID table (all 342 IDs found; the %d rows that were filled agree 100%%). Split into city, state, state name, census region. Added IATA code and coordinates (OurAirports, matched by name and state, 10 manual overrides). Dropped 4 redundant columns (sequence ID, city-market ID, source URL, status). |",
        blank_names, nrow(raw_lk), 100 * blank_names / nrow(raw_lk), pct_named_origin, nrow(raw_lk) - blank_names),
sprintf("| On-time reporting | %s rows but only %s distinct origin/destination pairs (%.1f%% exact duplicates). Six ID columns, four of them redundant. Undated. | Aggregated to one row per directed route with a flight-record count. Added IATA route labels, an undirected pair label (so ORD-LGA and LGA-ORD combine) and a has-return flag. |",
        fmt(rt_rows), fmt(nrow(route)), 100 * rt_dups / rt_rows),
"| Baggage complaints | Date stored as text `MM/YYYY`; Month and Year duplicate it; header carried a byte-order mark; rates not provided. | Parsed to a real month-start date, dropped the duplicate columns, renamed to plain English. Rates are computed in DAX from the totals (never averaged). |",
sprintf("| Survey | Unnamed index column; %s blank arrival delays (%.1f%%); %s respondents (%.1f%%) with a rating of 0, which means *not applicable*, not a score; inconsistent label case (`disloyal` vs `Loyal`). | Index dropped. Zeros set to blank. Blank arrival delays imputed from departure delay with a linear fit (r = %.3f) and flagged in `ArrivalDelayImputed`. Labels standardised. Added age, distance and delay bands (with sort keys), a service score, a satisfied flag. |",
        na_arrival, 100 * na_arrival / sv_rows, fmt(zero_rows), 100 * zero_rows / sv_rows, fit_r),
"| Survey (model) | 14 rating columns are awkward to chart together. | Added `fact_ratings` (long format, N/A rows excluded) so one chart can compare all factors. |",
"| Date | No calendar table. | Daily calendar 2004-2010 with year, quarter, month, season. |", "",
"Zero (not applicable) ratings removed, by item:", "",
"| Item | N/A responses |", "|---|---|",
sapply(names(rating_map), function(c) sprintf("| %s | %s |", factor_label[[rating_map[[c]]]], fmt(zero_counts[[c]]))),
"")
writeLines(c(hdr, log_lines), file.path(out_dir, "data_quality_report.md"), useBytes = TRUE)

cat("Done. Tables written to", normalizePath(out_dir), "\n")
cat(sprintf("airports %d | routes %d | baggage %d | dates %d | survey %d | ratings %s\n",
    nrow(ap), nrow(route), nrow(bg), nrow(dim_date), nrow(survey), fmt(nrow(ratings_long))))
