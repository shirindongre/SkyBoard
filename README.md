# SkyBoard: flights dashboard (Power BI project)

An interactive Power BI dashboard built from your four datasets. Data preparation is done in R, the model and DAX are in the project, and three R visuals sit on top of the dashboard.

## 1. Open it (5 minutes)

1. Unzip so the folder is `D:\Shirin\Uni\Sem 5\DV Lab\SkyBoard_v2\`. That is the path the model expects.
2. In Power BI Desktop, open **Options > Preview features** and tick **Power BI Project (.pbip) save option**, **Store semantic model using TMDL format** and **Store reports using enhanced metadata format (PBIR)** if they are listed. Restart if asked. Recent versions have these on already.
3. Open `SkyBoard.pbip`.
4. Click **Home > Refresh**. The project ships without cached data, so every visual is empty until the first refresh.
5. If you unzipped somewhere else: **Home > Transform data > Edit parameters**, set **DataFolder** to the `data` folder, then Refresh.
6. R visuals: **Options > R scripting**, point to your R install, and run `install.packages("ggplot2")` once. Click **Enable** when Desktop asks about script visuals.
7. Check it, then **File > Save as** a `.pbix` if your submission needs one.

## 2. What is in it

| Page | What it shows |
|---|---|
| Home | Headline numbers, six navigation tiles, how to explore |
| Network Explorer | Map of 342 airports, top hubs, busiest routes, traffic by state. Region, state and Top-N slicers. Right-click an airport and choose Drill through > Airport Profile |
| Baggage Watch | Complaint rate, cancellations, seasonality, year-on-year change, airline ranking. A metric slicer changes what the trend chart plots. Hover for a tooltip card |
| Passenger Voice | Satisfaction by class, age, distance, delay and trip purpose, with gauge against the overall rate |
| Service Drivers | Average rating and the happy-versus-unhappy gap for 14 service factors, a heatmap, and a decomposition tree |
| R Lab | Three R visuals that respond to the slicers |
| Method & Data Prep | What was wrong with the raw files and how it was fixed |
| Airport Profile, Airline tooltip | Hidden helper pages (drill-through and tooltip) |

## 3. Rubric mapping

**Aesthetic (5).** One pastel theme (`SkyBoardPastel.json`, registered in the report), consistent grid, navigation bar on every page, an insight sentence under each page that updates with the slicers.

**Technical depth and insights (5).**
- Star schema with a marked date table and a role-playing airport dimension (origin active, destination inactive, switched on with `USERELATIONSHIP`).
- 91 DAX measures in 9 display folders (64 feed visuals directly or through other measures; 27 are spare analysis measures). Techniques: `SAMEPERIODLASTYEAR`, `TOTALYTD`, `DATESINPERIOD`, `RANKX`, `TOPN`, `ALLSELECTED`, `REMOVEFILTERS`, `SWITCH` with `SELECTEDVALUE` over disconnected tables, variables, and dynamic text.
- Rates are always ratios of totals, never averages of rates.
- Insights found:
  1. American Eagle has 10.5 complaints per 1,000 passengers, against 4.7 for United and 2.7 for Hawaiian.
  2. The complaint rate peaked in 2006 (7.4) and fell to 4.1 by 2010.
  3. The worst month of every year is December or January; the best is April or autumn (R3 isolates this seasonal pattern).
  4. Complaints and cancellations move together (r = 0.82 across airline-months).
  5. Business class is 70% satisfied, Economy 19%.
  6. Business trips are 59% satisfied, personal trips 10%.
  7. Longer flights are rated higher (35% under 500 miles, 77% over 3,000).
  8. A delay of an hour or more costs about 12 points, far less than class or trip purpose.
  9. Online boarding and inflight wifi have the strongest independent effect on satisfaction (R2).
  10. The ten busiest routes carry only 1.5% of flight records; Chicago, Atlanta, Denver and Dallas lead on traffic.

**Variety of graphs (5).** Map, bar, column, line, area, combo, ribbon, waterfall, scatter, treemap, donut, gauge, 100% stacked bar, stacked column, matrix heatmap, table, decomposition tree, cards, plus three R visuals.

**R integration (5).**
- `scripts/01_preprocess.R` is the data pipeline (cleaning, joining, imputing, validating). It writes the CSVs the dashboard loads and a report of every fix.
- `scripts/r_visuals/` holds three scripts that run inside Power BI: a correlation heatmap, a logistic regression of satisfaction drivers, and an STL trend/season split of baggage complaints. Each answers a question DAX cannot, and each reacts to the page slicers. Previews are in `docs/r_previews/`.

## 4. Re-run the data preparation

```
Rscript scripts/01_preprocess.R <folder with the four raw csv files> data
```
Needs R with `dplyr` and `tidyr`. The output matches the `data` folder exactly (checked byte for byte).

## 5. Read this before you submit

This project was generated and checked outside Power BI Desktop: every JSON file passes Microsoft's published schemas, every field a visual uses exists in the model, and the R scripts were run on your real data. I could not open it in Desktop, so please look through it once. The spots most likely to need a nudge:

- **Map.** It needs the "Map and filled map visuals" setting on. If it is blank, the other charts are unaffected.
- **Heatmap colours** on the Service Drivers matrix. If the cells are plain, use Format > Cell elements > Background colour > Gradient on *Avg Rating*.
- **Tile-style slicers** (Region, class, airline). If one shows as a vertical list, set Format > Slicer settings > Options > Orientation to Horizontal.
- **Drill-through and tooltip pages.** If the right-click option or hover card does not appear, check Page information on those two hidden pages.
- **DAX.** Measures were checked for syntax and references, but not evaluated by the engine. If one shows an error, the message names the measure.

Limits of the data: the on-time extract has no dates, the survey has no airline, and baggage covers three airlines. Findings are descriptive, not causal.
