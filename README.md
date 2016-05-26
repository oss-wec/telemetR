# telemetR
*0.85.0*

A Shiny application for the exploratory analysis and visualization of animal movement.

## Justification

We have organized all GPS collar location data ( ~2 million records) into a single database yet all data output is limited to SQL statements and Microsoft Excel line limit (65,000 rows). The application also provides an GUI for exploratory data analysis and visualization of animal movement. Users can estimate basic home range and utilization distributions, and plots of standard movement parameters. After exploratory analysis is complete, data can be downloaded for further analysis.

## Requirements

Download and install the latest versions of R [(R version 3.2.x)](https://cran.r-project.org/bin/windows/base/) and RStudio [(RStudio Desktop 0.99.x)](https://www.rstudio.com/products/rstudio/download/).

Copy and paste these commands into the RStudio console. If this is the first time you've used R you'll be asked to choose a CRAN mirror. Use the *US (CA-1) https mirror.* This will install the required packages for the app.

```r
install.packages("leaflet")
install.packages("DT")
install.packages("data.table")
install.packages("shiny")
install.packages("shinyjs")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("dplyr")
install.packages("sp")
install.packages("geojsonio")
install.packages("adehabitatHR")
install.packages("gridExtra")
install.packages("fasttime")
install.packages("ggthemes")
install.packages("maptools")
```

I tried, for a long time to limit the number of packages required for the application. However, as more functionality is included in the application, more packages are required to do all the analyses. If the application errors on startup due to an uninstalled package, the R console will inform which packages needs to be installed. Install the package with the `install.packages('package name')` function call.

To run the app use the command below. For all future uses this is the only line you will need to run.
```r
shiny::runGitHub("telemetR", "ndow-wisr", launch.browser = TRUE)
# or
shiny::runGitHub("ndow-wisr/telemetR", launch.browser = TRUE)
```

## Demo & Instructions

[View a brief (older version) demo here.](https://drive.google.com/file/d/0B1OupsoLNZvkcExIT2VzcUlySWc/view?usp=sharing)

[Instructions for using the app](https://github.com/ndow-wisr/telemetR/wiki/Instructions)

## Known Bugs

Kernel Density estimation may error. This error is from the `adehabitatHR` package, I'm working on it.

## TODO
- [x] Change name of column headers
- [ ] Use 'start' and 'stop' markers for first and last points
- [x] Clear/Reset Map page input
- [ ] ~~Reset input on tab change... may not be possible?~~
- [x] Use data.table for all data munging
- [ ] Use Leaflet Draw to allow users to subset based on the area
- [ ] A way for users to request analysis based on data they've entered
- [ ] ~~Remove erroneous points, persistent to data download~~
  - [ ] ~~There is some questions about this step for future analysis~~
- [x] Include migration analysis plots
- [x] Include utilization distributions.
  - [ ] Download as shapefile?
- [x] NSD plots and associated maps

[Feature requests and report errors that occur while using the application.](https://github.com/ndow-wisr/telemetR/issues)
