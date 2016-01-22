# Collar Data Export
*0.6.0*

A Shiny application to extract GPS collar data from NDOW's GPS collar data warehouse.

## Justification

We have organized all GPS collar location data ( >1.8 million records) into a single database yet all data output is limited to custom SQL statements and Microsoft Excel line limit (65,000 rows). This application provides a user interface for interactively and visually selecting GPS location data for export.

## Use Case

The application can be used anytime a subset of GPS collar data is needed for analysis. The collar manufacturer's website will be the most up to date data, if required use their proprietary applications.

### Brownian Bridge Movement Models

The location data for one Rocky Mountain bighorn sheep was exported to analyze the expected movement path using Brownian bridge movement models.

![bbmm export](https://e3a0a746a3d3c5fa95928f1d69a3e9079c622a5f.googledrive.com/host/0B1OupsoLNZvkYTdtVFRIelBoN00/bbmm_example.jpg)

### Quantifying Migration

The location data for on mule deer was exported to quantify migration by plotting the net squared displacement against time.

![bbmm export](https://e3a0a746a3d3c5fa95928f1d69a3e9079c622a5f.googledrive.com/host/0B1OupsoLNZvkYTdtVFRIelBoN00/nsd_example.jpg)

*check the Migration Tab for more information.*

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
install.packages("sp")
```

To run the app use the command below. For all future uses this is the only line you will need to run.
```r
shiny::runGitHub("CollarDataExport", "mgritts")
```

Permissions for the GIS data drive (V:/ActiveProjects/Game) is required to access the data for the application.

## Instructions

General Instructions for the use of the application. If you want a more general introduction to R use the [Quick R website](http://www.statmethods.net). For additional analysis or visualizations in R contact Mitch Gritts.

### Collared Animals Tab

The application opens with a table of animals collared with GPS collars. The table can be filtered using the *Species* and *Management Area* drop down lists. The *Management Area* drop down only filters the table for Mule Deer.

Any field in the table can be searched using the search bar on the right. The table can be sorted by clicking the arrows next to each column name.

### Map Tab

The second page is a map plotted with the animals from the table on the Collared Animals Page. The blue dots represent the first and last GPS location of the animal. The grey line is a smoothed path (every 20 locations) between the first and last point.

The map can be filtered by individual by entering individual NDOW IDs in the *NDOW ID* text box. The NDOW IDs used should be from the table on the Collared Animals Page. To enter more than one use a comma to separate NDOW IDs (ex: 831, 3669). To further filter the plot enter a *Date Range* and click the *Use Date Range* checkbox. This will only include points between the two dates.

Use the reset tab to reset the input to the original values. If changing species or management area on the collared animals page, the data on the Map page will need to be reset in order to view all the animals. The default date range is 2010-01-01 to today's  date.

### Data Tab

The third page shows all the GPS data for individuals displayed on the map. This includes every GPS fix (not only every 20 as mapped) for the species, management area (for deer), NDOW ID, and date range selected. This data can be downloaded as a .CSV file by clicking the *Download Data* button, and saved to your Downloads folder. The downloaded data will include every GPS location.

### Migration Tab

This tab can be used to calculate Net Squared Displacement (NSD), and basic utilization distributions (TODO) overlayed on a map. Visual inspection of selected GPS data and basic analyses are available. Further development is in progress.

Refer to [Bunnefeld et al. 2010](http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2656.2010.01776.x/full) for more information on NSD calculations and utility.

## Demo

[View a brief demo here.](https://drive.google.com/file/d/0B1OupsoLNZvkcExIT2VzcUlySWc/view?usp=sharing)

## About the Data

The sources of raw data are the GPS collar manufacturers for each collar. This data is organized and maintained in a relational database by NDOW GIS & Game staff. New collar data is migrated to the database once a month. Therefore, the data in this application isn't the most up to date. At the greatest, the data is delayed by 30 days. The data is stored on NDOW's network, therefore users must be networked to and have permission for the Game folder on the GIS Share drive. This is due to the sensitive nature of location data. This is a temporary solution, we are looking for alternatives that will make the data more accessible to all staff that may use this data.

## Known Bugs

Current version has no known bugs

## TODO
- [ ] Change name of column headers
- [ ] Use 'start' and 'stop' markers for first and last points
- [x] Clear/Reset Map page input
- [ ] Reset input on tab change... may not be possible?
- [ ] Use data.table for all data munging
- [ ] Use Leaflet Draw to allow users to subset based on the area
- [ ] A way for users to request analysis based on data they've entered
- [ ] Remove erroneous points, persistent to data download
- [x] Include migration analysis plots
- [ ] Include utilization distributions.
  - [ ] As shapefile?
- [x] NSD plots and associated maps

To add items to the TODO list, or report errors that occur while using the application contact Mitch Gritts (mgritts@ndow.org).
