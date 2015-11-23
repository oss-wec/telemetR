# Collar Data Export
A Shiny application to extract GPS collar data from NDOW's GPS collar data warehouse.

## Requirements for the app

Download and install the latest versions of R [(R version 3.2.2)](https://cran.r-project.org/bin/windows/base/) and RStudio [(RStudio Desktop 0.99.489)](https://www.rstudio.com/products/rstudio/download/).

copy and paste these commands into the RStudio console. If this is the first time you've used R you'll be asked to choose a CRAN mirror. Use the *US (CA-1) https mirror.* This will install the required packages for the app.

```r
install.packages("leaflet")
install.packages("DT")
install.packages("data.table")
install.packages("shiny")
```

To run the app use the command below. For all future uses this is the only line you will need to run.
```r
shiny::runGitHub("CollarDataExport", "kissmygritts")
```

Permissions for the GIS data drive (V:/ActiveProjects/Game) is required to access the data for the application.

## Instructions

### Collared Animals Page

The application opens with a table of animals collared with GPS collars. The table can be filtered using the *Species* and *Management Area* drop down lists. The *Management Area* drop down only filters the table for Mule Deer.

Any field in the table can be searched using the search bar on the right. The table can be sorted by clicking the arrows next to each column name.

### Map Page

The second page is a map plotted with the animals from the table on the Collared Animals Page. The blue dots represent the first and last GPS location of the animal. The grey line is a smoothed path (every 20 locations) between the first and last point.

The map can be filtered by individual by entering individual NDOW IDs in the *NDOW ID* text box. The NDOW IDs used should be from the table on the Collared Animals Page. To enter more than one use a comma to separate NDOW IDs (ex: 831, 3669). To further filter the plot enter a *Date Range* and click the *Use Date Range* checkbox. This will only include points between the two dates.

Delete the text in the input boxes and uncheck the *Use Date Range* to reset the filters on the map. If changing species or management area on the collared animals page, the data on the Map page will need to be reset in order to view all the animals.

### Data Page

The third page shows all the GPS data for individuals displayed on the map. This includes every GPS fix (not only every 20 as mapped) for the species, management area (for deer), NDOW ID, and date range selected. This data can be downloaded as a .CSV file by clicking the *Download Data* button. The downloaded data will include every GPS location.

## Demo

[View a brief demo here.](https://drive.google.com/open?id=0B1OupsoLNZvkcExIT2VzcUlySWc)

## ToDo
- [ ] Change name of column headers
- [ ] Use 'start' and 'stop' markers for first and last points
- [ ] Clear input on Map page when changing input on the first page
- [ ] Use data.table for all data munging
- [ ] Use Leaflet Draw to allow users to subset based on the area
- [ ] A way for users to request analysis based on data they've entered
- [ ] Remove erroneous points, persists to data download
