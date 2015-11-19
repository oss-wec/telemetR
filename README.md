# GPSextractor
A Shiny application to extract GPS collar data

*Requirements* for the app 

Download and install the latest version of R [(R version 3.2.2)](https://cran.r-project.org/bin/windows/base/).

Download and install the latest version of RStudio [(RStudio Desktop 0.99.489)](https://www.rstudio.com/products/rstudio/download/)

copy and paste these commands into the RStudio console.
```
install.packages("leaflet")
install.packages("DT")
install.packages("data.table")
install.packages("shiny")
```

To run the app use this command
```
shiny::runGitHub("GPSextractor", "kissmygritts")
```