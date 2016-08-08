# telemetR
*1.0.0*

A Shiny application for the exploratory analysis and visualization of animal movement.

## Justification

We have organized all GPS collar location data (~2 million records) into a single database yet all data output is limited to SQL statements and Microsoft Excel line limit (65,000 rows). The application also provides an GUI for exploratory data analysis and visualization of animal movement. Users can estimate basic home range and utilization distributions, and plots of standard movement parameters. After exploratory analysis is complete, data can be downloaded for further analysis.

## Demo & Instructions

[View a brief (older version) demo here.](https://drive.google.com/file/d/0B1OupsoLNZvkcExIT2VzcUlySWc/view?usp=sharing)

[Instructions for using the app](https://github.com/ndow-wisr/telemetR/wiki/Instructions)

## Known Bugs

Kernel Density estimation may error. This error is from the `adehabitatHR` package, I'm working on it.

[Feature requests and report errors that occur while using the application.](https://github.com/ndow-wisr/telemetR/issues)
