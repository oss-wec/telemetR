# telemetR

A Shiny application for the exploratory analysis and visualization of animal movement.

## Justification

We have organized all GPS collar location data (~2.6 million records) into a single database yet all data output is limited to SQL statements and Microsoft Excel line limit (65,000 rows). The application also provides an GUI for exploratory data analysis and visualization of animal movement. Users can estimate basic home range and utilization distributions, and plots of standard movement parameters. After exploratory analysis is complete, data can be downloaded for further analysis.

## Demo & Instructions

![](http://imgur.com/43W2vha)

[View a brief demo here.](https://youtu.be/uNKUe6SvPMw)

[Instructions for using the app](https://github.com/ndow-wisr/telemetR/wiki/Instructions)

## Known Bugs

Kernel Density estimation may ~~error~~ produce strangely shaped contours. This is due to the extent of the area to estimate probability is smaller than the actual contours. 

[Feature requests and report errors that occur while using the application.](https://github.com/ndow-wisr/telemetR/issues)
