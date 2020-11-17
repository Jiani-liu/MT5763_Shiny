# MT5763_Shiny

## COVID-19 real time interactive tool

This github page contains the code and input data for the [COVID 19 Real Time INteractive Tool](https://fancy-statistic.shinyapps.io/shiny_covid/) developed by Jiani Liu.

Input data are obtained from the [Johns Hopkins Center for Systems Science and Engineering github page](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series).

The Shiny app aims to complement existing COVID-19 mapping dashboards (such as those developed by the [Johns Hopkins University](https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6)) with several interactive features, including the worldwide map and timeline function and the ability to overlay past outbreaks. 

## Shiny interface

Follow [this](https://fancy-statistic.shinyapps.io/shiny_covid/) link for the interactive Shiny app. A screenshot of the interface is provided below.

![Shiny app interface](www/mapper.png)

## Analysis code

Key elements of the analysis code are as follows:
- *global.R* – an R script that extracts and reformats time-series from the [Johns Hopkins Center for Systems Science and Engineering github page](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series). The output files are saved in the *input_data* folder.
- *app.R* - an R script used to render the Shiny app. This consists of several plotting functions as well as the ui (user interface) and server code required to render the Shiny app. The script has become more complex over time as a growing number of interactive features has been added.
- *input_data* - a folder containing dynamic input data relating to the evolving COVID-19 pandemic (updated by *jhu_data_full.R* and  *ny_data_us.R*) and static input data relating to past epidemics and country mapping coordinates.

## Updates

The [Shiny app](https://fancy-statistic.shinyapps.io/shiny_covid/) automatically updates itself based on the code in *app.R* and updated case data can be downloaded directly from the app via the 'Data' tab.  


## Other resources

Several resources proved invaluable when building this app, including:
- A [tutorial by Florianne Verkroost](https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/) on building interactive maps;
- The [SuperZIP app](https://shiny.rstudio.com/gallery/superzip-example.html) and [associated code](https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example);
- The [RStudio Leaflet tutorials](https://rstudio.github.io/leaflet/).

## Authors
Jiani Liu, School of Computer Science, University of St Andrews

## Contact
jl341@st-andrews.ac.uk
