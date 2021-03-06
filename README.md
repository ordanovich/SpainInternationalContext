<p align="center"><a href="http://portalsalud.csic.es/"><img src="https://github.com/ordanovich/images/blob/master/webportal_overview.gif?raw=true"></a></p>
<p align="center"><a href="http://longpop-itn.eu/"><img src="https://github.com/ordanovich/images/blob/master/logo3inline_small.png?raw=true"></a></p>

[Spain in International Context](http://portalsalud.csic.es/international_context/) is one of the core applications for data retrieval and visualization within the [Web Portal for Health and Population in Spain](http://portalsalud.csic.es/). It allows you to search for available datasets in 4 main statistical repositories at european and global levels and create versatile graphics which you can then export to a desired format. 

### Data providers

#### :heavy_check_mark: [Eurostat](https://ec.europa.eu/eurostat/home)
> to access the application for bulk data retrieval only [click here](http://portalsalud.csic.es/eurostat_download/) 
#### :heavy_check_mark: [Worldbank](https://www.worldbank.org/)
> to access the application for bulk data retrieval only [click here](http://portalsalud.csic.es/worldbank_download/) 
#### :heavy_check_mark: [World Health Organisation](https://www.who.int/)
> to access the application for bulk data retrieval only [click here](http://portalsalud.csic.es/who_download/) 
#### :heavy_check_mark: [Organisation for Economic Co-operation and Development](https://www.oecd.org)
> to access the application for bulk data retrieval only [click here](http://portalsalud.csic.es/worldbank_download/) 

### Nomenclature

- :raising_hand: [ui.R](https://github.com/ordanovich/SpainInternationalContext/blob/master/ui.R): User Interface component
- :computer: [server.R](https://github.com/ordanovich/SpainInternationalContext/blob/master/server.R): Server component
- :globe_with_meridians: [global.R](https://github.com/ordanovich/SpainInternationalContext/blob/master/global.R): Helper functions

#### :mag_right: Navigate between pages using the sidebar menu

![](https://github.com/ordanovich/images/blob/master/animated_switch_pages.gif?raw=true)

### Eurostat

#### :mag_right: Search for the datasets or tables using keywords

![](https://github.com/ordanovich/images/blob/master/animated_eurostat_search.gif?raw=true)

> Note that the dependency of the variables if hiereachical. 

#### :chart_with_upwards_trend: Explore the time-series graph

> Change the display charateristics of the graph and modify the parameters for better model fit.

![](https://github.com/ordanovich/images/blob/master/animated_eurostat_plotly.gif?raw=true)

#### :straight_ruler: Compare the data for Spain to other regions at selected NUTS level

> Tag and untag outliers in your data. Export results to an image. 

![](https://github.com/ordanovich/images/blob/master/animated_eurostat_ggstatsplot.gif?raw=true)

#### :fire: Generate a heatmap for Spain at selected NUTS level

> Export graph to any of the available formats.

![](https://github.com/ordanovich/images/blob/master/_animated_eurostat_heatmap.gif?raw=true)

### Worldbank

#### :chart_with_downwards_trend: Plot time series graphs and generate maps for selected variable 

> Explore the options to change the appearence of the graphs.

![](https://github.com/ordanovich/images/blob/master/animated_wb_graphMap.gif?raw=true)

#### :straight_ruler: Compare data at regional or country level 

![](https://github.com/ordanovich/images/blob/master/animated_wb_ggstatsplot.gif?raw=true)

### WHO

#### :o: Make circular plots and explore time series graphs

![](https://github.com/ordanovich/images/blob/master/animated_who_circular.gif?raw=true)

### OECD

#### :bar_chart: Run animations to see the change in the variable of choice over time and plot heatmaps

![](https://github.com/ordanovich/images/blob/master/animated_oecd.gif?raw=true)
