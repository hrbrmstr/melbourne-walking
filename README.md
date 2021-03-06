### What is this?

`melbourne-walking` is a small R project that is designed to:

-   show how to scrape a web page for links
-   download those links (without re-downloading on repeated runs) en masse
-   read in a bunch of (well, all of) those data files
-   clean the resultant data frame (it has varying date formats)
-   munge the data for a vis
-   visualize the data both as a static ggplot heatmap and interactive [htmlwidget](http://htmlwidgets.org)
-   compare raw R script vs Rmd vs [Jupyter notebook](https://jupyter.org/)

### Why "Melbourne Walking"?

The City of Melboune maintains a [site](http://www.pedestrian.melbourne.vic.gov.au/) where you can see data from their 24-hour pedestrian counting system which measures pedestrian activity in the city each day.

The system counts pedestrian movements to give the City of Melbourne a better understanding of how people use the city so we can manage the way they function and plan for future needs.

The main page has an interactive visualization you can explore, but they also [make the data available for download](http://www.pedestrian.melbourne.vic.gov.au/datadownload.html).

I found the data set in a [Python post](http://myownhat.blogspot.com/2015/07/quick-example-heat-map-of-pedestrian.html) and felt that it might be useful for the course I'm teaching this Fall.

### What do I do next?

The R code in this repo grabs all that data and visualizes the most active hours using a heatmap.

![](heatmap.png)

You can see the knitted Rmd over on [RPubs](http://rpubs.com/hrbrmstr/melbourne-walking).

If you don't have [R](http://www.r-project.org/) or [RStudio](https://www.rstudio.com/) just follow those links to get them. It'll be a steep learning curve if you aren't familiar with R even a little bit, though.

There are many ways to work with the R code. You can:

-   Open the whole project in RStudio and either:
    -   Run the `melbourne-walking.R` script in the editor
    -   "knit" the Rmd file, or
    -   "spin" the plain R script since it has knittable comments
-   Run the `melbourne-walking.R` from the top level of the project folder via `Rscript R/melbourne-walking.R`
-   Open `melbourne-walking.ipynb` in a Jupyter notebook with an R kernel

The interactive version will most likely not work from your R console.

There are four output files in the repo (and that will be generated if you run the R code in the ways described above):

-   `melbourne-walking.html` showing the output from the Rmd knit
-   `./R/melbourne-walking.html` showing the output from a "spin" of the R script
-   `melbourne-walking-ipynb.html` showing what the Jupyter notebook output looks like
-   `Rplots.pdf` which is a PDF version of running just the R script from the command line

### What if I get stuck?

First, you may want to ensure you do:

    devtools::install_github("hadley/rvest")
    devtools::install_github("hadley/xml2")

since there's a change on the way into how `rvest` & `xml2` interact and I tend to stay on `devtools` branches of the Hadleyverse.

You can [file an issue](https://github.com/hrbrmstr/melbourne-walking/issues) and I'll do my best to answer back in a timely fashion.

### Changelog

-   2015-07-20 Switched from parula to viridis palette
-   2015-07-10 Initial version

------------------------------------------------------------------------

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">Melbourne Walking in R</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://github.com/hrbrmstr/melbourne-walking" property="cc:attributionName" rel="cc:attributionURL">Bob Rudis</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.
