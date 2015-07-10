`melbourne-walking` is a small R project that is designed to:

-   show how to scrape a web page for links
-   download those links (without re-downloading on repeated runs) en masse
-   read in a bunch of data files
-   clean the resultant data frame
-   munge the data for a vis
-   visualize the data
-   compare raw R script vs Rmd vs Jupyter notebook

The City of Melboune maintains a [site](http://www.pedestrian.melbourne.vic.gov.au/) where you can see data from their 24-hour pedestrian counting system which measures pedestrian activity in the city each day.

The system counts pedestrian movements to give the City of Melbourne a better understanding of how people use the city so we can manage the way they function and plan for future needs.

The main page has an interactive visualiztion you can explore, but they also [make the data available for download](http://www.pedestrian.melbourne.vic.gov.au/datadownload.html).

The R code in this repo grabs all that data and visualizes the most active hours using a heatmap.

There are many ways to work with the R code. You can:

-   Open the whole project in RStudio and either:
    -   Run the `melbourne-walking.R` script in the editor
    -   "knit" the Rmd file, or
    -   "spin" the plain R script since it has knittable comments
-   Run the `melbourne-walking.R` from the top level of the project folder via `Rscript R/melbourne-walking.R`
-   Open `melbourne-walking.ipynb` in a Jupyter notebook with an R kernel

There are four output files in the repo (and that will be generated if you run the R code in the wasy described above):

-   `melbourne-walking.html` showing the output from the Rmd knit
-   `./R/melbourne-walking.html` showing the output from a "spin" of the R script
-   `melbourne-walking-ipynb.html` showing what the Jupyter notebook output looks like
-   `Rplots.pdf` which is a PDF version of running just the R script from the command line
