melbourne-walking is a small R project that is designed to:

-   show how to scrape a web page for links
-   download those links (without re-downloading on repeated runs) en masse
-   read in a bunch of data files
-   clean the resultant data frame
-   munge the data for a vis
-   visualize the data
-   compare raw R script vs Rmd vs Jupyter notebook

You can:

-   Open the whole project in RStudio and run the R script, knit the Rmd file or spin the R script
-   Run the `melbourne-walking.R` from the top level of the project folder via `Rscript R/melbourne-walking.R`
-   Open `melbourne-walking.ipynb` in a Jupyter notebook with an R kernel

There are four output files:

-   `melbourne-walking.html` showing the output from the Rmd knit
-   `./R/melbourne-walking.html` showing the output from a "spin" of the R script
-   `melbourne-walking-ipynb.html` showing what the Jupyter notebook output looks like
-   `Rplots.pdf` which is a PDF version of running just the R script from the command line