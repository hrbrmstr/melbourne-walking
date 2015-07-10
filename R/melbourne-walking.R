library(rvest)
library(httr)
library(dplyr)
library(xml2)
library(pbapply)
library(data.table)
library(lubridate)
library(tidyr)
library(ggplot2)
library(scales)
library(grid)

data_dir <- "./data"
base_url <- "http://www.pedestrian.melbourne.vic.gov.au/"

pg <- read_html(modify_url(base_url, path="datadownload.html"))

pg %>%
  html_nodes("a[href^='datadownload']") %>%
  html_attr("href") %>%
  pbsapply(function(x) {
    dfile <- basename(x)
    invisible(try(GET(modify_url(base_url, path=x),
                      write_disk(file.path(data_dir, dfile))),
                  silent=TRUE))
    dfile
  }) -> data_files

rbindlist(pblapply(data_files, function(x) {
  data.table::fread(file.path(data_dir, x), verbose=FALSE)
}), fill=TRUE) -> walking

walking$Date <- parse_date_time(walking$Date, orders=c("mdY", "dby", "dmY"))

walking <- gather(walking, location, count, -Date, -Hour)
walking$count <- as.numeric(walking$count)
walking <- filter(walking, !is.na(count))
walking$weekday <- wday(walking$Date, label=TRUE)

walking %>% count(weekday, Hour, wt=count) -> wday_totals

tbl_df(wday_totals) %>%
  mutate(Hour=factor(Hour),
         weekday=factor(weekday)) %>%
  rename(`Total Walkers\n(log scale)`=n) -> wday_totals

ggplot(wday_totals, aes(x=Hour, y=weekday)) +
  geom_tile(aes(fill=`Total Walkers\n(log scale)`), color="white", size=2) +
  scale_x_discrete(expand=c(0, 0)) +
  scale_y_discrete(expand=c(0, 0)) +
  scale_fill_gradientn(trans="log10", label=comma,
                       colours=rev(c("#f7fcb9", "#d9f0a3", "#addd8e", "#78c679",
                                     "#41ab5d", "#238443", "#006837", "#004529"))) +
  coord_equal() +
  labs(x=NULL, y=NULL, title="Melbourne Walkers (Time of Day/Day of Week)\n") +
  theme(plot.title=element_text(face="bold", hjust=0, size=16)) +
  theme(legend.key.width=unit(2, "cm")) +
  theme(legend.position="bottom")




