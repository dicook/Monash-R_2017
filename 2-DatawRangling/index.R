## ---- echo = FALSE-------------------------------------------------------
library(knitr)
opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.height = 4,
  fig.width = 8,
  fig.align = "center",
  cache = FALSE
)
options(digits = 2)

## ---- echo = FALSE-------------------------------------------------------
library(tidyr)
library(dplyr)
library(readr)
data(french_fries, package = "reshape2")
kable(head(french_fries, 4), format = "markdown", row.names = F)

## ---- echo=FALSE---------------------------------------------------------
genes <- read_csv("http://dicook.github.io/Monash-R/data/genes.csv")
kable(genes)

## ------------------------------------------------------------------------
library(XML)
src <- "http://www.realclearpolitics.com/epolls/2012/president/us/republican_presidential_nomination-1452.html"
tables <- readHTMLTable(src)
polls <- tables[[1]]
head(polls)

## ---- eval = FALSE-------------------------------------------------------
## library(maptools)
## xx <- readShapeSpatial("http://dicook.github.io/Monash-R/data/australia/region.shp")
## object.size(as(xx, "SpatialPolygons"))
## xxx <- thinnedSpatialPoly(as(xx, "SpatialPolygons"),
##   tolerance=0.5, minarea=0.001, topologyPreserve=TRUE)
## object.size(as(xxx, "SpatialPolygons"))
## qplot(long, lat, data=xx, group=group) + geom_path() + coord_map()

## ---- echo=FALSE, results='asis'-----------------------------------------
kable(head(french_fries), format = "markdown", row.names = FALSE)

## ------------------------------------------------------------------------
library(tidyr)
ff_long <- gather(french_fries, key = variable, value = rating, potato:painty)
head(ff_long)

## ------------------------------------------------------------------------
french_fries_wide <- spread(ff_long, key = variable, value = rating)

head(french_fries_wide)

## ---- fig.height=2, fig.width=8------------------------------------------
library(ggplot2)
ff.m <- french_fries %>% 
  gather(type, rating, -subject, -time, -treatment, -rep)
head(ff.m)
qplot(rating, data=ff.m, binwidth=2) + 
  facet_wrap(~type, ncol=5) 

## ------------------------------------------------------------------------
qplot(type, rating, data = ff.m, fill = type, geom = "boxplot")

## ---- fig.show='hold', fig.align='default', fig.height=4, fig.width=4----
head(ff.m)
ff.s <- ff.m %>% spread(rep, rating)
head(ff.s)

## ---- fig.show='hold', fig.align='default', fig.height=3, fig.width=3----
qplot(`1`, `2`, data=ff.s) + theme(aspect.ratio=1) + 
  xlab("Rep 1") + ylab("Rep 2")
qplot(`1`, `2`, data=ff.s) + theme(aspect.ratio=1) + 
  xlab("Rep 1") + ylab("Rep 2") + 
  scale_x_log10() + scale_y_log10()

## ------------------------------------------------------------------------
billboard <- read.csv("http://dicook.github.io/Monash-R/data/billboard.csv")

## ---- echo=FALSE, fig.height=3-------------------------------------------
long_billboard <- gather(billboard, key = week, value = rank, X1:X76)
long_billboard$week <- as.numeric(gsub("X", "", long_billboard$week))

qplot(week, rank, data = long_billboard, geom = "line", colour = artist, group = track)

## ------------------------------------------------------------------------
library(dplyr)
french_fries_split <- group_by(ff_long, variable) # SPLIT
french_fries_apply <- summarise(french_fries_split, rating = mean(rating, na.rm = TRUE)) # APPLY + COMBINE
french_fries_apply

## ------------------------------------------------------------------------
french_fries %>%
    gather(key = variable, value = rating, potato:painty) %>%
    group_by(variable) %>%
    summarise(rating = mean(rating, na.rm = TRUE))

## ------------------------------------------------------------------------
french_fries %>%
    filter(subject == 3, time == 1)

## ------------------------------------------------------------------------
french_fries %>%
    arrange(desc(rancid)) %>%
    head

## ------------------------------------------------------------------------
french_fries %>%
    select(time, treatment, subject, rep, potato) %>%
    head

## ------------------------------------------------------------------------
french_fries %>%
    group_by(time, treatment) %>%
    summarise(mean_rancid = mean(rancid), sd_rancid = sd(rancid))

## ------------------------------------------------------------------------
french_fries %>% 
  select(subject, time, treatment) %>% 
  tbl_df() %>% 
  count(subject, time) %>%
  spread(time, n)

## ------------------------------------------------------------------------
french_fries %>% 
  gather(type, rating, -subject, -time, -treatment, -rep) %>%
  select(subject, time, treatment, type) %>% 
  tbl_df() %>% 
  count(subject, time) %>%
  spread(time, n)

## ------------------------------------------------------------------------
qplot(time, rating, data=ff.m, colour=treatment) + 
  facet_grid(subject~type) 

## ------------------------------------------------------------------------
ff.m.av <- ff.m %>% 
  group_by(subject, time, type, treatment) %>%
  summarise(rating=mean(rating))
qplot(time, rating, data=ff.m, colour=treatment) + 
  facet_grid(subject~type) +
  geom_line(data=ff.m.av, aes(group=treatment))

## ------------------------------------------------------------------------
genes <- read_csv("http://dicook.github.io/Monash-R/data/genes.csv")
genes

## ------------------------------------------------------------------------
gather(genes, variable, expr, -id)

## ------------------------------------------------------------------------
genes %>%
  gather(variable, expr, -id) %>%
  separate(variable, c("trt", "leftover"), "-")

## ------------------------------------------------------------------------
genes %>%
  gather(variable, expr, -id) %>%
  separate(variable, c("trt", "leftover"), "-") %>%
  separate(leftover, c("time", "rep"), "\\.")

## ------------------------------------------------------------------------
gclean <- genes %>%
  gather(variable, expr, -id) %>%
  separate(variable, c("trt", "leftover"), "-") %>%
  separate(leftover, c("time", "rep"), "\\.") %>%
  mutate(trt = sub("W", "", trt)) %>%
  mutate(rep = sub("R", "", rep))
gclean

## ---- fig.height=3-------------------------------------------------------
gmean <- gclean %>% 
  group_by(id, trt, time) %>% 
  summarise(expr = mean(expr))
qplot(trt, expr, data = gclean, colour = time) + 
  xlab("Type of modification") + ylab("Expression") + 
  facet_wrap(~id) +
  geom_line(data = gmean, aes(group = time))

## ------------------------------------------------------------------------
db <- nycflights13_sqlite()
db

## ------------------------------------------------------------------------
tbl(db, "flights")

## ------------------------------------------------------------------------
top <- tbl(db, "flights") %>%
  count(carrier) %>%
  arrange(desc(n))
top

## ------------------------------------------------------------------------
tbl(db, "airlines")

## ------------------------------------------------------------------------
a <- tbl(db, "airlines")
top <- left_join(top, a)
top

## ------------------------------------------------------------------------
top$query

## ------------------------------------------------------------------------
tbl(db, "flights") %>%
  group_by(carrier) %>% 
  summarise_each(funs(mean)) %>%
  arrange(desc(dep_delay))

## ---- error = TRUE-------------------------------------------------------
tbl(db, "flights") %>%
  group_by(carrier) %>%
  summarise(q = quantile(dep_delay, 0.75))

## ------------------------------------------------------------------------
flights <- collect(tbl(db, "flights"))

## ------------------------------------------------------------------------
object.size(tbl(db, "flights"))
object.size(flights)

## ------------------------------------------------------------------------
flights %>%
  group_by(carrier) %>%
  summarise(q = quantile(arr_time, 0.75, na.rm = T))

## ------------------------------------------------------------------------
fortunes::fortune(192)

## ------------------------------------------------------------------------
library(lubridate)

now()
today()
now() + hours(4)
today() - days(2)

## ------------------------------------------------------------------------
ymd("2013-05-14")
mdy("05/14/2013")
dmy("14052013")
ymd_hms("2013:05:14 14:5:30", tz = "America/New_York")

## ------------------------------------------------------------------------
flights %>%
  mutate(date = paste(year, month, day, sep = "-")) %>%
  mutate(date2 = ymd(date)) %>%
  select(date, date2)

## ---- warning = TRUE-----------------------------------------------------
flights <- flights %>%
  mutate(date = paste(year, month, day, sep = "-")) %>%
  mutate(time = paste(hour, minute, "0", sep = ":")) %>%
  mutate(dt = ymd_hms(paste(date, time))) 

## ------------------------------------------------------------------------
flights %>%
  filter(is.na(dt)) %>%
  select(hour, minute, dt)

## ------------------------------------------------------------------------
models <- flights %>% 
  group_by(carrier) %>%
  do(m = lm(dep_delay ~ as.numeric(dt), data = .))
models

## ------------------------------------------------------------------------
models %>%
  mutate(slope_min = coef(m)[["as.numeric(dt)"]]) %>%
  mutate(slope_day = slope_min * 60 * 60 * 24) %>%
  arrange(desc(slope_day)) %>%
  left_join(a, copy = TRUE)

