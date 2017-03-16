## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  cache = FALSE,
  fig.height = 2,
  fig.width = 5,
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE, echo = FALSE-----------------------------------------
## # devtools::install_github("metacran/crandb")
## # pkgs <- crandb::list_packages(limit = 999999)
## # length(pkgs)
## # [1] 7330

## ---- eval = FALSE-------------------------------------------------------
## curl::curl_download(
##   "https://raw.githubusercontent.com/dicook/Monash-R/gh-pages/1-Intro/index.Rmd",
##   "index.Rmd"
## )
## file.edit("index.Rmd")

## ---- eval = FALSE-------------------------------------------------------
## curl::curl_download(
##   "https://raw.githubusercontent.com/dicook/Monash-R/gh-pages/1-Intro/index.R",
##   "index.R"
## )
## file.edit("index.R")

## ------------------------------------------------------------------------
c(0, 1)

## ------------------------------------------------------------------------
x <- c(0, 1)

## ------------------------------------------------------------------------
sum(x + 10)

## ------------------------------------------------------------------------
c("a", "b")[1]
c("a", "b")[c(T, F)]

## ------------------------------------------------------------------------
x <- list(
  a = 10,
  b = c(1, "2")
)
x$a
x[["a"]]
x["a"]

## ------------------------------------------------------------------------
str(x)

## ---- eval = FALSE-------------------------------------------------------
## browseVignettes("dplyr")

## ------------------------------------------------------------------------
`+`

## ------------------------------------------------------------------------
"+" <- function(x, y) "I forgot how to add"
1 + 2

## ------------------------------------------------------------------------
rm("+")

## ------------------------------------------------------------------------
data(economics, package = "ggplot2")
# data frames are essentially a list of vectors
str(economics)

## ---- fig.height = 3, fig.width = 10-------------------------------------
library(ggplot2)
p <- ggplot(economics, aes(date, unemploy / pop)) + 
  geom_line()
p

## ---- fig.show = 'hold'--------------------------------------------------
p
p + geom_smooth(method = "lm", se = F)
p + geom_smooth(method = "loess", se = F)
p + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr"), se = F)

## ------------------------------------------------------------------------
m <- lm((unemploy / pop) ~ date, data = economics)
str(m)

## ---- fig.height = 4.5, fig.width = 10-----------------------------------
economics$yhat <- m$fitted.values
ggplot(economics) + 
  geom_line(aes(date, unemploy / pop)) +
  geom_line(aes(date, yhat), color = "blue")

## ---- fig.height = 4.5, fig.width = 10, fig.align = "center"-------------
library(plotly)
ggplotly()

## ---- echo = FALSE-------------------------------------------------------
data(economics)

## ------------------------------------------------------------------------
head(economics)

## ------------------------------------------------------------------------
library(tidyr)
e <- gather(economics, variable, value, -date)
head(e)

## ---- fig.height = 5.5, fig.width = 10-----------------------------------
ggplot(e, aes(date, value)) + geom_line() +
  facet_wrap(~ variable, scales = "free_y")

## ------------------------------------------------------------------------
library(dplyr)
# add a year column
e <- mutate(economics, year = format(date, "%Y"))
# split by year
e <- group_by(e, year)
# mean of each group
summarise(e, mpce = mean(pce))

## ------------------------------------------------------------------------
economics %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(mpce = mean(pce))

## ------------------------------------------------------------------------
library(babynames)
head(babynames)
dim(babynames)

## ---- cache = TRUE-------------------------------------------------------
library(readr)
bb_path <- tempfile(fileext = ".csv", tmpdir = ".")
write_csv(babynames, bb_path)
read_csv(bb_path)

## ---- eval = FALSE-------------------------------------------------------
## library(readxl)
## read_excel("my-spreadsheet.xls", sheet = "data")
## read_excel("my-spreadsheet.xls", sheet = 2)

## ---- eval = FALSE-------------------------------------------------------
## library(haven)
## # SAS files
## read_sas("path/to/file")
## # SPSS files
## read_por("path/to/file")
## read_sav("path/to/file")
## # Stata files
## read_dta("path/to/file")

## ------------------------------------------------------------------------
db <- src_sqlite("babynames.sqlite3", create = TRUE)
if (!db_has_table(db$con, "babynames")) {
  copy_to(db, babynames)
}

## ------------------------------------------------------------------------
db
tbl(db, "babynames")

## ------------------------------------------------------------------------
h <- db %>% 
  tbl("babynames") %>%
  filter(name == "Hilary")

## ------------------------------------------------------------------------
class(h)
h$query
# execute SQL query and bring into R
hc <- collect(h)
class(hc)
hc

## ---- fig.height = 5, fig.width = 10, cache = FALSE, warning = FALSE-----
plot_ly(hc, x = year, y = prop, color = sex, colors = c("blue", "hotpink"))

## ------------------------------------------------------------------------
popular <- babynames %>%
  group_by(name) %>%
  summarise(N = sum(n)) %>%
  arrange(desc(N))
popular

## ------------------------------------------------------------------------
top <- top_n(popular, 1000)
topnames <- subset(babynames, name %in% top[["name"]])
topnames

## ---- eval = FALSE-------------------------------------------------------
## library(shiny)
## library(ggplot2)
## ui <- bootstrapPage(
##   selectizeInput(
##     inputId = 'name',
##     label = 'Enter a name',
##     choices = unique(topnames$name),
##     selected = "James",
##     multiple = TRUE
##   ),
##   plotOutput('plot')
## )
## server <- function(input, output) {
##   output$plot <- renderPlot({
##     dat <- subset(topnames, name %in% input$name)
##     ggplot(dat, aes(year, prop, colour = sex)) +
##       geom_line() + facet_wrap(~ name)
##   })
## }
## runApp(shinyApp(ui, server))

