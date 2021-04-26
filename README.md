Accepted Refugees United States
================


This package is built on the work from [Fabio Votta](https://favstats.eu) for the preparation of the article [Exploring U.S. Refugee Data](https://favstats.eu/post/exploring_us_refugee_data/) . 


It allows to scrap data from from US Refugee Processing Center (RPC): <http://ireports.wrapsnet.org/Interactive-Reporting/EnumType/Report?ItemPath=/rpt_WebArrivalsReports/MX%20-%20Arrivals%20by%20Nationality%20and%20Religion> and generate a few pre-build visualizations.

## Install


## Refresh Data

data consist in 3 data frame

 1. age_dataset.Rdata
 2. religion_dataset.Rdata
 3. education_dataset.Rdata

A scrapping function allows to refresh the dataset using `Rselenium` package


```{r}

usaRefugeeData::data_dir()

port <- sample(4000L:5000L, 1)
rD <- RSelenium::rsDriver(verbose = FALSE, port = port)

remDr <- rD$client

url <- "http://ireports.wrapsnet.org/Interactive-Reporting/EnumType/Report?ItemPath=/rpt_WebArrivalsReports/MX%20-%20Arrivals%20for%20a%20Demographic%20Profile"

remDr$navigate(url)

usaRefugeeData::set_up()

1:266 %>% walk(usaRefugeeData::download_all)

```


## Generate Viz

3 ggplot2 functions allows to build quick visualization  from those scrapped dataset

additionally you can look at the vignettes to get more advanced viz


#### Building package documentation 

`devtools::document()`

`devtools::check(document = FALSE)`

`pkgdown::build_site()`

------------