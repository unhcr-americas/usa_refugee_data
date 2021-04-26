Accepted Refugees United States
================


This package is built on the work from [Fabio Votta](https://favstats.eu) for the preparation of the article [Exploring U.S. Refugee Data](https://favstats.eu/post/exploring_us_refugee_data/) . 


The original scripts were scraping from from [US Refugee Processing Center (RPC)](https://www.wrapsnet.org/admissions-and-arrivals/) __Interactive reporting__ that has been now decommissioned.


![ ](inst/refdata.jpg?raw=true "Refugee Processing Center")



## Refresh Data

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
Data then consist in 3 data frames

 1. age_dataset.Rdata
 2. religion_dataset.Rdata
 3. education_dataset.Rdata

## Generate Viz

3 ggplot2 functions allows to build quick visualization  from those scrapped dataset

Additionally you can look at the vignettes to get more advanced viz

## Install

The package is still under development - need to fix the scrapping functions

```{r}

remotes::install_github(unhcr-americas/usa_refugee_data)

```

#### Building package documentation 

`devtools::document()`

`devtools::check(document = FALSE)`

`pkgdown::build_site()`

------------