#' data_dir
#' 
#' This function creates a data folder if one doesn't already exist.
#'
#' @md
#' @examples
#' \dontrun{
#' }
#' @export


data_dir <- function() {
  if(!dir.exists("data")) dir.create("data")
}


#' set_up
#'
#' Initial Set Up for scrapping functions
#'
#' @md
#' @examples
#' \dontrun{
#' }
#' @export
set_up <- function() {
  year_type <- remDr$findElement(using = 'xpath', 
                                 value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl04_ctl03_ddValue"]/option[2]')
  year_type$clickElement()
  
  start_year <- remDr$findElement(using = 'xpath', 
                                  value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl04_ctl05_txtValue"]')
  start_year$clickElement()
  start_year$sendKeysToActiveElement(list("1/1/2001"))
  
  end_year <- remDr$findElement(using = 'xpath', 
                                value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl04_ctl09_txtValue"]')
  end_year$clickElement()
  
  end_year$sendKeysToActiveElement(list("1/1/2021"))
  
  init_selector <- remDr$findElement(using = 'xpath', 
                                     value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl04_ctl07_ddValue"]/option[2]') 
  cntry_selector$clickElement()
}


#' download_all
#'
#' Scrap and Download all data from US Refugee Processing Center (RPC)
#'
#' @md
#' @examples
#' \dontrun{
#' }
#' @export

download_all <- function(x) {
  
  print(glue::glue("{x} out of 266 ({round((x/266)*100, 2)}%)\n"))
  
  #this wont work the first time around!!
  cntry_selector <- remDr$findElement(using = 'xpath', 
                                      value = glue::glue('//*[@id="dnn_ctr513_View_ReportViewer1_ctl04_ctl07_ddValue"]/option[{x}]'))
  cntry_selector$clickElement()
  
  
  view_report <- remDr$findElement(using = 'xpath', 
                                   value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl04_ctl00"]')
  view_report$clickElement()
  
  # Sys.sleep(20)
  
  is_visible <- T
  
  while (is_visible) {
    Sys.sleep(5)
    is_visible <- remDr$getPageSource() %>% extract2(1) %>% read_html() %>% 
      html_nodes("#dnn_ctr513_View_ReportViewer1_AsyncWait_Wait") %>% 
      html_attr("style") %>% 
      str_detect("visibility: visible") 
    
    cat("Waiting 5 seconds...\n")
  }
  
  message("Clicking now!\n")
  
  click_diskette <- remDr$findElement(using = 'xpath', 
                                      value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl05_ctl04_ctl00_ButtonImg"]')
  click_diskette$clickElement()
  
  
  download_excel <- remDr$findElement(using = 'xpath', 
                                      value = '//*[@id="dnn_ctr513_View_ReportViewer1_ctl05_ctl04_ctl00_Menu"]/div[5]/a')
  download_excel$clickElement()
  
  out <- F
  
  while (magrittr::not(out)) {
    Sys.sleep(1)
    #out <- file.exists("C:/Users/fabio/Downloads/MX - Arrivals for a Demographic Profile.xls")
    out <- file.exists(glue::glue("{getwd()}/data/refugee_dat.xls"))
  }
  
 # file.rename("C:/Users/fabio/Downloads/MX - Arrivals for a Demographic Profile.xls", 
 #             glue::glue("C:/Users/fabio/Downloads/{x}.xls"))
  file.rename(glue::glue("{getwd()}/data/refugee_dat.xls"), 
              glue::glue("{getwd()}/data/{x}.xls"))  
}


#' is_complete_age
#'
#' @md
#' @examples
#' \dontrun{
#' }
#' @export
is_complete_age <- function(only_ages) {
  complete_ages <- c("Under 14" , "Age 14 to 20", "Age 21 to 30", 
                   "Age 31 to 40", "Age 41 to 50", "Age 51 to 64", 
                   "Age 65 and Over")

  only_ages <- only_ages %>% na.omit()  
  complete_ages %>% 
    map_lgl(~str_detect(only_ages, .x) %>% any)
}


#' is_complete_years
#'
#' @md
#' @examples
#' \dontrun{
#' }
#' @export
is_complete_years <- function(x) {
  complete_years <- glue::glue("CY {2002:2020}") %>% 
    as.character() 
  data <- x %>% na.omit()  
  complete_years %>% 
    map_lgl(~str_detect(data, .x) %>% any)
}


#' is_complete_gender
#'
#' @md
#' @examples
#' \dontrun{
#' }
#' @export
is_complete_gender <- function(x) {
  complete_genders <- c(glue::glue("{complete_years} F"),
                        glue::glue("{complete_years} M"))
  data <- na.omit(x)
  complete_genders %>% 
    map_lgl(~str_detect(data, .x) %>% any)
}

#' get_checkers
#'
#' @md
#' @param raw_dat Initial raw data
#' @return  
#' @examples
#' \dontrun{
#' }
#' @export
get_checkers <- function(raw_dat) {
  
  age_check <- raw_dat %>% 
    magrittr::use_series(department_of_state) %>% 
    is_complete_age %>% 
    tibble(complete_ages, present = .)
  
  years_check <- raw_dat %>% 
    filter(department_of_state == "Characteristic") %>% t %>% as.character %>% na.omit() %>% 
    keep(str_detect(., "CY")) %>% 
    is_complete_years %>% 
    tibble(complete_years, present = .)
  
  gender_check <- raw_dat %>% 
    tidyr::fill(department_of_state, .direction = "down") %>% 
    filter(department_of_state == "Characteristic") %>% t %>% as_tibble() %>%
    tidyr::fill(V1, .direction = "down") %>% 
    filter(str_detect(V2, "M|F")) %>% 
    mutate(tester = paste(V1, V2)) %>% 
    use_series(tester) %>% 
    is_complete_gender() %>% 
    tibble(complete_genders, present = .) %>% 
    arrange(complete_genders)
  
  return(list(age_check = age_check, 
              years_check = years_check, 
              gender_check = gender_check))
}

#' check_ages_complete
#'
#' @md
#' @examples
#' \dontrun{
#' }
#' @export
check_ages_complete <- function(x) {
  age_complete <- x$age_check %>%
    magrittr::use_series(present) %>% all
  
  years_complete <- x$years_check %>%
    magrittr::use_series(present) %>% all
  
  genders_complete <- x$gender_check %>% 
    use_series(present)  %>% all
  
  return(list(age_complete = age_complete, 
              years_complete = years_complete, 
              genders_complete = genders_complete))
}

#' check_ages_incomplete
#'
#' @md
#' @examples
#' \dontrun{
#' }
#' @export
check_ages_incomplete <- function(x) {
  age_incomplete <- x$age_check %>%
    magrittr::use_series(present) %>% sum() %>% equals(0)
  
  years_incomplete <- x$years_check %>%
    magrittr::use_series(present) %>% sum() %>% equals(0)
  
  genders_incomplete <- x$gender_check %>% 
    use_series(present) %>% sum() %>% equals(0)
  
  return(list(age_incomplete = age_incomplete, 
              years_incomplete = years_incomplete, 
              genders_incomplete = genders_incomplete))
}

#' check_ages_partial
#'
#' @md
#' @examples
#' \dontrun{
#' }
#' @export
check_ages_partial <- function(x) {
  age_partial <- x$age_check %>%
    magrittr::use_series(present) %>% 
    sum() %>% 
    magrittr::is_less_than(7)
  
  years_partial <- x$years_check %>%
    magrittr::use_series(present) %>% 
    sum() %>% 
    magrittr::is_less_than(17)
  
  genders_partial <- x$gender_check %>% 
    use_series(present) %>% 
    sum() %>% 
    magrittr::is_less_than(34)
  
  return(list(age_partial = age_partial, 
              years_partial = years_partial, 
              genders_partial = genders_partial))
}


#' parse_age_data
#'
#' @md
#' @param filename filename
#' @examples
#' \dontrun{
#' }
#' @export
parse_age_data <- function(filename) {
  
  cat(glue::glue("\n{parse_number(filename)} out of 266 ({round((parse_number(filename)/266)*100, 2)}%)\n"))
  
  raw_dat <- readxl::read_excel(glue::glue("{getwd()}/data/{filename}.xls"), sheet = "Age Group") %>% 
    janitor::clean_names() 
  
  age_checkers <- get_checkers(raw_dat)
  
  is_ages_incomplete <- age_checkers %>% 
    check_ages_incomplete %>% as_vector  %>% all
  
  is_ages_complete <- age_checkers %>% 
    check_ages_complete %>% as_vector %>% all
  
  is_ages_partial <- age_checkers %>% 
    check_ages_partial %>% as_vector  %>% any
  
  if(is_ages_incomplete) {
    file.copy(from = glue::glue("{getwd()}/data/{filename}.xls"), to = glue::glue("{getwd()}/data/no_data/{filename}.xls"))
    if (file.exists(glue::glue("{getwd()}/data/no_data/{filename}.xls"))) file.remove(glue::glue("{getwd()}/data/{filename}.xls"))
  } else if(is_ages_complete) {
    
    cleaned_dat <- raw_dat %>% 
      tidyr::fill(department_of_state, .direction = "down") %>% 
      filter(department_of_state %in% c("Characteristic", complete_ages)) %>% 
      set_names(.[1,]) %>% 
      .[-1,] %>% 
      janitor::clean_names() %>% 
      select_if(.predicate = function(x) is.na(x) %>% all %>% not) %>% 
      gather(key, value, -characteristic) %>% 
      mutate(key = ifelse(str_detect(key, "na"), NA, key)) %>% 
      tidyr::fill(key, .direction = "down") %>% 
      mutate(gender = ifelse(characteristic == "Characteristic", value, NA)) %>% 
      tidyr::fill(gender, .direction = "down") %>%
      filter(not(gender == "Total")) %>%
      filter(not(value %in% c("F", "M"))) %>% 
      mutate(key = parse_number(key)) %>% 
      mutate(value = as.numeric(value)) %>% 
      mutate(cntry = filename)
    
    return(cleaned_dat)
    
  } else if (is_ages_partial) {
    
    simulated_ages <- expand.grid(complete_ages, 2002:2018) %>% 
      bind_rows(., .) %>% mutate(gender = c(rep("F", nrow(.)/2), rep("M", nrow(.)/2))) %>% 
      rename(characteristic = Var1, key = Var2) %>% 
      mutate(characteristic = as.character(characteristic))
    
    cleaned_dat <- raw_dat %>% 
      tidyr::fill(department_of_state, .direction = "down") %>% 
      filter(department_of_state %in% c("Characteristic", complete_ages)) %>% 
      set_names(.[1,]) %>% 
      .[-1,] %>% 
      janitor::clean_names() %>% 
      select_if(.predicate = function(x) is.na(x) %>% all %>% not)  %>% 
      gather(key, value, -characteristic) %>% 
      mutate(key = ifelse(str_detect(key, "na"), NA, key)) %>% 
      tidyr::fill(key, .direction = "down") %>% 
      mutate(gender = ifelse(characteristic == "Characteristic", value, NA)) %>% 
      tidyr::fill(gender, .direction = "down") %>%
      filter(not(gender == "Total")) %>%
      filter(not(value %in% c("F", "M"))) %>% 
      mutate(key = parse_number(key)) %>% 
      mutate(value = as.numeric(value)) %>% 
      full_join(simulated_ages) %>% 
      mutate_all(function(x) ifelse(is.na(x), 0, x)) %>% 
      mutate(cntry = filename) 
    
    return(cleaned_dat)
  }
}
#' parse_religion_data
#'
#' Ggplot2 Age viz
#'
#' @md
#' @param filename filename
#' @return ggplot2 object
#' @examples
#' \dontrun{
#' }
#' @export

parse_religion_data <- function(filename) {
  readxl::read_excel(glue::glue("{getwd()}/data/{filename}.xls"), sheet = "Religion") %>% 
    janitor::clean_names()  %>% 
    tidyr::fill(department_of_state, .direction = "down") %>%
    .[-1:-14,] %>% 
    set_names(.[1,]) %>% 
    .[-1,] %>% 
    janitor::clean_names() %>% 
    select_if(.predicate = function(x) is.na(x) %>% all %>% not) %>% 
    gather(key, value, -religion) %>% 
    filter(str_detect(religion, "Total|Data prior") %>% not) %>% 
    filter(str_detect(key, "cumulative|percent") %>% not) %>% 
    mutate(key = ifelse(str_detect(key, "na"), NA, key)) %>% 
    tidyr::fill(key, .direction = "down") %>% 
    mutate(gender = ifelse(religion == "Religion", value, NA)) %>% 
    tidyr::fill(gender, .direction = "down") %>% 
    filter(not(gender == "Total")) %>%
    filter(not(value %in% c("F", "M"))) %>% 
    mutate(key = parse_number(key)) %>% 
    mutate(value = as.numeric(value)) %>% 
    mutate(cntry = filename)
}

#' parse_education_data
#'
#' Ggplot2 Age viz
#'
#' @md
#' @param filename filename
#' @return ggplot2 object
#' @examples
#' \dontrun{
#' }
#' @export

parse_education_data <- function(filename) {
  readxl::read_excel(glue::glue("{getwd()}/data/{filename}.xls"), sheet = "Education") %>% 
    janitor::clean_names()  %>% 
    tidyr::fill(department_of_state, .direction = "down") %>%
    .[-1:-14,] %>% 
    set_names(.[1,]) %>% 
    .[-1,]  %>% 
    janitor::clean_names() %>% 
    select_if(.predicate = function(x) is.na(x) %>% all %>% not) %>% 
    gather(key, value, -education) %>% 
    filter(str_detect(education, "Total|Data prior") %>% not) %>% 
    filter(str_detect(key, "cumulative|percent") %>% not) %>% 
    mutate(key = ifelse(str_detect(key, "na"), NA, key)) %>% 
    tidyr::fill(key, .direction = "down") %>% 
    mutate(gender = ifelse(education == "Education", value, NA)) %>% 
    tidyr::fill(gender, .direction = "down") %>% 
    filter(not(gender == "Total")) %>%
    filter(not(value %in% c("F", "M"))) %>% 
    mutate(key = parse_number(key)) %>% 
    mutate(value = as.numeric(value))%>% 
    mutate(cntry = filename)
}



