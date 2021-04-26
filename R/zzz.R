.onLoad <- function(libname = find.package("koboloadeR"), pkgname = "koboloadeR") {
  
  
  # CRAN Note avoidance
  if (getRversion() >= "2.15.1")
    utils::globalVariables(
      # used to remove note when doing devtools::check(document = FALSE, args = c('--as-cran'))
      c("V1", "V2", "Var1", "Var2", "aes", "arrange", "as_tibble", "as_vector", "bind_rows",
"case_when", "characteristic", "cntry_selector", "complete_ages",
"complete_genders", "complete_years", "coord_cartesian", "department_of_state",
"desc", "education", "equals", "extract2", "facet_wrap", "filter", "full_join", "gather",
"gender", "geom_area", "geom_hline", "geom_line", "geom_text", "ggplot", "ggtitle", "glue",
"group_by", "guides", "html_attr", "html_nodes", "keep", "key", "labs", "map_lgl", "mutate",
"mutate_all", "na.omit", "not", "parse_number", "perc", "perc2", "perc_label", "present",
"read_html", "religion", "religion_cat", "remDr", "rename", "scale_color_viridis_d",
"scale_fill_viridis_d", "scale_x_continuous", "scale_y_continuous", "select_if",
"set_names", "str_detect", "summarise", "summarize", "tester", "theme", "theme_minimal",
"tibble", "total", "ungroup", "unit", "use_series", "value"
      )
    )
  
  
}


