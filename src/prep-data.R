library(kableExtra)
library(tidyverse)
library(janitor)

make_table <- function(dataset)
{
  table <- dataset %>% 
    kable(.) %>%
    kable_styling(fixed_thead = T)
  
  return(table)
}

prep_data <- function(.aggregation, .persistence){
  
  group <- summaries_models %>%
    dplyr::filter(grouped_by_display == .aggregation) %>%
    dplyr::filter(persistence_type_display == .persistence) %>%
    dplyr::distinct(grouped_by) %>% dplyr::pull()
  
  cols <- unlist(strsplit(gsub(" ", "", gsub("academic_year \\| ","",group)), "[|]"))
  
  data <- summaries_models %>%
    dplyr::filter(grouped_by_display == .aggregation) %>%
    dplyr::filter(persistence_type_display == .persistence) %>%
    dplyr::mutate(level_1 = round(level_1,2),
           level_2 = round(level_2,2),
           level_3 = round(level_3,2),
           level_4 = round(level_4,2),
           level_5 = round(level_5,2),
           percent_level_1 = round(percent_level_1,2),
           percent_level_2 = round(percent_level_2),
           percent_level_3 = round(percent_level_3,2),
           percent_level_4 = round(percent_level_4,2),
           percent_level_5 = round(percent_level_5,2)) %>%
    dplyr::select(dplyr::all_of(cols),
                  count,
                  level_1,
                  level_2,
                  level_3,
                  level_4,
                  level_5,
                  count_level_1,
                  percent_level_1,
                  count_level_2,
                  percent_level_2,
                  count_level_3,
                  percent_level_3,
                  count_level_4,
                  percent_level_4,
                  count_level_5,
                  percent_level_5) %>%
    janitor::clean_names(case = "title") 

  return(data)
}


prep_data_2 <- function(.aggregation, .persistence){
  
  data <- all_model %>%
    dplyr::filter(grouped_by_display == .aggregation) %>%
    dplyr::filter(persistence_type_display == .persistence)
  
  return(data)
}