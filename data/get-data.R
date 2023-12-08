#get prior goals
library(reshape2)
library(plyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(readr)
library(janitor)
library(ideadata)
library(purrr)


# get persistence goals from file recieved from MJ
# https://ideapublicschoolsorg-my.sharepoint.com/:x:/g/personal/mishan_jensen_ideapublicschools_org/EQ20zfMKWU9GmKlBUepoAJQBEuJ1ri68eYnF7yxATmNzVg?e=zpgxSB
goals <- read.csv("data/2022_2023_Growth_goal_based_on_final_21-22.csv") %>%
  clean_names()



connection_string <- glue::glue(
  "Driver={Sys.getenv('IDEA_RNA_ODBC_DRIVER')};",
  "Server=RGVPDRA-DASQL.IPS.ORG;",
  "Trusted_Connection=yes;",
  "database=Documentation"
)

conn <- odbc::dbConnect(odbc::odbc(), .connection_string = connection_string)

# example to pull data by table name
# results  <- dplyr::tbl(conn,
#                        dbplyr::in_schema(dbplyr::sql("[RGVPDRA-DASQL].[AceAwards].[dbo]"),
#                                          dbplyr::sql("Results"))) %>%
#   collect()


# get last years persistence by grade level from ACE data via SQL
# ACE calculates persistence from boy to last day of school
ldos_persistence_22_23 <- DBI::dbGetQuery(conn,"SELECT DISTINCT [AcademicYear],
    c.[DisplayName] AS [Campus]
	,  REPLACE([Indicator], 'Persistence ','')  AS [Grade]
	,CAST(REPLACE(REPLACE(REPLACE([Indicator], 'Persistence ',''),'PK', -1),'K',0) AS INT) AS [GradeLevelID]
	,[Results] 
  FROM [AceAwards].[dbo].[Results] a
  INNER JOIN 
  [AceAwards].[dbo].[Metrics] b
  ON a.[MetricID] = b.[MetricID]
  INNER JOIN 
[Dashboard].[dbo].[Schools] c
  ON a.SchoolID = c.ID
    WHERE a.[AcademicYear] = '2022-2023'
    AND b.[Source] = 'Persistence'
    AND b.[Indicator] NOT IN ('All Persistence','New Family Persistence')
    AND [Results] <> 0
  ORDER BY c.[DisplayName], CAST(REPLACE(REPLACE(REPLACE([Indicator], 'Persistence ',''),'PK', -1),'K',0) AS INT)") %>%
  clean_names()


fdop_persistence_22_23 <- read.csv("data/2022_2023_fdop.csv") %>%
  mutate(results = results * 100) %>%
  mutate(grade = str_trim(grade, side = c("both"))) %>%
  clean_names()


#join actuals with goal
ldos_persistence_22_23_goal <- goals %>%
  left_join(ldos_persistence_22_23, by =c("school_name" = "campus")) %>%
  select(academic_year = academic_year.y,
         region,
         campus = school_name,
         grade,
         grade_level_id,
         persistence = results,
         goal = x22_23_persistence_goal) %>%
  mutate(met_goal = if_else(persistence >= goal, 1, 0)) %>%
  filter(persistence >= 50) %>%
  mutate(persistence_type = "LDOS")


#join actuals with goal
fdop_persistence_22_23_goal <- goals %>%
  left_join(fdop_persistence_22_23, by =c("school_name" = "campus")) %>%
  select(academic_year = academic_year.y,
         region,
         campus = school_name,
         grade,
         grade_level_id,
         persistence = results,
         goal = x22_23_persistence_goal) %>%
  mutate(met_goal = if_else(persistence >= goal, 1, 0)) %>%
  filter(persistence >= 50) %>%
  filter(grade_level_id != 12) %>%
  mutate(persistence_type = "FDOP") 
  




persistence_22_23_goal <- fdop_persistence_22_23_goal %>%
  union_all(ldos_persistence_22_23_goal)

#create summary 
summarize_groups <- function(.data){
  grps <- glue::glue_collapse(group_vars(.data), sep =" | ")
  
  .data %>% 
    dplyr::summarize(
      count = n(),
      percent_met = mean(met_goal, na.rm = TRUE) *100,
      persistence_mean = mean(persistence, na.rm = TRUE),
      persistence_min = min(persistence, na.rm = TRUE),
      persistence_max = max(persistence, na.rm = TRUE),
      persistence_median = median(persistence, na.rm = TRUE),
      persistence_sd = sd(persistence, na.rm = TRUE),
      goal = mean(goal, na.rm = TRUE)) %>% 
    mutate(grouped_by = grps) 
  
}

persistence_22_23_goal_summary <- persistence_22_23_goal %>% 
  {list(group_by(., academic_year, persistence_type, region, campus), 
        group_by(., academic_year, persistence_type, region, grade), 
        group_by(., academic_year, persistence_type, grade), 
        group_by(., academic_year, persistence_type, region), 
        group_by(., academic_year, persistence_type))} %>% 
  map_df(summarize_groups) %>%
  ungroup() %>%
  select(academic_year,
         region,
         campus,
         grade,
         grouped_by,
         count,
         goal,
         persistence_type,
         percent_met,
         persistence_mean,
         persistence_min,
         persistence_max,
         persistence_median,
         persistence_sd) %>%
  filter(!is.na(academic_year))





summarize_level_percent <- function(.data){
  .data %>% 
    dplyr::summarize(
      count_level_1   = sum(if_else(persistence < level_2,1,0)),
      percent_level_1 = sum(if_else(persistence < level_2 ,1,0))/ n(),
      count_level_2   = sum(if_else(persistence >= level_2 & persistence < level_3,1,0)),
      percent_level_2 = sum(if_else(persistence >= level_2 & persistence < level_3,1,0))/ n(),
      count_level_3   = sum(if_else(persistence >= level_3 & persistence < level_4,1,0)),
      percent_level_3 = sum(if_else(persistence >= level_3 & persistence < level_4,1,0))/ n(),
      count_level_4   = sum(if_else(persistence >= level_4 & persistence < level_5,1,0)),
      percent_level_4 = sum(if_else(persistence >= level_4 & persistence < level_5,1,0))/ n(),
      count_level_5   = sum(if_else(persistence >= level_5,1,0)),
      percent_level_5 = sum(if_else(persistence >= level_5,1,0))/ n())
}

group_by_list <- function (.data, .type){
  if(.type == "sd"){
    return (.data %>% 
              {list(group_by(., academic_year, 
                             region, 
                             campus,
                             grade,
                             grouped_by,
                             persistence_type,
                             count, 
                             goal,
                             percent_met,
                             persistence_mean,
                             persistence_min,
                             persistence_max,
                             persistence_median,
                             persistence_sd,
                             level_1,
                             level_2,
                             level_3,
                             level_4,
                             level_5))})
  }else{
    return (.data %>% 
              {list(group_by(., academic_year, 
                             region, 
                             campus,
                             grade,
                             grouped_by,
                             persistence_type,
                             count,
                             level_1,
                             level_2,
                             level_3,
                             level_4,
                             level_5))})
    
  }
}

select_summary_columns <- function (.data, .type){
  if(.type == "sd"){
    return (.data %>%
              select(academic_year, 
                     region, 
                     campus, 
                     grade, 
                     grouped_by,
                     persistence_type,
                     count, 
                     goal,
                     percent_met,
                     persistence_mean,
                     persistence_min,
                     persistence_max,
                     persistence_median,
                     persistence_sd,
                     level_1,
                     level_2,
                     level_3,
                     level_4,
                     level_5,
                     persistence))
  }else{
    return (.data %>%
              select(academic_year, 
                     region, 
                     campus, 
                     grade, 
                     grouped_by,
                     persistence_type,
                     count,
                     level_1,
                     level_2,
                     level_3,
                     level_4,
                     level_5,
                     persistence))
    
  }
  
}


run_model_w_actuals <- function(.model, .type) {
  

  model_sd_academic_year_region_campus <- .model %>% 
    filter (grouped_by == "academic_year | persistence_type | region | campus") %>%
    inner_join(persistence_22_23_goal , by = c("academic_year" = "academic_year","persistence_type" = "persistence_type","region" ="region","campus"= "campus")) %>%
    select(academic_year, 
           region, 
           campus, 
           grade = grade.y, 
           persistence,
           grouped_by,
           persistence_type,
           level_1,
           level_2,
           level_3,
           level_4,
           level_5)
  
  model_sd_academic_year_region_grade <- .model %>% filter (grouped_by == "academic_year | persistence_type | region | grade") %>%
    inner_join(persistence_22_23_goal , by = c("academic_year" = "academic_year","persistence_type" = "persistence_type","region" ="region","grade"= "grade")) %>%
    select(academic_year, 
           region, 
           campus = campus.y, 
           grade, 
           persistence,
           grouped_by,
           persistence_type,
           level_1,
           level_2,
           level_3,
           level_4,
           level_5)
  
  model_sd_academic_year_grade <- .model %>% filter (grouped_by == "academic_year | persistence_type | grade") %>%
    inner_join(persistence_22_23_goal , by = c("academic_year" = "academic_year","persistence_type" = "persistence_type","grade"= "grade")) %>%
    select(academic_year, 
           region = region.y, 
           campus = campus.y, 
           grade, 
           persistence,
           grouped_by,
           persistence_type,
           level_1,
           level_2,
           level_3,
           level_4,
           level_5)
  
  model_sd_academic_year_region <- .model %>% filter (grouped_by == "academic_year | persistence_type | region") %>%
    inner_join(persistence_22_23_goal  , by = c("academic_year" = "academic_year","persistence_type" = "persistence_type","region"= "region")) %>%
    select(academic_year, 
           region, 
           campus = campus.y, 
           grade = grade.y,
           persistence,
           grouped_by,
           persistence_type,
           level_1,
           level_2,
           level_3,
           level_4,
           level_5)
  
  model_sd_academic_year <- .model %>% filter (grouped_by == "academic_year | persistence_type") %>%
    inner_join(persistence_22_23_goal , by = c("academic_year" = "academic_year","persistence_type" = "persistence_type"))%>%
    select(academic_year, 
           region = region.y,  
           campus = campus.y, 
           grade = grade.y,  
           persistence,
           grouped_by,
           persistence_type,
           level_1,
           level_2,
           level_3,
           level_4,
           level_5)
  
  model <- model_sd_academic_year_region_campus %>%
    union_all(model_sd_academic_year_region_grade) %>%
    union_all(model_sd_academic_year_grade) %>%
    union_all(model_sd_academic_year_region) %>%
    union_all(model_sd_academic_year) %>%
    mutate(level  = case_when ( persistence < level_2 ~ 1,
                                persistence >= level_2 & persistence < level_3 ~ 2,
                                persistence >= level_3 & persistence < level_4 ~ 3,
                                persistence >= level_4 & persistence < level_5 ~ 4,
                                persistence >= level_5 ~ 5))
  
  return (model)
}

run_model_w_actuals_summary <- function(.model, .type) {
  
  summary_model_sd_academic_year_region_campus <- .model %>% 
    filter (grouped_by == "academic_year | persistence_type | region | campus") %>%
    inner_join(persistence_22_23_goal %>% select(-grade, -goal), by = c("academic_year" = "academic_year","persistence_type" = "persistence_type", "region" ="region","campus"= "campus")) %>%
    select_summary_columns(., .type) %>%
    group_by_list(., .type) %>%
    map_df(summarize_level_percent) %>%
    ungroup()
  
  
  summary_model_sd_academic_year_region_grade <- .model %>% filter (grouped_by == "academic_year | persistence_type | region | grade") %>%
    inner_join(persistence_22_23_goal %>% select(-campus, -goal), by = c("academic_year" = "academic_year","persistence_type" = "persistence_type","region" ="region","grade"= "grade")) %>%
    select_summary_columns(., .type) %>%
    group_by_list(., .type) %>%
    map_df(summarize_level_percent) %>%
    ungroup()
  
  summary_model_sd_academic_year_grade <- .model %>% filter (grouped_by == "academic_year | persistence_type | grade") %>%
    inner_join(persistence_22_23_goal %>% select(-region, -campus, -goal), by = c("academic_year" = "academic_year","persistence_type" = "persistence_type","grade"= "grade")) %>%
    select_summary_columns(., .type) %>%
    group_by_list(., .type) %>%
    map_df(summarize_level_percent) %>%
    ungroup()
  
  summary_model_sd_academic_year_region <- .model %>% filter (grouped_by == "academic_year | persistence_type | region") %>%
    inner_join(persistence_22_23_goal %>% select(-campus, -grade, -goal), by = c("academic_year" = "academic_year","persistence_type" = "persistence_type","region"= "region")) %>%
    select_summary_columns(., .type) %>%
    group_by_list(., .type) %>%
    map_df(summarize_level_percent) %>%
    ungroup()
  
  summary_model_sd_academic_year <- .model %>% filter (grouped_by == "academic_year | persistence_type") %>%
    inner_join(persistence_22_23_goal %>% select(-region,-campus, -grade, -goal), by = c("academic_year" = "academic_year","persistence_type" = "persistence_type")) %>%
    select_summary_columns(., .type) %>%
    group_by_list(., .type) %>%
    map_df(summarize_level_percent) %>%
    ungroup()
  
  summary_model <- summary_model_sd_academic_year_region_campus %>%
    union_all(summary_model_sd_academic_year_region_grade) %>%
    union_all(summary_model_sd_academic_year_grade) %>%
    union_all(summary_model_sd_academic_year_region) %>%
    union_all(summary_model_sd_academic_year)
  
  return (summary_model)
}

# model_sd_1 <- persistence_22_23_goal_summary %>%
#   mutate(level_1 = 0,
#          level_2 = goal - (persistence_sd/2),
#          level_3 = goal,
#          level_4 = persistence_mean + (persistence_sd/2),
#          level_5 = persistence_mean + persistence_sd)
# 
# 
# summary_model_sd_1 <- run_model_w_actuals_summary(model_sd_1, "sd")


model_sd_2 <- persistence_22_23_goal_summary %>%
  mutate(level_1 = 0,
         level_2 = goal - (persistence_sd/2),
         level_3 = goal,
         level_4 = goal + (persistence_sd/2),
         level_5 = goal + persistence_sd)


summary_model_sd_2 <- run_model_w_actuals_summary(model_sd_2, "sd")

all_model_sd_2 <- run_model_w_actuals(model_sd_2, "sd")



# knn model where k=2 for persistence actucals less than persistence goal 
# and k=3 where persistence actucals are greater than persistence goal




# get_level_kmeans <- function(.persistence, .k, .item){
#   
#   b <- NA
#   tryCatch(      
#       b <- stats::kmeans(.persistence, centers = .k)$centers %>% sort() %>% .[.item],
#     error = function(e){          
#       b <- NA
#     }
#   )
#   
#   return (b)
# }
# 
# 
# 
# summarize_kmeans_less_goal <- function(.data){
#   grps <- glue::glue_collapse(group_vars(.data), sep =" | ")
#   
#   .data %>% 
#     dplyr::summarize(
#       count = n(),
#       level_1 = 0,
#       level_2 = get_level_kmeans(persistence, 2, 1),
#       level_3 = mean(goal)) %>% 
#     mutate(grouped_by = grps) 
#   
# }
# 
# summarize_kmeans_more_goal <- function(.data){
#   grps <- glue::glue_collapse(group_vars(.data), sep =" | ")
#   
#   .data %>% 
#     dplyr::summarize(
#       count = n(),
#       level_4 = get_level_kmeans(persistence, 3, 2),
#       level_5 = get_level_kmeans(persistence, 3, 3)) %>% 
#     mutate(grouped_by = grps) 
#   
# }
# 
# 
# model_kmeans_less_than_goal_data <- persistence_22_23_goal %>%
#   filter(persistence < goal) %>% 
#   {list(group_by(., academic_year, region, campus), 
#         group_by(., academic_year, region, grade), 
#         group_by(., academic_year, grade), 
#         group_by(., academic_year, region), 
#         group_by(., academic_year))} %>% 
#   map_df(summarize_kmeans_less_goal) %>%
#   ungroup() 
# 
# 
# model_kmeans_greater_than_goal_data <- persistence_22_23_goal %>%
#   filter(persistence >= goal) %>% 
#   {list(group_by(., academic_year, region, campus), 
#         group_by(., academic_year, region, grade), 
#         group_by(., academic_year, grade), 
#         group_by(., academic_year, region), 
#         group_by(., academic_year))} %>% 
#   map_df(summarize_kmeans_more_goal) %>%
#   ungroup() 
# 
# model_kmeans <- model_kmeans_less_than_goal_data %>%
#   inner_join(model_kmeans_greater_than_goal_data, by = c("academic_year"= "academic_year",
#                                                          "grouped_by" = "grouped_by", 
#                                                          "region" = "region",
#                                                          "campus" = "campus",
#                                                          "grade" = "grade")) %>%
#   mutate(count = count.x + count.y) %>%
#   select(-count.x , -count.y)%>%
#   filter( !is.na(level_2) & !is.na(level_4) & !is.na(level_5))
# 
# model_kmeans_summary <- run_model_w_actuals_summary(model_kmeans, "")
# 
# all_model_kmeans <- run_model_w_actuals(model_kmeans, "")


# summary_model_sd_1_clean <- summary_model_sd_1 %>%
#   mutate(model = "Level 2/4/5 Avg SPR +/- SPR SD") %>%
#   select(model,
#          academic_year,
#          region,
#          campus,
#          grade,
#          grouped_by,
#          count,
#          level_1,
#          level_2,
#          level_3,
#          level_4,
#          level_5,
#          count_level_1,
#          percent_level_1,
#          count_level_2,
#          percent_level_2,
#          count_level_3,
#          percent_level_3,
#          count_level_4,
#          percent_level_4,
#          count_level_5,
#          percent_level_5)


summary_model_sd_2_clean <- summary_model_sd_2 %>%
  mutate(model = "Level 2/4/5 Goal +/- SPR SD") %>%
  select(model,
         academic_year,
         region,
         campus,
         grade,
         grouped_by,
         persistence_type,
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
         percent_level_5)

# 
# summary_model_kmeans_clean <- model_kmeans_summary %>%
#   mutate(model = "Level 2/4/5 K means") %>%
#   select(model,
#          academic_year,
#          region,
#          campus,
#          grade,
#          grouped_by,
#          count,
#          level_1,
#          level_2,
#          level_3,
#          level_4,
#          level_5,
#          count_level_1,
#          percent_level_1,
#          count_level_2,
#          percent_level_2,
#          count_level_3,
#          percent_level_3,
#          count_level_4,
#          percent_level_4,
#          count_level_5,
#          percent_level_5)


summaries_models <- summary_model_sd_2_clean %>%
  # dplyr::union_all(summary_model_sd_1_clean) %>%
  # dplyr::union_all(summary_model_kmeans_clean) %>%
  dplyr::mutate(grouped_by_display = case_when( grouped_by == "academic_year | persistence_type | region | campus" ~ "Campuses",
                                                grouped_by == "academic_year | persistence_type | region | grade" ~ "Regional and Grade Level",
                                                grouped_by == "academic_year | persistence_type | grade" ~ "Grade Level",         
                                                grouped_by == "academic_year | persistence_type | region" ~ "Regional",        
                                                grouped_by == "academic_year | persistence_type" ~ "Org-wide")) %>%
  dplyr::mutate(persistence_type_display = case_when( persistence_type == "FDOP" ~ "First Day of Persistience",
                                                      persistence_type == "LDOS" ~ "Last Day of School")) %>%
  
  dplyr::filter(!is.na(level_2)) %>%
  dplyr::ungroup()


all_model_sd_2_clean <- all_model_sd_2 %>%
  mutate(model = "Level 2/4/5 Goal +/- SPR SD") %>%
  select(model,
         academic_year,
         region,
         campus,
         grade,
         persistence,
         grouped_by,
         persistence_type,
         level_1,
         level_2,
         level_3,
         level_4,
         level_5,
         level)

# all_model_kmeans_clean <- all_model_kmeans %>%
#   mutate(model = "Level 2/4/5 K means") %>%
#   select(model,
#          academic_year,
#          region,
#          campus,
#          grade,
#          persistence,
#          grouped_by,
#          persistence_type,
#          level_1,
#          level_2,
#          level_3,
#          level_4,
#          level_5,
#          level)

all_model <- all_model_sd_2_clean %>%
  # union_all(all_model_kmeans_clean) %>%
  dplyr::mutate(grouped_by_display = case_when( grouped_by == "academic_year | persistence_type | region | campus" ~ "Campuses",
                                                grouped_by == "academic_year | persistence_type | region | grade" ~ "Regional and Grade Level",
                                                grouped_by == "academic_year | persistence_type | grade" ~ "Grade Level",         
                                                grouped_by == "academic_year | persistence_type | region" ~ "Regional",        
                                                grouped_by == "academic_year | persistence_type" ~ "Org-wide")) %>%
  dplyr::mutate(persistence_type_display = case_when( persistence_type == "FDOP" ~ "First Day of Persistience",
                                                      persistence_type == "LDOS" ~ "Last Day of School")) %>%
  
  select(model,
         academic_year,
         region,
         campus,
         grade,
         persistence,
         persistence_type,
         persistence_type_display,
         grouped_by,
         grouped_by_display,
         level_1,
         level_2,
         level_3,
         level_4,
         level_5,
         level)



summaries_models_summary <- summaries_models %>%  
  dplyr::select(model,
                persistence_type,
                persistence_type_display,
                grouped_by,
                grouped_by_display,
                count,
                count_level_1,
                count_level_2,
                count_level_3,
                count_level_4,
                count_level_5) %>%
  dplyr::group_by(model,
                  persistence_type,
                  persistence_type_display,
                  grouped_by,
                  grouped_by_display) %>%
  dplyr::summarise(count_level_1 = sum(count_level_1, na.rm = TRUE),
                   count_level_2 = sum(count_level_2, na.rm = TRUE),
                   count_level_3 = sum(count_level_3, na.rm = TRUE),
                   count_level_4 = sum(count_level_4, na.rm = TRUE),
                   count_level_5 = sum(count_level_5, na.rm = TRUE),
                   campuses = count_level_1 + count_level_2 + count_level_3 + count_level_4 + count_level_5) %>%
  dplyr::ungroup()

write.csv(summaries_models, "data/summaries_models.csv", row.names = FALSE)  
write.csv(summaries_models_summary, "data/summaries_models_summary.csv", row.names = FALSE)  
write.csv(all_model, "data/all_model.csv", row.names = FALSE) 


#https://stackoverflow.com/questions/60992750/how-to-add-summary-statistics-in-histogram-plot-using-ggplot2