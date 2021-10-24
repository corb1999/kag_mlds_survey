# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# INTRO ====================================================================
metadatar <- list(script_starttime = Sys.time(), 
                  script_det = list(version_dt = as.Date("2021-10-24"), 
                                    author = "corb", 
                                    proj_name = "kag_mlds_survey", 
                                    script_type = "explore", 
                                    notepad = paste0("analyze annual kaggle ml and ds survey results for practice")), 
                  seed_set = 6)
metadatar

# LOAD LIBRARIES **********************************************************
R.version.string
Sys.info()
getwd()
library(lobstr)
library(rlang)
library(tidyverse)
library(tidylog)
library(lubridate)
library(janitor)
library(scales)
library(gt)
set.seed(metadatar$seed_set[1])
options(digits = 4, max.print = 99, warnPartialMatchDollar = TRUE, 
        tibble.print_max = 30, scipen = 999, nwarnings = 5)
mem_used()

# basic helper functions ***************************************************

# function to print object size
sizer <- function(x) {
  aaa <- format(object.size(x), "MB")
  return(aaa)}

# function to quickly run garbage collection
trash <- function(x) {
  gc(verbose = TRUE)}

# function to quickly view a sample of a dataframe
viewer <- function(x) {
  if (is.data.frame(x) == FALSE) {
    print("Error, insert a dataframe")
  } else {
    if(nrow(x) < 95) {
      View(x[sample(1:nrow(x), floor(nrow(x) * 0.5)), ])
    } else {
      View(x[sample(1:nrow(x), 100), ])
    }}}

# a function to make a quick data dictionary of a data frame
data_dictionary <- function(aa) {
  dd <- data.frame(column_order = seq(1, ncol(aa)), 
                   column_name_text = colnames(aa), 
                   column_class = sapply(aa, class, simplify = TRUE), 
                   column_nacount = sapply(lapply(aa, is.na), 
                                           sum, simplify = TRUE), 
                   column_uniques = sapply(lapply(aa, unique), 
                                           length, simplify = TRUE), 
                   row_01 = sapply(aa[1, ], as.character, simplify = TRUE), 
                   row_02 = sapply(aa[2, ], as.character, simplify = TRUE),
                   row_03 = sapply(aa[3, ], as.character, simplify = TRUE),
                   row_04 = sapply(aa[4, ], as.character, simplify = TRUE),
                   row_05 = sapply(aa[5, ], as.character, simplify = TRUE),
                   row.names = NULL)
  return(dd)}

# start the clock timer, used for monitoring runtimes
clockin <- function() {
  aa <- Sys.time()
  clock_timer_start <<- aa
  return(aa)}

# end the clock timer, used in conjunction with the clockin fun
clockout <- function(x) {
  aa <- clock_timer_start
  bb <- Sys.time()
  cc <- bb - aa
  return(cc)}

# helps turn a character dollar variable into numeric
#   requires stringr, uncomment last line to turn NA to zero
cash_money <- function(x) {
  aa <- str_remove_all(x, pattern = "\\$")
  bb <- str_remove_all(aa, pattern = ",")
  cc <- as.numeric(bb)
  # cc <- ifelse(is.na(cc), 0, cc)
  return(cc)}

# POST SCRIPT; alt to using paste0() all the time (i saw this on twitter)
'%ps%' <- function(lhs, rhs) {
  return_me <- paste0(lhs, rhs)
  return(return_me)}

# ^ ====================================
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# unzip and load data ----------------------------------------------------

unzip(zipfile = (getwd() %ps% "/kaggle_survey_2021_responses.csv.zip"), 
      overwrite = TRUE)

loader_path1 <- paste0(getwd(), "/kaggle_survey_2021_responses.csv")
clockin()
raw_df <- read.csv(loader_path1, stringsAsFactors = FALSE)
clockout()
dim(raw_df)

raw_df <- raw_df %>% as_tibble() %>% clean_names()

# cleanup !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ls()
trash()
sizer(raw_df)
obj_size(raw_df)

# ^ -----

# clean data ------------------------------------------------------

colnames(raw_df)
viewer(raw_df)

# take the first row, which contains the text of the question :::::
df_question_txt <- data.frame(column_name = colnames(raw_df), 
                              questiont_txt = as.character(raw_df[1, ]))

# create a function to help parse the salary ranges in the next step :::::
fun_parse_left <- function(x) {
  aa <- str_locate(x, pattern = "-")
  bb <- aa[1, 1]
  cc <- str_subset(x, start = 1L, end = bb)
  return(cc)}

# filter and clean the data
df <- raw_df %>% 
  # eliminate the question row
  filter(time_from_start_to_finish_seconds != 'Duration (in seconds)') %>% 
  # only usa is of interest to me at the moment
  filter(q3 == "United States of America") %>% 
  select(q4, q5, q6, q20, q25) %>% 
  mutate(target_professions = ifelse(q5 %in% c('Product Manager', 
                                               'Machine Learning Engineer', 
                                               'DBA/Database Engineer', 
                                               'Data Scientist', 
                                               'Data Analyst', 
                                               'Business Analyst', 
                                               'Data Engineer'), 
                                     TRUE, FALSE), 
         # start fixing the salar field :::::::::::::::::::::::::
         salary = case_when(q25 == "" ~ "0-0", 
                            q25 == ">$1,000,000" ~ "1000000-1000000", 
                            TRUE ~ q25), 
         salary = str_remove_all(salary, pattern = "\\$"), 
         salary = str_remove_all(salary, pattern = ","), 
         salary_parse = str_split_fixed(salary, pattern = "-", n = 2))

interim <- data.frame(salary_min = as.numeric(df$salary_parse[, 1]), 
                      salary_max = as.numeric(df$salary_parse[, 2]))

df <- cbind(df, interim)

df <- df %>% select(-c(salary_parse)) %>% as_tibble() %>% 
  mutate(salary_midpoint = (salary_min + salary_max) / 2)

# cleanup !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
rm(raw_df, interim)
ls()
trash()
dim(df)
sizer(df)

# ^ -----

# exploratory eda ------------------------------------------------------

df %>% 
  filter(target_professions == TRUE, 
         salary_min > 0) %>% 
  ggplot(aes(x = salary_min)) + 
  geom_histogram(bins = 30, color = "white") + 
  geom_vline(data = df %>% 
               filter(target_professions == TRUE, 
                      salary_min > 0), 
             aes(xintercept = median(salary_min)), 
             color = "red") + 
  labs(caption = "n = " %ps% (df %>% filter(target_professions == TRUE, 
                                            salary_min > 0) %>% nrow()), 
       subtitle = "Selected Professions, Lower Salary Range Histogram")

df %>% 
  filter(target_professions == TRUE, 
         salary_min > 25000, salary_min < 500000) %>% 
  ggplot(aes(x = salary_min, y = q5, color = q5)) + 
  geom_point(alpha = 0.1) + 
  geom_boxplot(varwidth = TRUE, alpha = 0, size = 1) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  labs(y = "", 
       subtitle = "Min salary range, selected professions, 25K-500K only", 
       caption = "n = " %ps% (df %>% 
                                filter(target_professions == TRUE, 
                                       salary_min > 25000, 
                                       salary_min < 500000) %>% nrow()))

df %>% 
  filter(target_professions == TRUE, 
         salary_min > 0) %>% 
  group_by(q5) %>% 
  summarise(responses = n(), 
            mid_sal_med = median(salary_midpoint), 
            mid_sal_q75 = quantile(salary_midpoint, 0.75)) %>% 
  gt() %>% 
  fmt_currency(columns = c(mid_sal_med, mid_sal_q75), currency = "USD", 
               decimals = 0)


# ^ -----
