library(stringr)

read_file <- function(file_path){
  # 1. Зчитати файл read.csv("olympics.csv", skip = 1, header = TRUE, encoding="UTF-8", stringsAsFactors = FALSE)
  return (read.csv(file_path, skip = 1, header = TRUE, encoding="UTF-8", stringsAsFactors = FALSE))
}

remove_invalid_rows <- function(dataframe){
  dataframe <- remove_totals(dataframe)
  return (dataframe)
}

remove_totals <- function(dataframe){
  return (dataframe[!dataframe[1] == "Totals", ])
}

rename_columns <- function(dataframe){
  for (col_name in colnames(dataframe))
    names(dataframe)[names(dataframe) == col_name] <- get_new_column_name(col_name) 
  return (dataframe)
}

# 1. Напишіть функцію prepare_set <- function(file_name) {} яка в якості
# аргументу приймає ім’я файлу і повертає дата фрейм

prepare_set <- function(file_path){
  dataframe <- read_file(file_path)
  dataframe <- remove_invalid_rows(dataframe)
  dataframe <- rename_columns(dataframe)
  dataframe <- add_country_id(dataframe)
  dataframe <- trim_county_names(dataframe)
  return (dataframe)
}

get_new_column_name <- function(old_col_name){
  # 1.2. Першому стовпцю дати назву "Country"
  if (old_col_name == 'X') return ('Country')

  # 'X..' -> ''
  if (grepl('X\\.\\.', old_col_name)) return (gsub(pattern = 'X\\.\\.',replacement = '',x = old_col_name))

  # 'X01..' -> 'Gold'
  if (grepl('X01\\.\\.', old_col_name)) return (gsub(pattern = 'X01\\.\\.',replacement = 'Gold',x = old_col_name))

  # 'X02..' -> 'Silver'
  if (grepl('X02\\.\\.', old_col_name)) return (gsub(pattern = 'X02\\.\\.',replacement = 'Silver',x = old_col_name))

  # 'X03..' -> 'Bronze'
  if (grepl('X03\\.\\.', old_col_name)) return (gsub(pattern = 'X03\\.\\.',replacement = 'Bronze',x = old_col_name))

  return (old_col_name)
}

add_country_id <- function(dataframe, column_name = 'Country'){
  dataframe['ID'] <- str_match(dataframe[[column_name]], '(.*\\()([A-Z0-9]{3})(\\).*)')[,3]
  return (dataframe)
}

trim_county_names <- function(dataframe, column_name = 'Country'){
  dataframe[[column_name]] <- str_match(dataframe[[column_name]], '([A-Za-z\\s]*)(\\s\\(.*)')[,2]
  return (dataframe)
}

print("--------------- First Part ---------------\n")

olympics <- prepare_set("olympics.csv")

first_answer <- function(dataframe){
  return (dataframe[dataframe$'Gold' == max(dataframe$'Gold'),'Country'])
}

print("--------------- Answer 1 ---------------\n")
print(first_answer(olympics))

second_answer <- function(dataframe){
  diff <- get_abs_summer_winter_diff(dataframe)
  country_with_max_diff <- dataframe[diff == max(diff),'Country']
  return (country_with_max_diff)
}

print("--------------- Answer 2 ---------------\n")
print(second_answer(olympics))

get_abs_summer_winter_diff <- function(dataframe){
  return (abs(dataframe$'Total' - dataframe$'Total.1'))
}

third_answer <- function(dataframe){
  dataframe <- filter_losers(dataframe)
  diff <- get_rel_summer_winter_diff(dataframe)
  country_with_max_diff <- dataframe[diff == max(diff),'Country']
  return (country_with_max_diff)
}

print("--------------- Answer 3 ---------------\n")
print(third_answer(olympics))

filter_losers <- function(dataframe){
  return (dataframe[dataframe$'Gold' > 0 &dataframe$'Gold.1' > 0,])
}

get_rel_summer_winter_diff <- function(dataframe){
  rel_diff <- (dataframe$'Gold' - dataframe$'Gold.1') / dataframe$'Gold.2'
  return (rel_diff)
}

fourth_answer <- function(dataframe){
  points <- get_points(dataframe)
  return (data.frame(Country=dataframe$'Country', Points=points))
}

print("--------------- Answer 4 ---------------\n")
print(fourth_answer(olympics))

get_points <- function(dataframe, gold_reward = 3, silver_reward = 2, bronze_reward = 1){
  gold_points = dataframe$'Gold.2'*gold_reward
  silver_points = dataframe$'Silver.2'*silver_reward
  bronze_points = dataframe$'Bronze.2'*bronze_reward

  total_points = gold_points + silver_points + bronze_points
  return (total_points)
}

print("--------------- Second Part ---------------")

census_file_path <- "census.csv"
census <- read.csv(census_file_path, stringsAsFactors = FALSE)

fifth_answer <- function(dataframe){
  counties_count_by_state <- get_counties_count_by_state(dataframe)
  max_counties <- max(counties_count_by_state$'COUNTY')
  state_with_max_counties <- counties_count_by_state[counties_count_by_state$'COUNTY' == max_counties, 'STNAME']
  return (state_with_max_counties)
}

print("--------------- Answer 5 ---------------\n")
print(fifth_answer(census))

get_counties_count_by_state <- function(dataframe){
  return (aggregate(COUNTY ~ STNAME, dataframe, function(x) length(unique(x))))
}

sixth_answer <- function(dataframe){
  dataframe <- dataframe[ dataframe$SUMLEV != 040 ,]
  pop_by_state <- get_population_by_state(dataframe)
  sorted_pop_by_state <- pop_by_state[order(-pop_by_state[,'CENSUS2010POP']), ]
  return (sorted_pop_by_state[1:3,'STNAME'])
}

print("--------------- Answer 6 ---------------\n")
print(sixth_answer(census))

get_population_by_state <- function(dataframe){
  return (aggregate(CENSUS2010POP ~ STNAME, dataframe, function (state) get_population_in_largest_counties(state) ))
}

get_population_in_largest_counties <- function(state, largest_counties_number = 3){
  return (sum(sort(state, decreasing = TRUE)[1:largest_counties_number]))
}

seventh_answer <- function(dataframe){
  dataframe <- dataframe[ dataframe$SUMLEV != 040 ,]
  changes_by_county <- get_pop_changes_by_county(dataframe)
  sorted_changes_by_county <- changes_by_county[order(-changes_by_county[,'range']), ]
  return(sorted_changes_by_county[1,'CTYNAME'])
}

print("--------------- Answer 7 ---------------\n")
print(seventh_answer(census))

get_pop_changes_by_county <- function(dataframe){
  columns = c("POPESTIMATE2010",
              "POPESTIMATE2011",
              "POPESTIMATE2012",
              "POPESTIMATE2013",
              "POPESTIMATE2014",
              "POPESTIMATE2015" )
  dataframe$range <- apply(dataframe, 1, function (county) get_county_changes_range(county[columns]))
  return (dataframe[c('CTYNAME','range')])
}

get_county_changes_range <- function(county){
  estimates <- as.numeric(county)
  return (max(estimates) - min(estimates))
}

eighth_answer <- function(dataframe){
  filtered <- dataframe[
    ( dataframe$REGION == '1' | dataframe$REGION == '2' )
    & (grepl('^Washington', dataframe$CTYNAME))
    & (as.numeric(dataframe$POPESTIMATE2015) > as.numeric(dataframe$POPESTIMATE2014))
  ,]

  return (filtered[c("STNAME", "CTYNAME")])
}

print("--------------- Answer 8 ---------------\n")
print(eighth_answer(census))
