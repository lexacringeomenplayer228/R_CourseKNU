# [1, 2, 3, 4] -> [001.csv, 002.csv, ...]
generate_filename <- function (int_values) {
  return (sprintf("%03d.csv", int_values))
}

get_pollutions <- function(directory, regions) {
  setwd(directory)
  files <- generate_filename(regions)
  pollutions_by_region <- lapply(files, read.csv)
  return (pollutions_by_region)
}

# 1. Написати функцію pmean, яка обчислює середнє значення (mean)
# забруднення сульфатами або нітратами серед заданого переліка моніторів

pmean <- function(directory, pollutant, id = 1:332) {
  pollutions_by_id <- get_pollutions(directory, id)
  pollutions <- do.call("rbind", pollutions_by_id)
  mean_pollution <- mean(pollutions[[pollutant]], na.rm=TRUE)
  return(mean_pollution)
}

print("--- pmean")
print(pmean('specdata', 'sulfate'))

complete_cases_as_vector <- function(dataframe) {
  return (sapply(dataframe, function(x) sum(complete.cases(x)) ))
}

# 2. Написати функцію complete, яка виводить кількість повних спостережень
# (the number of completely observed cases) для кожного файлу

complete <- function(directory, id){
  pollutions <- get_pollutions(directory, id)

  complete_cases_per_id = data.frame(
    id = id,
    nobs = complete_cases_as_vector(pollutions)
  )

  return (complete_cases_per_id)
}

print("--- complete")
print(complete('.', 1:10))

remove_if_many_na <- function(dataframe, threshold){
  return (dataframe[lapply(dataframe, function (x) { sum(complete.cases(x)) > threshold}) == TRUE])
}

remove_rows_with_na <- function(dataframe){
  return (lapply(dataframe, function (x) x[complete.cases(x),]))
}

get_correlations <- function(dataframe, col1, col2){
  return (sapply(dataframe, function(x) cor(x[[col1]], x[[col2]]) ))
}

# 3. Написати функцію corr, яка приймає два аргументи: directory
# (папка, де знаходяться файли спостережень) та threshold
# (порогове значення, за замовчуванням дорівнює 0) та обчислює кореляцію
# між сульфатами та нітратами для моніторів, кількість повних спостережень
# для яких більше порогового значення

corr <- function(directory, threshold = 0) {
  pollutions <- get_pollutions(directory, 1:332)
  valid_regions <- remove_if_many_na(pollutions, threshold)
  valid_regions_without_na <- remove_rows_with_na(valid_regions)

  if (length(valid_regions_without_na) == 0) { return (c()) }

  correlations <- get_correlations(valid_regions_without_na, 'sulfate', 'nitrate')
  return (correlations)
}

print("--- corr")
print(corr('.'))
