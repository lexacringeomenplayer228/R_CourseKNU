# Lab 5

### 1. Написати функцію `pmean`, яка обчислює середнє значення (mean) забруднення сульфатами або нітратами серед заданого переліка моніторів

```r
pmean <- function(directory, pollutant, id = 1:332) {
  pollutions_by_id <- get_pollutions(directory, id)
  pollutions <- do.call("rbind", pollutions_by_id)
  mean_pollution <- mean(pollutions[[pollutant]], na.rm=TRUE)
  return(mean_pollution)
}

pmean('specdata', 'sulfate')
```

3.189369


### 2. Написати функцію `complete`, яка виводить кількість повних спостережень (the number of completely observed cases) для кожного файлу

```r
complete <- function(directory, id){
  pollutions <- get_pollutions(directory, id)

  complete_cases_per_id = data.frame(
    id = id,
    nobs = complete_cases_as_vector(pollutions)
  )

  return (complete_cases_per_id)
}

complete('specdata', 1:10)
```

   id nobs  
1   1  117  
2   2 1041  
3   3  243  
4   4  474  
5   5  402  
6   6  228  
7   7  442  
8   8  192  
9   9  275  
10 10  148


### 3. Написати функцію `corr`, яка приймає два аргументи: directory (папка, де знаходяться файли спостережень) та threshold (порогове значення, за замовчуванням дорівнює 0) та обчислює кореляцію між сульфатами та нітратами для моніторів, кількість повних спостережень для яких більше порогового значення

```r
corr <- function(directory, threshold = 0) {
  pollutions <- get_pollutions(directory, 1:332)
  valid_regions <- remove_if_many_na(pollutions, threshold)
  valid_regions_without_na <- remove_rows_with_na(valid_regions)

  if (length(valid_regions_without_na) == 0) { return (c()) }

  correlations <- get_correlations(valid_regions_without_na, 'sulfate', 'nitrate')
  return (correlations)
}

corr('specdata')
```

[1] -0.222552561 -0.018957541 -0.140512544 -0.043897372 -0.068159562  
[6] -0.123506666 -0.075888144 -0.159673652 -0.086841940 0.161379334  
[11] 0.763128837 -0.078813785 -0.545372688 -0.222748447 -0.178085309  
...  
[311] -0.123172036 -0.061565518 -0.180133963 0.458643619 0.728610818  
[316] 0.253978075 0.139867175 0.316429404 0.268780500 0.279397143  
[321] 0.267260662 0.287133842 0.146211226

