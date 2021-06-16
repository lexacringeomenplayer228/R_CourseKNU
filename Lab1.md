# Lab 1

### 1. Створити змінні базових (atomic) типів. Базові типи: `character`, `numeric`, `integer`, `complex`, `logical`

```r
var_char <- 'c'
class(var_char)

"character"

var_num <- 1
class(var_num)

"numeric"

var_int <- 1L
class(var_int)

"integer"

var_complex <- complex(real = stats::rnorm(2), imaginary = stats::rnorm(2))
class(var_complex)

"complex"

var_logical <- TRUE
class(var_logical)

"logical"
```

### 2. Створити вектори, які: містить послідовність з 5 до 75; містить числа 3.14, 2.71, 0, 13; 100 значень `TRUE`

```r
vector_a <- c(5:75)
```

5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29  
30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54  
55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75

```r
vector_b <- c(3.14, 2.71, 0, 13)
```

3.14 2.71 0.00 13.00

```r
vector_c <- rep(TRUE, times=100)
```

TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE


### 3. Створити наступну матрицю за допомогою `matrix`, та за допомогою `cbind` або `rbind`

```r
matrix_values <- c(0.5, 3.9, 0,2, 1.3, 131, 2.2, 7, 3.5, 2.8, 4.6, 5.1)
matrix(matrix_values, nrow = 4)

column_1 <- matrix_values[1:4]
column_2 <- matrix_values[5:8]
column_3 <- matrix_values[9:12]
cbind(column_1, column_2, column_3)
```

### 4. Створити довільний список (`list`), в який включити всі базові типи

```r
list(var_char, var_num, var_int, var_complex, var_logical)
```

### 5. Створити фактор з трьома рівнями «baby», «child», «adult»

```r
factor(c('baby', 'child', 'adult'))
```

### 6. Знайти індекс першого значення NA в векторі 1, 2, 3, 4, NA, 6, 7, NA, 9, NA, 11. Знайти кількість значень NA

```r
values <- c(1, 2, 3, 4, NA, 6, 7, NA, 9, NA, 11)
match(NA,values))
sum(is.na(values)))
```

5
3


### 7. Створити довільний `dataframe` та вивести в консоль

```r
dataframe <- data.frame(column_1, column_2)
```

column_1 column_2  
1 0.5 1.3  
2 3.9 131.0  
3 0.0 2.2  
4 2.0 7.0


### 8. Змінити імена стовпців цього `dataframe`

```r
colnames(dataframe) <- c('New1', 'New2')
```

New1 New2  
1 0.5 1.3  
2 3.9 131.0  
3 0.0 2.2  
4 2.0 7.0

