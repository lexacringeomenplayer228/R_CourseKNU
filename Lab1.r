# 1. Створити змінні базових (atomic) типів. Базові типи: character, numeric, integer, complex, logical
print('--------------- Task 1 ---------------')

var_char <- 'c'
print(class(var_char))

var_num <- 1
print(class(var_num))

var_int <- 1L
print(class(var_int))

var_complex <- complex(real = stats::rnorm(2), imaginary = stats::rnorm(2))
print(class(var_complex))

var_logical <- TRUE
print(class(var_logical))

# 2. Створити вектори, які: містить послідовність з 5 до 75; містить числа 3.14, 2.71, 0, 13; 100 значень TRUE
print('--------------- Task 2 ---------------')

vector_a <- c(5:75)
print(vector_a)

vector_b <- c(3.14, 2.71, 0, 13)
print(vector_b)

vector_c <- rep(TRUE, times=100)
print(vector_c)

# 3. Створити наступну матрицю за допомогою matrix, та за допомогою cbind або rbind
print('--------------- Task 3 ---------------')

matrix_values <- c(0.5, 3.9, 0,2, 1.3, 131, 2.2, 7, 3.5, 2.8, 4.6, 5.1)
matrix(matrix_values, nrow = 4)

column_1 <- matrix_values[1:4]
column_2 <- matrix_values[5:8]
column_3 <- matrix_values[9:12]
cbind(column_1, column_2, column_3)

print('Done')

# 4. Створити довільний список (list), в який включити всі базові типи
print('--------------- Task 4 ---------------')

list(var_char, var_num, var_int, var_complex, var_logical)

print('Done')

# 5. Створити фактор з трьома рівнями «baby», «child», «adult»
print('--------------- Task 5 ---------------')

factor(c('baby', 'child', 'adult'))

print('Done')

# 6. Знайти індекс першого значення NA в векторі 1, 2, 3, 4, NA, 6, 7, NA, 9, NA, 11. Знайти кількість значень NA
print('--------------- Task 6 ---------------')

values <- c(1, 2, 3, 4, NA, 6, 7, NA, 9, NA, 11)
print(match(NA,values))
print(sum(is.na(values)))

# 7. Створити довільний data frame та вивести в консоль
print('--------------- Task 7 ---------------')

dataframe <- data.frame(column_1, column_2)
print(dataframe)

# 8. Змінити імена стовпців цього data frame
print('--------------- Task 8 ---------------')

colnames(dataframe) <- c('New1', 'New2')
print(dataframe)

