# 1. Функція add2(x, y), яка повертає суму двох чисел
print('--------------- Task 1 ---------------')

add2 <- function(x,y) {
  return (x+y)
}

print(add2(5, 2))

# 2. Функція above(x, n), яка приймає вектор та число n, та повертає
# всі елементі вектору, які більше n. По замовчуванню n = 10.
print('--------------- Task 2 ---------------')

above <- function(x,n=10) {
  return (x[x > n])
}

test_vector <- c(4:15)
print(above(test_vector))
print(above(test_vector, 5))
print(above(test_vector, 100))

# 3. Функція my_ifelse(x, exp, n), яка приймає вектор x, порівнює всі
# його елементи за допомогою exp з n, та повертає елементи вектору, які
# відповідають умові expression. Наприклад, my_ifelse(x, “>”, 0) повертає
# всі елементи x, які більші 0. Exp може дорівнювати “<”, “>”, “<=”, “>=”, “==”.
# Якщо exp не співпадає ні з одним з цих виразів, повертається вектор x.
print('--------------- Task 3 ---------------')

my_ifelse <- function(x,expression,n) {
  if (expression == '>')  return (x[x>n])
  if (expression == '<')  return (x[x<n])
  if (expression == '>=') return (x[x>=n])
  if (expression == '<=') return (x[x<=n])
  if (expression == '==') return (x[x==n])
  return (x)
}

print(my_ifelse(test_vector, '>=' ,12))

# 4. Функція columnmean(x, removeNA), яка розраховує середнє значення (mean)
# по кожному стовпцю матриці, або data frame. Логічний параметр removeNA
# вказує, чи видаляти NA значення. По замовчуванню він дорівнює TRUE.
print('--------------- Task 4 ---------------')

columnmean <- function(x,removeNA=TRUE) {
  apply(x, 2, function (column) mean(column, na.rm = removeNA))
}

test_matrix = cbind(c(1, 1, 4, NA, 1), c(1, NA, 1, 4, 4), c(0, 1, 1, 1, 1))

print(columnmean(test_matrix))
print(columnmean(test_matrix, FALSE))
