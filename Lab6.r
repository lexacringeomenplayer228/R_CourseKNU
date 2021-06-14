# 1. Створити матрицю mat з 5 стовпцями та 10 строками за допомогою matrix з випадковими даними (функція rnorm(50)).
print('--------------- Task 1 ---------------')

print(mat <- matrix(rnorm(50), nrow = 10))

# 2. Знайти максимальне значення в кожному стовпці.
print('--------------- Task 2 ---------------')

print(apply(mat, 2, max))

# 3. Знайти середнє (mean) значення кожного стовпця.
print('--------------- Task 3 ---------------')

colMeans(mat)
print(apply(mat, 2, mean))

# 4. Знайти мінімальне значення в кожному рядку.
print('--------------- Task 4 ---------------')

print(apply(mat, 1, min))

# 5. Відсортувати кожен стовбець таблиці.
print('--------------- Task 5 ---------------')

print(apply(mat, 2, sort))

# 6. Знайти кількість значень < 0 для кожного стовпця
print('--------------- Task 6 ---------------')

print(apply(mat, 2, function (x) sum(x < 0)))

# 7. Вивести вектор з булевими значеннями TRUE та FALSE. TRUE, якщо в стовпці є елементи >2, FALSE – якщо немає.
print('--------------- Task 7 ---------------')

print(apply(mat, 2, function (x) any(x > 2)))

# 8. Створить список list1 <- list(observationA = c(1:5, 7:3), observationB = matrix(1:6, nrow=2)).
# Для цього списку знайдіть sum за допомогою lapply.
print('--------------- Task 8 ---------------')

list1 <- list(observationA = c(1:5, 7:3), observationB = matrix(1:6, nrow=2))
print(lapply(list1, sum))

# 9. Для кожного елементу списку list1 знайдіть максимальне та мінімальне значення (range) за допомогою lapply та sapply.
print('--------------- Task 9 ---------------')

print(lapply(list1, range))
print(sapply(list1, range))

# 10. Для вбудованого набору даних InsectSprays знайти середнє count для кожного spray.
print('--------------- Task 10 ---------------')

print(aggregate(
  x=list(mean=InsectSprays$count), 
  by=list(spray=InsectSprays$spray), 
  FUN=mean))
