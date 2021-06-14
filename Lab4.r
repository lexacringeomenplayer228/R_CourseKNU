dataframe = read.csv("hw1_data.csv")

# 1. Які назви стовпців файлу даних?
print('--------------- Task 1 ---------------')

print(names(dataframe))

# 2. Виведіть перші 6 строк фрейму даних
print('--------------- Task 2 ---------------')

print(dataframe[1:6,])

# 3. Скільки спостерігань (строк) в дата фреймі?
print('--------------- Task 3 ---------------')

print(nrow(dataframe))

# 4. Виведіть останні 10 строк дата фрейму
print('--------------- Task 4 ---------------')

print(tail(dataframe, 10))

# 5. Як багато значень «NA» в стовпці «Ozone»?
print('--------------- Task 5 ---------------')

print(sum(is.na(dataframe$'Ozone')))

# 6. Яке середнє (mean) стовпця «Ozone». Виключити «NA» значення
print('--------------- Task 6 ---------------')

print(mean(dataframe$'Ozone',na.rm=TRUE))

# 7. Виведіть частину набору даних (subset) зі значенням «Ozone» > 31
# та «Temp» > 90. Яке середнє (mean) значень «Solar.R» в цьому наборі даних (subset)?
print('--------------- Task 7 ---------------')

subset = subset(dataframe, dataframe$'Ozone' > 31 & dataframe$'Temp' > 90)
print(mean(subset$'Solar.R'))

# 8. Яке середнє значення (mean) для «Temp» для червня («Month» дорівнює 6)?
print('--------------- Task 8 ---------------')

june = subset(dataframe, dataframe$'Month' == 6)
print(mean(june$'Temp'))

# 9. Яке максимальне значення «Ozone» для травня («Month» дорівнює 5)?
print('--------------- Task 9 ---------------')

may = subset(dataframe, dataframe$'Month' == 5)
print(max(may$'Ozone', na.rm=TRUE))
