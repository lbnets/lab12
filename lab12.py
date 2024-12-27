до пункту 3.2 (Python)
# КОРЕЛЯЦІЙНЕ ПОЛЕ
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


data = pd.read_csv('/Users/alinalucickaa/Desktop/pythonMOVD/laba12/mental-illnesses-world.csv',sep=';')


# Фільтруємо дані для України та Польщі
ukraine_data = data[data['Entity'] == 'Ukraine']
poland_data = data[data['Entity'] == 'Poland']


# Об'єднуємо дані по роках
combined_data = pd.merge(ukraine_data, poland_data, on='Year', suffixes=('_Ukraine', '_Poland'))




# Розділяємо дані на три періоди
period1 = combined_data[(combined_data['Year'] >= 1990) & (combined_data['Year'] <= 1999)]
period2 = combined_data[(combined_data['Year'] >= 2000) & (combined_data['Year'] <= 2007)]
period3 = combined_data[(combined_data['Year'] >= 2008) & (combined_data['Year'] <= 2019)]


# Побудова кореляційного поля
plt.figure(figsize=(10, 6))


# Графік для загальної кореляції
plt.subplot(1, 2, 1)
sns.regplot(x='Depressive_disorders_share_of_population_Ukraine',
           y='Depressive_disorders_share_of_population_Poland',
           data=combined_data,
           scatter_kws={'color': 'black'})
plt.title('Загальне кореляційне поле')
plt.xlabel('Частка населення України з депресією (%)')
plt.ylabel('Частка населення Польщі з депресією (%)')


# Графік для трьох періодів
plt.subplot(1, 2, 2)
plt.scatter(period1['Depressive_disorders_share_of_population_Ukraine'],
           period1['Depressive_disorders_share_of_population_Poland'],
           color='red', label='1990–1999 (Трансформація)')
plt.scatter(period2['Depressive_disorders_share_of_population_Ukraine'],
           period2['Depressive_disorders_share_of_population_Poland'],
           color='green', label='2000–2007 (Стабілізація)')
plt.scatter(period3['Depressive_disorders_share_of_population_Ukraine'],
           period3['Depressive_disorders_share_of_population_Poland'],
           color='blue', label='2008–2019 (Криза та війна)')
plt.title('Кореляційне поле за періодами')
plt.xlabel('Частка населення України з депресією (%)')
plt.ylabel('Частка населення Польщі з депресією (%)')
plt.legend()


plt.tight_layout()
plt.show()

from scipy.stats import pearsonr


#КОЕФІЦІЄНТ кореляції
# Вибір змінних для розрахунку
X = combined_data['Depressive_disorders_share_of_population_Ukraine']
Y = combined_data['Depressive_disorders_share_of_population_Poland']


# Розрахунок коефіцієнта кореляції
correlation_coefficient, _ = pearsonr(X, Y)


# Вивід результату
print(f"Коефіцієнт кореляції між Україною та Польщею: {correlation_coefficient:.4f}")


#КОРЕЛЯЦІЙНЕ ВІДНОШЕННЯ
# Розбиваємо X на L інтервалів
L = 4
combined_data['X_group'] = pd.cut(X, bins=L)


# Обчислюємо загальне середнє значення для Y
Y_mean = Y.mean()


# Обчислюємо частинні математичні сподівання для кожної групи
partial_math_spodiv = combined_data.groupby('X_group')['Depressive_disorders_share_of_population_Poland'].mean()


# Обчислюємо групову дисперсію
group_dispers = sum(combined_data['X_group'].value_counts() * (partial_math_spodiv - Y_mean) ** 2) / len(Y)


# Обчислюємо загальну дисперсію
total_dispers = sum((Y - Y_mean) ** 2) / len(Y)


# Обчислюємо кореляційне відношення
сor_relation = group_dispers / total_dispers
print(f"Кореляційне відношення: {сor_relation:.4f}")

# КОРЕЛЯЦІЙНА МАТРИЦЯ
# Обираємо стовпці для побудови матриці
selected_data = combined_data[['Depressive_disorders_share_of_population_Ukraine',
                              'Depressive_disorders_share_of_population_Poland',
                              'Anxiety_disorders_share_of_population_Ukraine',
                              'Anxiety_disorders_share_of_population_Poland',
                              'Bipolar_disorders_share_of_population_Ukraine',
                              'Bipolar_disorders_share_of_population_Poland',
                              'Eating_disorders_share_of_population_Ukraine',
                              'Eating_disorders_share_of_population_Poland'
                              ]]


# Змінюємо назви стовпців
selected_data.rename(columns={
   'Depressive_disorders_share_of_population_Ukraine': 'Українці з депресією',
   'Depressive_disorders_share_of_population_Poland': 'Поляки з депресією',
   'Anxiety_disorders_share_of_population_Ukraine': 'Українці з тривожним розладом',
   'Anxiety_disorders_share_of_population_Poland':'Поляки з тривожним розладом',
   'Bipolar_disorders_share_of_population_Ukraine':'Українці з біполярним розладом',
   'Bipolar_disorders_share_of_population_Poland':'Поляки з біполярним розладом',
   'Eating_disorders_share_of_population_Ukraine':'Українці з розладом харчової поведінки',
   'Eating_disorders_share_of_population_Poland':'Поляки з розладом харчової поведінки'
}, inplace=True)


# Обчислюємо кореляційну матрицю
correlation_matrix = selected_data.corr()


# Виводимо результат
print("Кореляційна матриця:")
print(correlation_matrix)
sns.heatmap(correlation_matrix, annot=True, cmap="coolwarm", fmt=".2f")
plt.title("Кореляційна матриця для України та Польщі")
plt.show()

from statsmodels.graphics.tsaplots import plot_acf
#АВТОКОРЕЛЯЦІЯ
# Вибираємо змінну для аналізу
ukraine_depression = ukraine_data['Depressive_disorders_share_of_population']


# Побудова автокореляційної функції
plot_acf(ukraine_depression, lags=15, title="Автокореляція (частка населення України з депресією)")
plt.show()

# РОЗРАХУНОК КОЕФІЦІЄНТІВ АВТОКОРЕЛЯЦІЇ ВРУЧНУ
autocorrelations_ukr = [ukraine_depression.autocorr(lag) for lag in range(8)]


print("Коефіцієнти автокореляції для України (частка населення з депресією):")
for lag, autocorr in enumerate(autocorrelations_ukr):
   print(f"Lag {lag}: {autocorr:.4f}")


plot_acf(ukraine_depression, lags=8, title="Корелограма для України (частка населення з депресією)")
plt.show()
