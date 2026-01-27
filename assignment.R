# установите и загрузите пакеты

library(friends)
library(tidyverse)
library(tidytext)
library(factoextra) 


# 1. отберите 6 главных персонажей (по количеству реплик)
# сохраните как символьный вектор
top_speakers <- friends |> 
  count(speaker, sort = TRUE) |> 
  slice_head(n = 6) |> 
  pull(speaker) |> 
  as.character() 
  
# 2. отфильтруйте топ-спикеров, 
# токенизируйте их реплики, удалите из них цифры
# столбец с токенами должен называться word
# оставьте только столбцы speaker, word
friends_tokens <- friends |> 
  filter(speaker %in% top_speakers) |> 
  unnest_tokens(word, text) |> 
  mutate(word = str_remove_all(word, "\\d")) |>  
  filter(word != "") |> 
  select(speaker, word)

# 3. отберите по 500 самых частотных слов для каждого персонажа
# посчитайте относительные частотности для слов
friends_tf <- friends_tokens |>
  count(speaker, word, name = "n") |> 
  group_by(speaker) |> 
  slice_max(n, n = 500) |> 
  mutate(
    total_words = sum(n),          
    tf = n / total_words) |>  
  ungroup() |> 
  arrange(speaker) |> 
  select(speaker, word, tf)
  
# 4. преобразуйте в широкий формат; 
# столбец c именем спикера превратите в имя ряда, используя подходящую функцию 
friends_tf_wide <- friends_tf |> 
  pivot_wider(names_from = "word", 
              values_from = "tf", 
              values_fill = 0)

friends_tf_wide

friends_tf_wide <- friends_tf_wide |> 
  column_to_rownames(var = "speaker")

# 5. установите зерно 123
# проведите кластеризацию k-means (k = 3) на относительных значениях частотности (nstart = 20)
# используйте scale()

set.seed(123)
km.out <- kmeans(scale(friends_tf_wide), centers = 3, nstart = 20)

km.out$cluster


# 6. примените к матрице метод главных компонент (prcomp)
# центрируйте и стандартизируйте, использовав аргументы функции
pca_fit <- # ваш код здесь

# 7. Покажите наблюдения и переменные вместе (биплот)
# в качестве геома используйте текст (=имя персонажа)
# цветом закодируйте кластер, выделенный при помощи k-means
# отберите 20 наиболее значимых переменных (по косинусу, см. документацию к функции)
# сохраните график как переменную q

q <- # ваш код здесь


# Последнее обновление: Wed Jan 28 01:04:07 MSK 2026
