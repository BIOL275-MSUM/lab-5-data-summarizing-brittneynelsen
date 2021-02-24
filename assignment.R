
# load packages -----------------------------------------------------------

library(tidyverse)

# print data --------------------------------------------------------------

iris
iris <- as_tibble(iris)
iris

# question 1 --------------------------------------------------------------

iris_r <- rename(
  iris, 
  sepal_length = Sepal.Length, 
  sepal_width = Sepal.Width, 
  petal_length = Petal.Length,
  petal_width = Petal.Width,
  species = Species
)

iris_r

# question 2 --------------------------------------------------------------

iris_cm <- mutate(iris_r, sepal_length = sepal_length * 10,
       sepal_width = sepal_width * 10,
       petal_length = petal_length * 10,
       petal_width = petal_width * 10)

iris_cm

# question 3 --------------------------------------------------------------

iris_area <- mutate(iris_cm, sepal_area = sepal_length * sepal_width, 
                    petal_area = petal_length * petal_width) %>%
  select(petal_area, sepal_area, species)
iris_area
       
  
#  question 4 -------------------------------------------------------------

summarize(iris, sample_size = n(), 
          min = min(Sepal.Length), 
          max = max(Sepal.Length), 
          range = max(Sepal.Length) - min(Sepal.Length), 
          median = median(Sepal.Length),
          q1 = quantile(Sepal.Length, probs = 0.25), 
          q2 = quantile(Sepal.Length, probs = 0.75), IQR = IQR(Sepal.Length))


#  question 5 -------------------------------------------------------------

iris_sum <- 
  iris %>%
  group_by(Species) %>%
  summarize(
  mean_w = mean(Petal.Width),
  sample_size = n(),
  sd = sd(Petal.Width),
  var = var(Petal.Width),
  sem = mean(Petal.Width) / sqrt(sample_size),
  ci_upper = mean_w + 2 * sem,
  ci_lower = mean_w - 2 * sem
)

iris_sum
#  question 6 -------------------------------------------------------------

ggplot(data = iris) +
  geom_jitter(mapping = aes(x = Species, y = Petal.Width))


#  question 7 -------------------------------------------------------------

ggplot(data = iris) +
  geom_jitter(mapping = aes(x = Species, y = Petal.Width)) +
  geom_crossbar(
    data = iris_sum, 
    mapping = aes(x = Species, y = mean_w, ymax = ci_upper, ymin = ci_lower),
    color = "red"
  )

#  question 8 -------------------------------------------------------------

ggplot(data = iris) +
  geom_point(mapping = aes(x = Petal.Length, y = Petal.Width))
ggplot(data = iris) +
  geom_point(mapping = aes(x = Petal.Length, y = Petal.Width, color = Species), alpha = 0.3)





