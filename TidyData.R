library(tidyverse)
library(ds4ling)

test_scores_rm

select(test_scores_rm, id, test2)

select(test_scores_rm, spec:test1) #: is a range

select(test_scores_rm, participant = id)#rename columns

glimpse(mtcars)

select(mtcars, wt, drat, gear)

select(mtcars, am:carb)

select(mtcars, hello_world = mpg)

filter(mtcars, mpg <20, mpg >14)
filter(mtcars, mpg <20 & mpg >14)

filter(mtcars, cyl == 6)

filter(mtcars, mpg >20 | disp < 200)

filter(test_scores_rm, spec =="gq_lo")

mtcars %>% 
    select(mpg, disp) %>% 
    filter(mpg >20) %>% 
    ggplot()+
    aes(x = disp, y = mpg) +
    geom_point()

#mutate
select(mtcars, mpg) %>% 
  mutate(mpgx2 = mpg*2)

mtcars %>% 
  select(mpg) %>% 
  mutate(
    mpg_x2 = mpg * 2,
    mpg_c = mpg - mean(mpg),
    value = if_else (mpg<=18, "bad", "good")
  )

#group_by() + summarize()

mtcars %>% 
  summarize(avg=mean(mpg))

mean(mtcars$mpg)#first gives an integer, second a vector

mtcars %>% 
  group_by(cyl) %>% 
  summarize(avg = mean(mpg))

mtcars %>% 
  group_by(cyl) %>% 
  summarize(
    avg = mean(mpg),
    sd = sd(mpg),
    min = min(mpg),
    max= max(mpg)
    )
#
#tidyr February 20 2023
#https://www.osrrl.jvcasillas.com/slides/06_tv2/index.html#44

#separate
test_scores_rm %>% 
  separate(col = id, into = c("lang", "trash"), sep = 4, remove = FALSE) %>% 
  select(-trash) %>% 
  separate(col = spec, into = c("group", "level"), sep = "_")

  
#play with UNITE at home

scores_long <- test_scores_rm %>%
  pivot_longer (
    cols = test1:test2, 
    names_to = "test",
    values_to = "scores"
    )

scores_long %>% 
  ggplot ()+
  aes (x = test, y = scores) +
  geom_boxplot()

test_scores_rm %>% 
  ggplot()+
  aes(x = test1, y = test2) +
  geom_point() +
  geom_smooth(method = "lm")      

## fit a model with the lm() function

mod <- lm (mpg ~ wt, data=mtcars)
summary(mod1)
