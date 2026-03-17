############################
# Import Libraries & Dataset
###########################
library(dplyr)
print(mtcars)


############################
## Style 1: Multiple Objects (Inefficient)
############################

a <- filter(mtcars, carb >2)
b <- group_by(a, cyl)
c <- summarise(b, avg_mpg=mean(mpg) )
d <- filter(c, avg_mpg > 15)

###########################
# Style 2: Nested Functions
###########################

filter(
  summarise(
    group_by(
      filter(
        mtcars, carb>2
      )
    ,cyl
    )
  ,avg_mpg = mean(mpg)
  )
,avg_mpg > 15
)

##########################
# Style 3: Piping Method
##########################

piped_df <- 
mtcars %>% 
  filter(carb > 2) %>% 
  group_by(cyl) %>% 
  summarise(avg_mpg = mean(mpg)) %>% 
  filter(avg_mpg > 15)

print(piped_df)
