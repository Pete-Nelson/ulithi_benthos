# Peter Nelson
# Institute for Marine Science
# University of California Santa Cruz
# created: 25 March 2023
# purpose: visualization of Ulithi benthic data
# requires: data from 'data manipulation.R'
# modified: 

# UpSet plots ----
library(UpSetR)

# examples ----
movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), 
                   header = T, sep = ";")

# example of list input (list of named vectors)
listInput <- 
  list(one = c(1, 2, 3, 5, 7, 8, 11, 12, 13), 
       two = c(1, 2, 4, 5, 10), 
       three = c(1, 5, 6, 7, 8, 9, 10, 12, 13))

# example of expression input
expressionInput <- 
  c(one = 2, two = 1, three = 2, `one&two` = 1, `one&three` = 4, 
    `two&three` = 1, `one&two&three` = 2)

upset(fromList(listInput), order.by = "freq")

upset(movies,
      nsets = 6,
      number.angles = 30,
      point.size = 3.5,
      line.size = 2,
      mainbar.y.label = "Genre Intersections",
      sets.x.label = "Movies Per Genre",
      text.scale = c(2.5, 1.5, 2, 1.5, 2, 1.3))
