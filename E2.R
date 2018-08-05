library(ggplot2)
library(dplyr)
library(readr)
data("diamonds")
Diamonds <- read_csv("MATH2270 - Data Visualisation/Diamonds.csv")

Diamonds$cut<- factor(Diamonds$cut, levels=c('Fair','Good','Very Good','Premium','Ideal'), 
                      ordered=TRUE)
Diamonds$color <- factor(Diamonds$color, levels=c('J','I','H','G','F','E','D'), 
                        ordered=TRUE)
Diamonds$clarity<- factor(Diamonds$clarity, levels=c('I1','SI2','SI1','VS2','VS1','VVS2','VVS1','IF'), 
                          ordered=TRUE)
# Group carat into 5 chunks so as to generate a succinct visualisation. 
Diamonds <- mutate(Diamonds, 
                   Carat_Group = 
                     case_when(carat <=1 & carat >0 ~ "Wgt_1",
                               carat <=2 & carat >1 ~ "Wgt_2",
                               carat <=3 & carat >2 ~ "Wgt_3",
                               carat <=4 & carat >3 ~ "Wgt_4",
                               carat >4 ~ "Wgt_5"))
# Initial letter of Color Grade
Diamonds <- mutate(Diamonds, 
                   Cut_1st_letter = 
                     case_when(cut == 'Fair' ~ "F",
                               cut == 'Good' ~ "G",
                               cut == 'Very Good' ~ "VG",
                               cut == 'Premium' ~ "P",
                               cut == 'Ideal' ~ "I"))

Diamonds$Cut_1st_letter <- factor(Diamonds$Cut_1st_letter, levels=c('F','G','VG','P','I'), 
                                  ordered=TRUE)
Diamonds
p <- ggplot(Diamonds, aes(Cut_1st_letter, price)) + geom_violin() 
p + facet_grid(Carat_Group ~ clarity) + theme_bw()


class(Diamonds$carat)
