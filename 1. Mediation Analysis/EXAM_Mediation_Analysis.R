# library
library(tidyverse) # dplyr을 포함하여 다양한 함수가 포함되어 있는 라이브러리
library(knitr) # 마크다운 형식으로 표를 쉽게 작성하기 위한 라이브러리
library(lavaan) # 요인분석, 구조방정식에 사용되는 라이브러리
library(psych) # mediate 함수를 지원하는 라이브러리
library(MBESS) # 

# data setting
fdir <- 'C:/Users/Hojin Jeong/Documents/GitHub/psycho_anal_with_R'
setwd(fdir)
gaming_data <- "1. Mediation Analysis/Mediation_Data_Gaming_Addiction.csv" %>%
  read_csv()

#data summary
gaming_data %>%
  headTail() %>%
  kable(digits = 2, format = "markdown")

# visualize relationship
gaming_data %>% 
select('InterRelationship.total', 'Mot.aggresive', 'Mot.escapism', 'Mot.achivement', 'K_scale_sum') %>% 
  pairs.panels()

# write model
mod1 <- "# a path
         Mot.escapism ~ a1 * InterRelationship.total
         Mot.aggresive ~ a2 * InterRelationship.total
         Mot.achivement ~ a3 * InterRelationship.total
         

         # b path
         K_scale_sum ~ b1 * Mot.escapism
         K_scale_sum ~ b2 * Mot.aggresive
         K_scale_sum ~ b3 * Mot.achivement

         # c prime path 
         K_scale_sum ~ cp * InterRelationship.total

         # indirect and total effects
         a1b1 := a1 * b1
         a2b2 := a2 * b2
         a3b3 := a3 * b3
         total := cp + a1b1 + a2b2 + a3b3"

# set random seed so results can be reproduced
set.seed(1234)

# fit model
fsem1 <- sem(mod1, data = gaming_data, se = "bootstrap", bootstrap = 5000)
summary(fsem1, standardized = TRUE)

# print all model parameters
parameterestimates(fsem1, boot.ci.type = "bca.simple", standardized = TRUE) %>% 
  kable(digits = 2, format = "markdown")

# Test same model using mediation() from MBESS
with(gaming_data, mediations(x = InterRelationship.total, mediator = Mot.escapism, dv = K_scale_sum,
                           bootstrap = TRUE, which.boot = "BCa", B = 5000))


# Test the same model using mediate() from psych
mediate(K_scale_sum ~ InterRelationship.total + (Mot.escapism) + (Mot.aggresive) + (Mot.achivement), data = gaming_data, n.iter = 10000) %>%
  print(short = FALSE)