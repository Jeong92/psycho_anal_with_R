# library
library(tidyverse) # dplyr을 포함하여 다양한 함수가 포함되어 있는 라이브러리
library(knitr) # 마크다운 형식으로 표를 쉽게 작성하기 위한 라이브러리
library(psych) # psych 라이브러리
library(ggplot2) # 시각화를 위한 라이브러리리
library(stargazer)
library(rockchalk) # visualization

# data setting
fdir <- 'C:/Users/goran/Documents/GitHub/psycho_anal_with_R'
setwd(fdir)
gaming_data <- "./2. Moderation Analysis/Moderation_Data_Gaming_Addiction.csv" %>%
  read_csv()

#data summary
gaming_data %>%
  headTail() %>%
  kable(digits = 2, format = "markdown")

# 자료 확인
str(gaming_data)

# type change: num → factor
gaming_data <- gaming_data %>%
  mutate(Sex = factor(Sex, unique(Sex), labels = c("Male", "Female")))

# visualize relationship
gaming_data %>% 
  select(Mot.escapism, Impulsivity.sum, K_scale_sum) %>% 
  pairs.panels(lm=TRUE, stars=TRUE)

# visualize relationship for male
gaming_data %>% 
  filter(Sex == "Male") %>%
  select(Mot.escapism, Impulsivity.sum, K_scale_sum) %>% 
  pairs.panels(lm=TRUE, stars=TRUE)

# visualize relationship for female
gaming_data %>% 
  filter(Sex == "Female") %>%
  select(Mot.escapism, Impulsivity.sum, K_scale_sum) %>% 
  pairs.panels(lm=TRUE, stars=TRUE)

# write model
moderate_effects <- lm(K_scale_sum~Mot.escapism+Sex+Sex*Mot.escapism+Impulsivity.sum, data = gaming_data)

# 결과 확인
coef(summary(moderate_effects)) %>%
  kable(digits = 2)

# making table using stargazer library
main_effects <- lm(K_scale_sum~Mot.escapism+Sex+Impulsivity.sum, data = gaming_data)
stargazer(main_effects, moderate_effects, column.labels = c("Main Effects", "Interaction"),
          type="html", digits = 2, title = "Escapism, Impulsivity and Sex on Interner Gaming Addiction")


# plotting method1: ggplot2
ggplot(gaming_data, aes(x = Mot.escapism, y = K_scale_sum, color=Sex))+ # shape = Sex
  geom_point(colour = "white")+
  coord_cartesian(xlim = c(4, 20), ylim = c(30, 80))+
  geom_smooth(method=lm, se = TRUE, fullrange=TRUE)+ 
  theme_classic()+
  labs(x="Escapism", y="Gaming Disorder Scale")+
  theme(axis.title  = element_text(size = 20), axis.text  = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 10)) +
  ggsave("scatter plot by group.jpg", dpi = 600)


# plotting method2: rockchalk 
ps  <- plotSlopes(moderate_effects, plotx="Mot.escapism", modx="Sex", modxVals = c("Male", "Female"),
                  xlab = "Escapism",ylab = "Interner Gaming Addiction")
