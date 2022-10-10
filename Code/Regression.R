library(tidyverse)

setwd("C:/Users/Jeremy Chia/Desktop/2022/Research/Code/")
data <- read.csv("combined dataset.csv", fileEncoding = "UTF-8-BOM")

data %>% names()

####

roa_rq2 <- lm(ROA ~ t_Community + t_Resources + t_Customers + t_Employees + t_Metholodgy + t_Governance + t_Climate, data )
summary(roa_rq2)

roa_rq2_w <- lm(ROA ~ w_Community + w_Resources + w_Customers + w_Employees + w_Metholodgy + w_Governance + w_Climate, data )
summary(roa_rq2_w)

roa_rq3 <- lm(ROA ~ flesch + sentiment, data)
summary(roa_rq3)

roa_comb <- lm(ROA ~ w_Community + w_Resources + w_Customers + w_Employees + w_Metholodgy + w_Governance + w_Climate + flesch + sentiment, data )
summary(roa_comb)

####

mv_rq2 <- lm(MV_t4 ~ BV_t + EARN_t + EARN_t * NEG_t + t_Community + t_Resources + t_Customers + t_Employees + t_Metholodgy + t_Governance + t_Climate, data )
summary(mv_rq2)

mv_rq2_w <- lm(MV_t4 ~ BV_t + EARN_t + EARN_t * NEG_t + w_Community + w_Resources + w_Customers + w_Employees + w_Metholodgy + w_Governance + w_Climate, data )
summary(mv_rq2_w)

mv_rq3 <- lm(MV_t4 ~ BV_t + EARN_t + EARN_t * NEG_t + flesch + sentiment, data)
summary(mv_rq3)

mv_comb <- lm(MV_t4 ~ BV_t + EARN_t + EARN_t * NEG_t + w_Community + w_Resources + w_Customers + w_Employees + w_Metholodgy + w_Governance + w_Climate + flesch + sentiment, data )
summary(mv_comb)

####

zs_rq2 <- lm(zmijewski_t ~ t_Community + t_Resources + t_Customers + t_Employees + t_Metholodgy + t_Governance + t_Climate, data )
summary(zs_rq2)

zs_rq2_w <- lm(zmijewski_t ~ w_Community + w_Resources + w_Customers + w_Employees + w_Metholodgy + w_Governance + w_Climate, data )
summary(zs_rq2_w)

zs_rq3 <- lm(zmijewski_t ~ flesch + sentiment, data)
summary(zs_rq3)

zs_comb <- lm(zmijewski_t ~ w_Community + w_Resources + w_Customers + w_Employees + w_Metholodgy + w_Governance + w_Climate + flesch + sentiment, data )
summary(zs_comb)

####

summary(lm(zmijewski_t ~ flesch, data))
summary(lm(zmijewski_t ~ sentiment, data))


###

res <- cor(data %>% select(-c(Company,Year,t_Community,t_Resources,t_Customers,t_Employees,t_Metholodgy,t_Governance,t_Climate)))
round(res, 2)

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
