library(readr)
library(tidyverse)
library(stargazer)
library(ggplot2)
library(stats)
library(readxl)
library(vtable)
library(sandwich)
library(robustbase)
library(lmtest)
library(modelr)
library(broom)
library(corrplot)
library(estimatr)
options(scipen=999)



main <- as.data.frame(read_excel("C:/Users/3PX00LA_RS4/Desktop/ENG - Definitiva - 333 obs -  Decisiones sobre activos modernos.xlsx"))
attach(main)

#Arreglar los grupos de edades y agregar DUMMIES importantes

      # Dummies grupos de edad
main <- main %>% mutate("Age 18-24" = ifelse(`Age group` == "18-24",1,0))
main <- main %>% mutate("Age 25-34" = ifelse(`Age group` == "25-34",1,0))
main <- main %>% mutate("Age 35-44" = ifelse(`Age group` == "35-44",1,0))
main <- main %>% mutate("Age 45-65" = ifelse(`Age group` == "45-64" | `Age group` == "65+",1,0))
main$`Age group`[main$`Age group` == '45-64' | main$`Age group` == '65+'] <- '45-65' 

      #Arreglo de género y una carrera en español y varios
main$Gender[main$`Gender` == 'Puma Mayor'] <- 'Male'
main$Gender[main$`Gender` == 'genderqueer'] <- 'Male'
main$`Bachelor's degree`[main$`Bachelor's degree` == 'Actuacion'] <- 'Acting'
main$`Education level (currently enrolled)`[main$`Education level (currently enrolled)` == 'Carrera técnica'] <- 'Technical career'
main$`Education level (currently enrolled)`[main$`Education level (currently enrolled)` == 'Estudiante'] <- 'High school (or lower)'
main$`Father's max education`[main$`Father's max education` == 'Oficio'] <- 'High school (or lower)'




      #Dummy de Female y una de Risky in General
main <- main %>% mutate("Risky in general" = ifelse(`General Risk` > 5,1,0))
main <- main %>% mutate(Female = ifelse(Gender == "Female",1,0))

      #Dummies por grupo de ingresos - 2 de noviembre
main <- main %>% mutate("Group 1" = ifelse(`Monthly net income (MXN)` == '$0-$2,699',1,0)) %>%
                 mutate("Group 2" = ifelse(`Monthly net income (MXN)` == '$2,700-$6,799',1,0)) %>%
                 mutate("Group 3" = ifelse(`Monthly net income (MXN)` == '$6,800-$11,599',1,0)) %>%
                 mutate("Group 4" = ifelse(`Monthly net income (MXN)` == '$11,600-$34,999',1,0)) %>%
                 mutate("Group 5" = ifelse(`Monthly net income (MXN)` == '$35,000-$84,999',1,0)) %>%
                 mutate("Group 6" = ifelse(`Monthly net income (MXN)` == '$85,000 +',1,0))

      #Dummies por nivel educativo - 2 de noviembre
main <- main %>% mutate("B's degree" = ifelse(`Education level (currently enrolled)` == "Bachelor's Degree",1,0)) %>%
                 mutate("High school (or lower)" = ifelse(`Education level (currently enrolled)` == "High school (or lower)",1,0)) %>%
                 mutate("Technical career" = ifelse(`Education level (currently enrolled)` == "Technical career",1,0)) %>%
                 mutate("Master's degree" = ifelse(`Education level (currently enrolled)` == "Master's Degree",1,0)) %>%
                 mutate("PhD" = ifelse(`Education level (currently enrolled)` == "PhD",1,0))

      #Dummies nivel educativo de los PADRES - 2 de noviembre

              #MADRE
main <- main %>% mutate("Mother's MAX is Bd" = ifelse(`Mother's max education` == "Bachelor's Degree", 1, 0)) %>%
                 mutate("Mother's MAX is HS (or lower)" = ifelse(`Mother's max education` == "High school (or lower)", 1, 0)) %>%
                 mutate("Mother's MAX is Technical" = ifelse(`Mother's max education` == "Technical career", 1, 0)) %>%
                 mutate("Mother's MAX is Md" = ifelse(`Mother's max education` == "Master's Degree", 1, 0)) %>%
                 mutate("Mother's MAX is PhD" = ifelse(`Mother's max education` == "Bachelor's Degree", 1, 0)) 

              #PADRE
main <- main %>% mutate("Father's MAX is Bd" = ifelse(`Father's max education` == "Bachelor's Degree", 1, 0)) %>%
                 mutate("Father's MAX is HS (or lower)" = ifelse(`Father's max education` == "High school (or lower)", 1, 0)) %>%
                 mutate("Father's MAX is Technical" = ifelse(`Father's max education` == "Technical career", 1, 0)) %>%
                 mutate("Father's MAX is Md" = ifelse(`Father's max education` == "Master's Degree", 1, 0)) %>%
                 mutate("Father's MAX is PhD" = ifelse(`Father's max education` == "Bachelor's Degree", 1, 0))


      



###ANÁLISIS DESCRIPTIVO###

risk_stats <- main[,c("General Risk","Finance Risk","Career Risk",
                      "Health Risk","Education Risk","Leisure & Sport Risk","Alcohol/Tobacco OaM")]
sumtable(risk_stats)
  stargazer(risk_stats, title = "Descriptive risk statistics", type = "latex")
      #Conteo de personas por nivel educativo
nivel_educ <- main %>% group_by(`Education level (currently enrolled)`) %>% tally() 

      #Promedio de hombres y mujeres (excluyendo 2 NA's)
mean(Female, na.rm = TRUE) 

      #Conteo por carrera
cont_carrera <- main %>% group_by(`Bachelor's degree`) %>% tally(sort = TRUE)
cont_carrera

#Gráficas experimentales (20 de octubre)


      #General Risk question
ggplot(main, aes(`General Risk`)) + 
  geom_histogram(color = 'black', fill = 'pink') + 
  theme_bw() + ggtitle("Responses to General Risk Question") + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = seq(0,10,1)) + xlab(label = "General Risk self-assessment")

      #Lottery Reservation Price
ggplot(main, aes(`Lottery Reservation Price`)) +
  geom_histogram( color ='black', fill = 'aquamarine', bins = 20) + 
  theme_bw() + ggtitle(" Figure 2: Distribution of the Lottery Resrvation Price") + theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Times New Roman")) + 
  scale_x_continuous(breaks = seq(0,1200,100)) + ylab(label = "Count")
  

      #Lottery Reservation Probability
ggplot(main, aes(`Lottery Reservation Probability`)) +
  geom_histogram( color ='black', fill = 'orange', bins = 20) + 
  theme_bw() + ggtitle("Figure 3: Distribution of the Lottery Resrvation Probability") + theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Times New Roman")) + 
  scale_x_continuous(breaks = seq(0,100,10)) + xlab(label = "Lottery Reservation Probability (%)") + ylab(label = "Count")

      #Age
main %>% filter(!is.na(`Age group`)) %>% filter(!is.na(Gender)) %>% 
  ggplot(aes(`Age group`, color = `Gender`, fill = `Gender`)) +
  geom_histogram(stat = "count",na.rm = TRUE ) + theme_bw() + 
  ggtitle("Figure 4: Age Group count") + theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Times New Roman"))
      
      #Monthly Income
  #Ordernar los grupos de ingreso en el eje x con FACTOR
main$`Monthly net income (MXN)` <- factor(main$`Monthly net income (MXN)`, levels = 
                                            c("$0-$2,699", "$2,700-$6,799", "$6,800-$11,599","$11,600-$34,999","$35,000-$84,999","$85,000 +"))

  #Histograma
main %>% filter(!is.na(`Monthly net income (MXN)`)) %>% 
  ggplot(aes(`Monthly net income (MXN)`)) + 
  geom_histogram(stat = "count", color = 'black', fill = "green4", na.rm = TRUE) + 
  theme_bw() + ggtitle("Figure 5: Monthly Income Distribution") + theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Times New Roman"))

      #Education level
main %>% filter(!is.na(`Education level (currently enrolled)`)) %>% 
  ggplot(aes(`Education level (currently enrolled)`)) + 
  geom_histogram(stat = "count", color = 'black', fill = "royalblue1", na.rm = TRUE) + 
  theme_bw() + ggtitle("Figure 6: Education Level Distribution") + theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Times New Roman")) + 
  ylab(label = "Count")


#Experimentación con regresión- 
reg1 <- lm(`General Risk` ~ `Age group` + `Height` + `General interest in cryptos` +
     `Alcohol/Tobacco OaM` + `Lottery Reservation Price` + `Lottery Reservation Probability`, main)
summary(reg1)


      #Rieso general como variable binaria

reg2 <- lm(`Risky in general` ~ Female + `Height`, main )
summary(reg2)

#25 de octubre - "Main Results"

        ######## Ever Invested in a Crypto ##########

reg_ever_inv_price <- lm(`Ever Invested in a Crypto` ~ `Lottery Reservation Price`,na.action = na.omit, main)
summary(reg_ever_inv_price)

inv1 <- coeftest(reg_ever_inv_price, vcov = vcovHC(reg_ever_inv_price, type = "HC0"))

#

reg_ever_inv_prob <-lm(`Ever Invested in a Crypto` ~ `Lottery Reservation Probability`, na.action = na.omit, main)
summary(reg_ever_inv_prob)

inv2 <- coeftest(reg_ever_inv_prob, vcov = vcovHC(reg_ever_inv_prob, type = "HC0"))

#

reg_ever_inv_finrisk <-lm(`Ever Invested in a Crypto` ~ `Finance Risk`, na.action = na.omit, main)
summary(reg_ever_inv_finrisk)

inv3 <- coeftest(reg_ever_inv_finrisk, vcov = vcovHC(reg_ever_inv_finrisk, type = "HC0"))

#

reg_ever_inv_agg <- lm(`Ever Invested in a Crypto` ~ `Lottery Reservation Price` + `Lottery Reservation Probability`
                       + `Finance Risk`, na.action = na.omit, main)
summary(reg_ever_inv_agg)
bptest(reg_ever_inv_agg) #We can reject the H0 that the variance of the residuals is constant

inv4 <- coeftest(reg_ever_inv_agg, vcov = vcovHC(reg_ever_inv_agg, type = "HC0"))

#

reg_ever_inv_cont <- lm(`Ever Invested in a Crypto` ~ `Lottery Reservation Price` + `Lottery Reservation Probability` + `Finance Risk` + 
                        + `Female` + `Age 18-24` + `Age 25-34` + `Age 35-44`, main)  ####Coef. significativo de MUJERES
summary(reg_ever_inv_cont)

inv5 <- coeftest(reg_ever_inv_cont, vcov = vcovHC(reg_ever_inv_cont, type ="HC0"))

#

reg_ever_inv_mascontroles <- lm(`Ever Invested in a Crypto` ~ `Lottery Reservation Price` + `Lottery Reservation Probability` + `Finance Risk` + 
                                + `Female` + `Age 18-24` + `Age 25-34` + `Age 35-44` + `Monthly net income (MXN)` + `Education level (currently enrolled)`, main)

inv6 <- coeftest(reg_ever_inv_mascontroles, vcov = vcovHC(reg_ever_inv_mascontroles, type = "HC0"))

           ###TABLE 3###
stargazer(inv1, inv2, inv3, inv4, inv5, inv6, type = "latex", dep.var.caption = "Ever Invested in a Cryptocurrency",
          intercept.top = TRUE, intercept.bottom = FALSE, notes = "Heteroskedasticity-robust standar errors", title = "Factors influencing the decision of buying crypto")


      ### Determinants of risk attitudes in different domains of life ###

gen_dom <- lm(`General Risk` ~ Female + `Age group` + Height + `Mother's max education` + `Father's max education`, main)
domain1 <- coeftest(gen_dom, vcov = vcovHC(gen_dom, type = "HC0"))



fin_dom <- lm(`Finance Risk` ~ Female + `Age group` + Height + `Mother's max education` + `Father's max education`, main)
domain2 <- coeftest(fin_dom, vcov = vcovHC(fin_dom, type = "HC0"))



spor_dom <- lm(`Leisure & Sport Risk` ~ Female + `Age group` + Height + `Mother's max education` + `Father's max education`, main)
domain3 <- coeftest(spor_dom, vcov = vcovHC(spor_dom, type = "HC0"))



career_dom <- lm(`Career Risk` ~ Female + `Age group` + Height + `Mother's max education` + `Father's max education`, main)
domain4 <- coeftest(career_dom, vcov = vcovHC(career_dom, type = "HC0"))



health_dom <- lm(`Health Risk` ~ Female + `Age group` + Height + `Mother's max education` + `Father's max education`, main)
domain5 <- coeftest(health_dom, vcov = vcovHC(health_dom, type = "HC0"))

          ### Table 2: Risk factors in different domains ###
stargazer(domain1, domain2, domain3, domain4, domain5, type = "latex", column.labels = c("General Risk", "Finance Risk", "Leisure and Sport Risk",
                                                                                         "Career Risk", "Health Risk"), notes = "Heteroskedasticity-robust standar errors",
                                                                                          title = "Risk factors in different domains")




        ######## Ever EXCHANGED a Crypto ##########

reg_ever_exch_price <- lm(`Ever exchanged a crypto for a good or service` ~ `Lottery Reservation Price`,na.action = na.omit, main)
summary(reg_ever_inv_price)

reg_ever_exch_prob <-lm(`Ever exchanged a crypto for a good or service` ~ `Lottery Reservation Probability`, na.action = na.omit, main)
summary(reg_ever_inv_prob)

reg_ever_exch_finrisk <-lm(`Ever exchanged a crypto for a good or service` ~ `Finance Risk`, na.action = na.omit, main)
summary(reg_ever_inv_finrisk)

reg_ever_exch_agg <- lm(`Ever exchanged a crypto for a good or service` ~ `Lottery Reservation Price` + `Lottery Reservation Probability`
                       + `Finance Risk`, na.action = na.omit, main)
summary(reg_ever_exch_agg)



        ######## General Interest in Crypto #########

reg_gen_int_price <- lm(`General interest in cryptos`~ `Lottery Reservation Price`,na.action = na.omit, main)
gen1 <- coeftest(reg_gen_int_price, vcov = vcovHC(reg_gen_int_price, type = "HC0"))

#

reg_gen_int_prob <-lm(`General interest in cryptos` ~ `Lottery Reservation Probability`, na.action = na.omit, main)
gen2 <- coeftest(reg_gen_int_prob, vcov = vcovHC(reg_gen_int_prob, type = "HC0"))

#

reg_gen_int_finrisk <-lm(`General interest in cryptos` ~ `Finance Risk`, na.action = na.omit, main)
gen3 <- coeftest(reg_gen_int_finrisk, vcov = vcovHC(reg_gen_int_finrisk, type = "HC0"))

#

reg_gen_int_agg <- lm(`General interest in cryptos` ~ `Lottery Reservation Price` + `Lottery Reservation Probability`
                       + `Finance Risk`, na.action = na.omit, main)
gen4 <- coeftest(reg_gen_int_agg, vcov = vcovHC(reg_gen_int_agg, type = "HC0"))

#

reg_gen_int_cont <- lm(`General interest in cryptos` ~ `Lottery Reservation Price` + `Lottery Reservation Probability` + `Finance Risk` + 
                        + `Female` + `Age 18-24` + `Age 25-34` + `Age 35-44` + `High school (or lower)` +  `B's degree` + `Master's degree`, main)  ####Coef. significativo de MUJERES
gen5 <- coeftest(reg_gen_int_cont, vcov = vcovHC(reg_gen_int_cont, type ="HC0"))

                  ### TABLE 5: Determinants of interest in cryptocurrencies ###
stargazer(gen1, gen2, gen3, gen4, gen5, type = "latex", dep.var.caption = "General Interest in Cryptocurrencies", 
          intercept.bottom = FALSE, intercept.top = TRUE, notes = "Heteroskedasticity-robust standar errors", title = "Determinants of interest in cryptocurrencies")


      
      ### Estadísticas de Crypto-bros ###  (25 de noviembre)

just_crypto_main <-subset(main, `Ever Invested in a Crypto` == 1)
just_crypto_main %>% group_by(Gender) %>% tally(`Ever Invested in a Crypto`)

sin_cb <- subset(main, `Ever Invested in a Crypto` == 0)


#invested monthly
just_crypto_main %>% group_by(`Monthly income invested in crypto`) %>% tally()

#exchanged
just_crypto_main %>% tally(`Ever exchanged a crypto for a good or service`)

#Matriz de correlaciones de riesgo
risk_corr <- cor(risk_stats,  use = "complete.obs" )
corrplot(risk_corr)

#Descriptivas de crypto-bros

cb_risk_stats <- just_crypto_main[,c("General Risk","Finance Risk","Career Risk",
                      "Health Risk","Education Risk","Leisure & Sport Risk","Alcohol/Tobacco OaM")]


    ### TABLA 4: Descriptive statistics of crypto users ###
stargazer(cb_risk_stats, type = "latex")



#Pruebas para saber si la diferencia entre crypto bros y la población en general es significativa

t.test(sin_cb$`General Risk`, just_crypto_main$`General Risk`, 
       alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)


t.test(sin_cb$`Finance Risk`, just_crypto_main$`Finance Risk`, 
       alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)


t.test(sin_cb$`Career Risk`, just_crypto_main$`Career Risk`, 
       alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)


t.test(sin_cb$`Health Risk`, just_crypto_main$`Health Risk`, 
       alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)


t.test(sin_cb$`Education Risk`, just_crypto_main$`Education Risk`, 
       alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)


t.test(sin_cb$`Leisure & Sport Risk`, just_crypto_main$`Leisure & Sport Risk`, 
       alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)


t.test(sin_cb$`Alcohol/Tobacco OaM`, just_crypto_main$`Alcohol/Tobacco OaM`, 
       alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)


