################################################################################
#                                                                              #
#                 Edit explorations                                            #
#                Guillermo Ruiz Pava                                           #
#                                                                              #
################################################################################


##### Explorations #####  

utils::install.packages("ggside")
library(ggplot2)
library(ggside)
library(dplyr)

#Frugal, recode, EFA
Edit_2019_2020$frug1_recod <- recode(Edit_2019_2020$I2R1C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 

table(Edit_2019_2020$frug1_recod)
table(Edit_2019_2020$I2R1C1)

Edit_2019_2020$frug2_recod <- recode(Edit_2019_2020$I2R2C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug3_recod <- recode(Edit_2019_2020$I2R3C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug4_recod <- recode(Edit_2019_2020$I2R4C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug5_recod <- recode(Edit_2019_2020$I2R5C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug6_recod <- recode(Edit_2019_2020$I2R6C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug7_recod <- recode(Edit_2019_2020$I2R7C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug8_recod <- recode(Edit_2019_2020$I2R8C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug9_recod <- recode(Edit_2019_2020$I2R9C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug10_recod <- recode(Edit_2019_2020$I2R10C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug11_recod <- recode(Edit_2019_2020$I2R11C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug12_recod <- recode(Edit_2019_2020$I2R12C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug13_recod <- recode(Edit_2019_2020$I2R13C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug14_recod <- recode(Edit_2019_2020$I2R14C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug15_recod <- recode(Edit_2019_2020$I2R15C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug16_recod <- recode(Edit_2019_2020$I2R16C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug17_recod <- recode(Edit_2019_2020$I2R17C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 
Edit_2019_2020$frug18_recod <- recode(Edit_2019_2020$I2R18C1, `4` = 1, `3` = 2, `2` = 3, `1` = 4) 

install.packages("psych")
library("psych")

fdat <- as_tibble(Edit_2019_2020)
fdat <- fdat %>% select(NORDEMP,
                frug1_recod, 
                frug2_recod, 
                frug3_recod, 
                frug4_recod, 
                frug5_recod,
                frug6_recod,
                frug7_recod, 
                frug8_recod,
                frug9_recod,
                frug10_recod,
                frug11_recod,
                frug12_recod,
                frug13_recod,
                frug14_recod,
                frug15_recod,
                frug16_recod,
                frug17_recod,
                frug18_recod)

fdat1 <- as.data.frame(na.omit(fdat))
rownames(fdat1) <- fdat1$NORDEMP
head(fdat1)
fit <- principal(fdat1[,2:19], 2, rotate = "promax")
dim(fdat1)
dim(fit$scores)

fit_ort <- principal(fdat1[,2:19], 2, rotate = "varimax")
fit_ort$loadings
plot(ev)

KMO(fdat1)
fit_ort$scores
summary(fit$scores)
hist(fit_ort$scores[,1])
hist(fit_ort$scores[,2])

fit_ort <- as.data.frame(fit_ort$scores)
fit_ort
head(fit_ort)
fit_ort$NORDEMP <- as.numeric(rownames(fit_ort))
fit_ort[1:10,]

is.numeric(fit_ort$NORDEMP)

EDIT_19_20 <- merge.data.frame(EDIT_19_20, fit_ort, by=c("NORDEMP"), all.x=TRUE)
dim(EDIT_19_20)
EDIT_19_20$RC1 <- EDIT_19_20$RC1.x
EDIT_19_20$RC2 <- EDIT_19_20$RC2.x

Edit_2019_2020 <- merge.data.frame(Edit_2019_2020, fit_ort, by=c("NORDEMP"), all.x=TRUE)
write.csv(Edit_2019_2020, file = "EDIT_19_20_frug.csv", sep = " ; ")

Edit_2019_2020$RC1 <- Edit_2019_2020$RC1.y
Edit_2019_2020$RC2 <- Edit_2019_2020$RC2.y

install.packages("nFactors")
library(nFactors)

ev <- eigen(cor(fdat1))
ap <- parallel(subject = nrow(fdat1), var = ncol(fdat1), rep = 100, cent = .05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)







#Frugal indicator

Edit_2019_2020$frug1 <- ifelse(Edit_2019_2020$I2R1C1<3, 1, 0) 
Edit_2019_2020$frug2 <- ifelse(Edit_2019_2020$I2R2C1<3, 1, 0) 
Edit_2019_2020$frug3 <- ifelse(Edit_2019_2020$I2R3C1<3, 1, 0) 
Edit_2019_2020$frug4 <- ifelse(Edit_2019_2020$I2R4C1<3, 1, 0) 
Edit_2019_2020$frug5 <- ifelse(Edit_2019_2020$I2R5C1<3, 1, 0) 
Edit_2019_2020$frug6 <- ifelse(Edit_2019_2020$I2R6C1<3, 1, 0) 
Edit_2019_2020$frug7 <- ifelse(Edit_2019_2020$I2R7C1<3, 1, 0) 
Edit_2019_2020$frug8 <- ifelse(Edit_2019_2020$I2R8C1<3, 1, 0) 
Edit_2019_2020$frug9 <- ifelse(Edit_2019_2020$I2R9C1<3, 1, 0) 
Edit_2019_2020$frug10 <- ifelse(Edit_2019_2020$I2R10C1<3, 1, 0) 
Edit_2019_2020$frug11 <- ifelse(Edit_2019_2020$I2R11C1<3, 1, 0) 
Edit_2019_2020$frug12 <- ifelse(Edit_2019_2020$I2R12C1<3, 1, 0) 
Edit_2019_2020$frug13 <- ifelse(Edit_2019_2020$I2R13C1<3, 1, 0) 
Edit_2019_2020$frug14 <- ifelse(Edit_2019_2020$I2R14C1<3, 1, 0) 
Edit_2019_2020$frug15 <- ifelse(Edit_2019_2020$I2R15C1<3, 1, 0) 
Edit_2019_2020$frug16 <- ifelse(Edit_2019_2020$I2R16C1<3, 1, 0) 
Edit_2019_2020$frug17 <- ifelse(Edit_2019_2020$I2R17C1<3, 1, 0) 
Edit_2019_2020$frug18 <- ifelse(Edit_2019_2020$I2R18C1<3, 1, 0) 

Edit_2019_2020$frug_value <- Edit_2019_2020$frug1 + Edit_2019_2020$frug2 + Edit_2019_2020$frug3 + Edit_2019_2020$frug4 + Edit_2019_2020$frug5 + Edit_2019_2020$frug8 + Edit_2019_2020$frug9 + Edit_2019_2020$frug14 + Edit_2019_2020$frug15 + Edit_2019_2020$frug16 + Edit_2019_2020$frug17 + Edit_2019_2020$frug18

Edit_2019_2020$frug_recurso <- Edit_2019_2020$frug6 + Edit_2019_2020$frug7 + Edit_2019_2020$frug8 + Edit_2019_2020$frug9 + Edit_2019_2020$frug10 + Edit_2019_2020$frug11 + Edit_2019_2020$frug12 + Edit_2019_2020$frug14

Edit_2019_2020$indic_frug <- Edit_2019_2020$frug_recurso*Edit_2019_2020$frug_value

freq(Edit_2019_2020$frug_inn_v2020)
hist(Edit_2019_2020$indic_frug, breaks = 10)
summary(Edit_2019_2020$indic_frug)


Edit_2019_2020$NOInnov <- ifelse(Edit_2019_2020$TIPOLO=="NOINNO", "NOinn", "Inn")  
Edit_2019_2020$AmpliaInnov <- ifelse(Edit_2019_2020$TIPOLO=="AMPLIA", "Inn", "NOInn")  
library("cleaner")
freq(Edit_2019_2020$TIPOLO)
freq(Edit_2019_2020$NOInnov)
freq(Edit_2019_2020$AmpliaInnov)
Edit_2019_2020$ventas_nal <- (Edit_2019_2020$I3R1C1+Edit_2019_2020$I3R2C1)/2
Edit_2019_2020$log_ventas_nal <- log(Edit_2019_2020$ventas_nal+1)
Edit_2019_2020$export <- (Edit_2019_2020$I3R1C2+Edit_2019_2020$I3R2C2)/2
Edit_2019_2020$log_export <- log(Edit_2019_2020$export+1)
Edit_2019_2020$Total_ACTI <- (Edit_2019_2020$II1R10C1+Edit_2019_2020$II1R10C2)/2
Edit_2019_2020$Total_ID <- (Edit_2019_2020$II1R1C1+Edit_2019_2020$II1R1C2)/2

Edit_2019_2020$rest_ext_19 <- 1-(Edit_2019_2020$III1R1C1/Edit_2019_2020$III1R8C1)
Edit_2019_2020$rest_ext_20 <- 1-(Edit_2019_2020$III1R1C2/Edit_2019_2020$III1R8C2)
Edit_2019_2020$rest_ext <- (Edit_2019_2020$rest_ext_19+Edit_2019_2020$rest_ext_20)/2

cor(Edit_2019_2020$rest_ext_19, Edit_2019_2020$RC1, use = "complete.obs")
cor(Edit_2019_2020$rest_ext_19, Edit_2019_2020$RC2, use = "complete.obs")

cor(Edit_2019_2020$rest_ext_20, Edit_2019_2020$RC1, use = "complete.obs")
cor(Edit_2019_2020$rest_ext_20, Edit_2019_2020$RC2, use = "complete.obs")

cor(Edit_2019_2020$rest_ext, Edit_2019_2020$RC1, use = "complete.obs")
cor(Edit_2019_2020$rest_ext, Edit_2019_2020$RC2, use = "complete.obs")

summary(Edit_2019_2020$Total_ACTI)



ggplot(Edit_2019_2020, aes(Total_ACTI, indic_frug)) + 
  geom_point(size = 2)
cor(Edit_2019_2020$Total_ACTI, Edit_2019_2020$indic_frug, use = "complete.obs")

library("ggplot2")
ggplot(Edit_2019_2020, aes(Total_ACTI, RC1)) + 
  geom_point(size = 2)
cor(Edit_2019_2020$Total_ACTI, Edit_2019_2020$RC1, use = "complete.obs")

ggplot(Edit_2019_2020, aes(Total_ACTI, RC2)) + 
  geom_point(size = 2)
cor(Edit_2019_2020$Total_ACTI, Edit_2019_2020$RC2, use = "complete.obs")

ggplot(Edit_2019_2020, aes(Total_ID, indic_frug)) + 
  geom_point(size = 2)
cor(Edit_2019_2020$Total_ID, Edit_2019_2020$indic_frug, use = "complete.obs")

ggplot(Edit_2019_2020, aes(Total_ID, indic_frug)) + 
  geom_point(size = 2)
cor(Edit_2019_2020$Total_ID, Edit_2019_2020$RC1, use = "complete.obs")
cor(Edit_2019_2020$Total_ID, Edit_2019_2020$RC2, use = "complete.obs")

ggplot(Edit_2019_2020, aes(log_ventas_nal, indic_frug)) + 
  geom_point(size = 2)
cor(Edit_2019_2020$log_ventas_nal, Edit_2019_2020$indic_frug, use = "complete.obs")
cor(Edit_2019_2020$log_ventas_nal, Edit_2019_2020$RC1, use = "complete.obs")
cor(Edit_2019_2020$log_ventas_nal, Edit_2019_2020$RC2, use = "complete.obs")
ggplot(Edit_2019_2020, aes(log_ventas_nal, RC2)) + 
  geom_point(size = 2)

ggplot(Edit_2019_2020, aes(export, indic_frug)) + 
  geom_point(size = 2)
cor(Edit_2019_2020$export, Edit_2019_2020$indic_frug, use = "complete.obs")
cor(Edit_2019_2020$export, Edit_2019_2020$RC1, use = "complete.obs")
cor(Edit_2019_2020$export, Edit_2019_2020$RC2, use = "complete.obs")

ggplot(Edit_2019_2020, aes(br_coop_v2020, indic_frug)) + 
  geom_point(size = 2)
cor(Edit_2019_2020$Total_ACTI, Edit_2019_2020$indic_frug, use = "complete.obs")


# Boxplot by group
library(ggplot2)

Edit_2019_2020$client <- as.factor(Edit_2019_2020$V3R3C1)

ggplot(data = Edit_2019_2020, aes(x = client, y = indic_frug)) +
  stat_boxplot(geom = "errorbar", # Boxplot with error bars 
               width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors
               alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "Degree of Frugality") +  # Continuous variable label
  scale_x_discrete(name = "Cooperation with clients") +      # Group label
  ggtitle("Diferencia en frugalidad al cooperar con clientes") + # Plot title
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25)) 

t.test(indic_frug ~ client, data = Edit_2019_2020)

ggplot(data = Edit_2019_2020, aes(x = client, y = RC1)) +
  stat_boxplot(geom = "errorbar", # Boxplot with error bars 
               width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors
               alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "RC1 (use of resources)") +  # Continuous variable label
  scale_x_discrete(name = "Cooperation with clients") +      # Group label
  ggtitle("Diferencia en 'frugalidad' al cooperar con clientes") + # Plot title
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25)) 

t.test(RC1 ~ client, data = Edit_2019_2020)


ggplot(data = Edit_2019_2020, aes(x = client, y = RC2)) +
  stat_boxplot(geom = "errorbar", # Boxplot with error bars 
               width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors
               alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "RC2 (value creation)") +  # Continuous variable label
  scale_x_discrete(name = "Cooperation with clients") +      # Group label
  ggtitle("Diferencia en 'frugalidad' al cooperar con clientes") + # Plot title
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25)) 

t.test(RC2 ~ client, data = Edit_2019_2020)

Edit_2019_2020$supplier <- as.factor(Edit_2019_2020$V3R2C1)

t.test(indic_frug ~ supplier, data = Edit_2019_2020)




Edit_2019_2020$compet <- as.factor(Edit_2019_2020$V3R4C1)

t.test(indic_frug ~ compet, data = Edit_2019_2020)

ggplot(data = Edit_2019_2020, aes(x = compet, y = indic_frug)) +
  stat_boxplot(geom = "errorbar", # Boxplot with error bars 
               width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors
               alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "Degree of Frugality") +  # Continuous variable label
  scale_x_discrete(name = "Cooperation with competitors") +      # Group label
  ggtitle("Diferencia en frugalidad al cooperar con competidores (coopetencia)") + # Plot title
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25)) 

Edit_2019_2020$universid <- as.factor(Edit_2019_2020$V3R6C1)

t.test(indic_frug ~ universid, data = Edit_2019_2020)
t.test(RC1 ~ universid, data = Edit_2019_2020)
t.test(RC2 ~ universid, data = Edit_2019_2020)

ggplot(data = Edit_2019_2020, aes(x = universid, y = indic_frug)) +
  stat_boxplot(geom = "errorbar", # Boxplot with error bars 
               width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors
               alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "Degree of Frugality") +  # Continuous variable label
  scale_x_discrete(name = "Cooperation with university") +      # Group label
  ggtitle("Diferencia en indicador de frugalidad al cooperar con universidades") + # Plot title
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25)) 

ggplot(data = Edit_2019_2020, aes(x = universid, y = RC1)) +
  stat_boxplot(geom = "errorbar", # Boxplot with error bars 
               width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors
               alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "RC1") +  # Continuous variable label
  scale_x_discrete(name = "Cooperation with university") +      # Group label
  ggtitle("Diferencia en impacto de frugalidad (RC1) al cooperar con universidades") + # Plot title
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25)) 

ggplot(data = Edit_2019_2020, aes(x = universid, y = RC2)) +
  stat_boxplot(geom = "errorbar", # Boxplot with error bars 
               width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors
               alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "RC2") +  # Continuous variable label
  scale_x_discrete(name = "Cooperation with university") +      # Group label
  ggtitle("Diferencia en impacto de frugalidad (RC2) al cooperar con universidades") + # Plot title
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25)) 


Edit_2019_2020$gobierno <- as.factor(Edit_2019_2020$V3R12C1)

t.test(indic_frug ~ gobierno, data = Edit_2019_2020)

ggplot(data = Edit_2019_2020, aes(x = gobierno, y = indic_frug)) +
  stat_boxplot(geom = "errorbar", # Boxplot with error bars 
               width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors
               alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "Degree of Frugality") +  # Continuous variable label
  scale_x_discrete(name = "Cooperation with government") +      # Group label
  ggtitle("Diferencia en frugalidad al cooperar con el gobierno") + # Plot title
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25)) 


####obstáculos

#escasez

Edit_2019_2020$escasez <- dplyr::recode(Edit_2019_2020$I10R1C1, `3` = 0, `2` = 1, `1` = 1) 

Edit_2019_2020$escasez <- as.factor(Edit_2019_2020$escasez)

Edit_2019_2020$escasez <- NULL

table(Edit_2019_2020$I10R1C1)
table(Edit_2019_2020$escasez)

t.test(RC1 ~ escasez, data = Edit_2019_2020)
t.test(RC2 ~ escasez, data = Edit_2019_2020)

ggplot(data = Edit_2019_2020, aes(x = escasez, y = RC1)) +
  stat_boxplot(geom = "errorbar", # Boxplot with error bars 
               width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors
               alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "RC1") +  # Continuous variable label
  scale_x_discrete(name = "Scarcity own resources") +      # Group label
  ggtitle("Diferencia en frugalidad") + # Plot title
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25)) 

ggplot(data = Edit_2019_2020, aes(x = escasez, y = RC2)) +
  stat_boxplot(geom = "errorbar", # Boxplot with error bars 
               width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors
               alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "RC2") +  # Continuous variable label
  scale_x_discrete(name = "Scarcity own resources") +      # Group label
  ggtitle("Diferencia en frugalidad") + # Plot title
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25)) 

#Financial constraints
Edit_2019_2020$fin_const <- dplyr::recode(Edit_2019_2020$I10R10C1, `3` = 0, `2` = 1, `1` = 1) 

Edit_2019_2020$fin_const <- as.factor(Edit_2019_2020$fin_const)

table(Edit_2019_2020$fin_const)

t.test(RC1 ~ fin_const, data = Edit_2019_2020)
t.test(RC2 ~ fin_const, data = Edit_2019_2020)


#HR constraints
Edit_2019_2020$HR_const <- dplyr::recode(Edit_2019_2020$I10R2C1, `3` = 0, `2` = 1, `1` = 1) 

Edit_2019_2020$HR_const <- as.factor(Edit_2019_2020$HR_const)

table(Edit_2019_2020$HR_const)

t.test(RC1 ~ HR_const, data = Edit_2019_2020)
t.test(RC2 ~ HR_const, data = Edit_2019_2020)

####

ggplot(data = Edit_2019_2020, aes(x = NOInnov, y = log_ventas_nal)) +
  stat_boxplot(geom = "errorbar", # Boxplot with error bars 
               width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors
               alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "Log Natural Ventas Nacional") +  # Continuous variable label
  scale_x_discrete(name = "Categoría de Innovación") +      # Group label
  ggtitle("Diferencia en Ventas") + # Plot title
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25)) 

t.test(log_ventas_nal ~ NOInnov, data = Edit_2019_2020)
t.test(ventas_nal ~ NOInnov, data = Edit_2019_2020)
t.test(ventas_nal ~ AmpliaInnov, data = Edit_2019_2020)
#Relación entre media de ventas nacionales innovadoras y no innovadoras
69988256/15124483
69988256-15124483


#RElación entre la diferencia de ventas nacionales promedio de innovaodras y no innovadoras y la inversión en ACTI promedio
(69988256-15124483)/mean(Edit_2019_2020$Total_ACTI, na.rm = TRUE)


ggplot(data = Edit_2019_2020, aes(x = NOInnov, y = log_export)) +
  stat_boxplot(geom = "errorbar", # Boxplot with error bars 
               width = 0.2) +
  geom_boxplot(fill = "#4271AE", colour = "#1F3552", # Colors
               alpha = 0.9, outlier.colour = "red") +
  scale_y_continuous(name = "Log Natural Ventas Nacional") +  # Continuous variable label
  scale_x_discrete(name = "Categoría de Innovación") +      # Group label
  ggtitle("Diferencia en Ventas") + # Plot title
  theme(axis.line = element_line(colour = "black", # Theme customization
                                 size = 0.25)) 
t.test(export ~ NOInnov, data = Edit_2019_2020)
t.test(export ~ AmpliaInnov, data = Edit_2019_2020)


10417341/ 4157767

(10417341-4157767)/mean(Edit_2019_2020$Total_ACTI, na.rm = TRUE)

mean(Edit_2019_2020$IV1R11C2)

####Explorations 

(Edit_2019_2020) %>% 
  count(I1R1C1N, I1R2C1N, I1R3C1N, I1R1C1M, I1R2C1M, I1R3C1M)

(Edit_2015_2016) %>% 
  count(I1R1C1N, I1R2C1N, I1R3C1N, I1R1C1M, I1R2C1M, I1R3C1M)


EDIT_ingresos_15_20$ln_RD_int <-log(EDIT_ingresos_15_20$RD_int+1)

EDIT_ingresos_15_20$ventas_nal_pc <- EDIT_ingresos_15_20$ventas_nal/EDIT_ingresos_15_20$Total_emp

EDIT_ingresos_15_20$ln_ventas_nal_pc <-log(EDIT_ingresos_15_20$ventas_nal_pc+1)
EDIT_ingresos_15_20$ln_ventas_nal_pc[EDIT_ingresos_15_20$ln_ventas_nal_pc=="NaN"]="NA"

EDIT_ingresos_15_20$inn_dum <- EDIT_ingresos_15_20$prod_inn>0

mod0 <- lm(ln_ventas_nal_pc ~ ln_RD_int, data = EDIT_ingresos_15_20[ln_ventas_nal_pc!="NA"], na.action = na.omit)
summary(mod0)

mod1 <- lm(ln_ventas_nal ~ ln_RD_int + EDIT_ingresos_15_20$inn_dum, data = EDIT_ingresos_15_20, na.action = na.omit)
summary(mod1)

mod2 <- lm(ln_ventas_nal ~ EDIT_ingresos_15_20$inn_dum, data = EDIT_ingresos_15_20, na.action = na.omit)
summary(mod2)

install.packages("stargazer")
library(stargazer)

stargazer(mod0,mod1, mod2, type = "text")


####

CIIU <- freq(EDIT_EAM_15_20$CIIU_2d, na.rm=TRUE)

EDIT_EAM_15_20$CIIU_2d <- substr(EDIT_EAM_15_20$CIIU4,start = 1, stop = 2)

EDIT_EAM_15_20$CIIU_2d

EDIT_EAM_15_20$RD_pc <-(EDIT_EAM_15_20$RD_int/EDIT_EAM_15_20$personal)
EDIT_EAM_15_20$RD_pc[EDIT_EAM_15_20$RD_pc==Inf] <- NA
EDIT_EAM_15_20$ln_RD_int <-log(EDIT_EAM_15_20$RD_int+1)

EDIT_EAM_15_20$ventas_nal_pc <- EDIT_EAM_15_20$ventas_nal/EDIT_EAM_15_20$personal
summary(EDIT_EAM_15_20$ventas_nal_pc)

EDIT_EAM_15_20$ventas_nal_pc[EDIT_EAM_15_20$ventas_nal_pc==Inf] <- NA
EDIT_EAM_15_20$ln_vent_nal_pc <-log(EDIT_EAM_15_20$ventas_nal_pc+1)

EDIT_EAM_15_20$inn_dum <- EDIT_EAM_15_20$prod_inn>0
EDIT_EAM_15_20$invent_pc <- EDIT_EAM_15_20$inventario/EDIT_EAM_15_20$personal
EDIT_EAM_15_20$invent_pc[EDIT_EAM_15_20$invent_pc==Inf] <- NA
EDIT_EAM_15_20$ln_invent_pc <-log(EDIT_EAM_15_20$invent_pc+1)

freq(EDIT_EAM_15_20$Temporal)

EDIT_EAM_15_20$Temporal_pc <-EDIT_EAM_15_20$personal
EDIT_EAM_15_20$Temporal_pc[EDIT_EAM_15_20$Temporal_pc==Inf] <- NA


EDIT_EAM_15_20 <- EDIT_EAM_15_20 %>%
  group_by(year, CIIU_2d) %>%
  mutate(
    sectorSales          = sum(ventas_nal, na.rm = TRUE),
    sectorSales_mean     = mean(ventas_nal, na.rm = TRUE),
    sectorSales_pc_mean    = mean(ventas_nal_pc, na.rm = TRUE),
    
    sectorInvent_mean    = mean(inventario, na.rm = TRUE),
    sectorInvent         = sum(inventario, na.rm = TRUE),
    sectorInvent_pc_mean    = mean(invent_pc, na.rm = TRUE),
    
    sectorRD_mean    = mean(RD_int, na.rm = TRUE),
    sectorRD         = sum(RD_int, na.rm = TRUE),
    sectorRD_pc_mean    = mean(RD_pc, na.rm = TRUE),
    
    sectorTemporal_mean    = mean(Temporal, na.rm = TRUE),
    sectorTemporal       = sum(Temporal, na.rm = TRUE),
    sectorTemporal_pc_mean    = mean(Temporal_pc, na.rm = TRUE),
    
  )

EDIT_EAM_15_20$log_ventas_nal_pc_C <- log(EDIT_EAM_15_20$ventas_nal_pc+1) - log(EDIT_EAM_15_20$sectorSales_pc_mean+1)
EDIT_EAM_15_20$log_invent_pc_C <- log(EDIT_EAM_15_20$invent_pc+1) - log(EDIT_EAM_15_20$sectorInvent_pc_mean+1)
EDIT_EAM_15_20$log_RD_pc_C <- log(EDIT_EAM_15_20$RD_pc+1) - log(EDIT_EAM_15_20$sectorRD_pc_mean+1)
EDIT_EAM_15_20$Temporal_pc_C <-  EDIT_EAM_15_20$Temporal_pc - EDIT_EAM_15_20$sectorTemporal_pc_mean

summary(EDIT_EAM_15_20$log_ventas_nal_pc_C)

hist(EDIT_EAM_15_20$log_ventas_nal_pc_C)


hist(EDIT_EAM_15_20$ventas_nal_pc)

#EDIT_EAM_15_20$RD_int[EDIT_EAM_15_20$RD_int==0]& 
#table (EDIT_EAM_15_20$inn_dum[EDIT_EAM_15_20$inn_dum==1])
#table(EDIT_EAM_15_20$TIPOLO)


mod0 <- lm(ventas_nal_pc ~ invent_pc , data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod0)

mod0a <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C , data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod0a)

mod1 <- lm(ventas_nal_pc ~ invent_pc + RD_pc, data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod1)

mod1a <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C + log_RD_pc_C , data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod1a)

mod2 <- lm(ventas_nal_pc ~ invent_pc + RD_pc + invent_pc*RD_pc, data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod2)

mod2a <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C + log_RD_pc_C + log_invent_pc_C*log_RD_pc_C, data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod2a)

stargazer(mod0, mod0a, mod1, mod1a, mod2, mod2a,type = "text")

EDIT_EAM_15_20$invent_pc_sqr <- EDIT_EAM_15_20$invent_pc*EDIT_EAM_15_20$invent_pc
EDIT_EAM_15_20$log_invent_pc_C_sqr <- EDIT_EAM_15_20$log_invent_pc_C*EDIT_EAM_15_20$log_invent_pc_C
mod3 <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C + log_invent_pc_C_sqr + log_RD_pc_C + log_invent_pc_C*log_RD_pc_C, data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod3)

EDIT_EAM_15_20$Temporal_pc_C_sqr <- EDIT_EAM_15_20$Temporal_pc_C*EDIT_EAM_15_20$Temporal_pc_C
EDIT_EAM_15_20$Temporal_pc_sqr <- EDIT_EAM_15_20$Temporal_pc*EDIT_EAM_15_20$Temporal_pc

mod4 <- lm(ventas_nal_pc ~ invent_pc + RD_pc + Temporal_pc, data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod4)

mod4a <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C + log_RD_pc_C + Temporal_pc_C, data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod4a)

mod4b <- lm(ventas_nal_pc ~ invent_pc + RD_pc + Temporal_pc + invent_pc*RD_pc, data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod4b)

mod4b.1 <- lm(ventas_nal_pc ~ invent_pc + invent_pc_sqr +RD_pc + Temporal_pc+ Temporal_pc_sqr + invent_pc*RD_pc, data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod4b.1)

mod4c <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C + log_RD_pc_C + Temporal_pc_C + log_invent_pc_C*log_RD_pc_C, data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod4c)

mod4d <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C+ log_invent_pc_C_sqr + log_RD_pc_C + Temporal_pc_C + log_invent_pc_C*log_RD_pc_C, data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod4d)

mod4e <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C+ log_invent_pc_C_sqr + log_RD_pc_C + Temporal_pc_C + log_invent_pc_C*log_RD_pc_C + log_RD_pc_C*Temporal_pc_C, data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod4e)

mod4f <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C+ log_invent_pc_C_sqr + log_RD_pc_C + Temporal_pc_C + Temporal_pc_C_sqr + log_invent_pc_C*log_RD_pc_C + log_RD_pc_C*Temporal_pc_C, data = EDIT_EAM_15_20, na.action = na.exclude)
summary(mod4f)

#####R&D NA's#####

EDIT_EAM_15_20_mod <- EDIT_EAM_15_20

freq(EDIT_EAM_15_20_mod$RD_int)

EDIT_EAM_15_20_mod$RD_int[is.na(EDIT_EAM_15_20_mod$RD_int)] <- 0


EDIT_EAM_15_20_mod$RD_pc <-(EDIT_EAM_15_20_mod$RD_int/EDIT_EAM_15_20_mod$personal)
EDIT_EAM_15_20_mod$RD_pc[EDIT_EAM_15_20_mod$RD_pc==Inf] <- NA
EDIT_EAM_15_20_mod$ln_RD_int <-log(EDIT_EAM_15_20_mod$RD_int+1)

EDIT_EAM_15_20_mod$ventas_nal_pc <- EDIT_EAM_15_20_mod$ventas_nal/EDIT_EAM_15_20_mod$personal
EDIT_EAM_15_20_mod$ventas_nal_pc[EDIT_EAM_15_20_mod$ventas_nal_pc==Inf] <- NA
EDIT_EAM_15_20_mod$ln_vent_nal_pc <-log(EDIT_EAM_15_20_mod$ventas_nal_pc+1)

EDIT_EAM_15_20_mod$inn_dum <- EDIT_EAM_15_20_mod$prod_inn>0
EDIT_EAM_15_20_mod$invent_pc <- EDIT_EAM_15_20_mod$inventario/EDIT_EAM_15_20_mod$personal
EDIT_EAM_15_20_mod$invent_pc[EDIT_EAM_15_20_mod$invent_pc==Inf] <- NA
EDIT_EAM_15_20_mod$ln_invent_pc <-log(EDIT_EAM_15_20_mod$invent_pc+1)

freq(EDIT_EAM_15_20_mod$Temporal)

EDIT_EAM_15_20_mod$Temporal_pc <-EDIT_EAM_15_20_mod$personal
EDIT_EAM_15_20_mod$Temporal_pc[EDIT_EAM_15_20_mod$Temporal_pc==Inf] <- NA


EDIT_EAM_15_20_mod <- EDIT_EAM_15_20_mod %>%
  group_by(year, CIIU_2d) %>%
  mutate(
    sectorSales          = sum(ventas_nal, na.rm = TRUE),
    sectorSales_mean     = mean(ventas_nal, na.rm = TRUE),
    sectorSales_pc_mean    = mean(ventas_nal_pc, na.rm = TRUE),
    
    sectorInvent_mean    = mean(inventario, na.rm = TRUE),
    sectorInvent         = sum(inventario, na.rm = TRUE),
    sectorInvent_pc_mean    = mean(invent_pc, na.rm = TRUE),
    
    sectorRD_mean    = mean(RD_int, na.rm = TRUE),
    sectorRD         = sum(RD_int, na.rm = TRUE),
    sectorRD_pc_mean    = mean(RD_pc, na.rm = TRUE),
    
    sectorTemporal_mean    = mean(Temporal, na.rm = TRUE),
    sectorTemporal       = sum(Temporal, na.rm = TRUE),
    sectorTemporal_pc_mean    = mean(Temporal_pc, na.rm = TRUE),
    
  )

EDIT_EAM_15_20_mod$log_ventas_nal_pc_C <- log(EDIT_EAM_15_20_mod$ventas_nal_pc+1) - log(EDIT_EAM_15_20_mod$sectorSales_pc_mean+1)
EDIT_EAM_15_20_mod$log_invent_pc_C <- log(EDIT_EAM_15_20_mod$invent_pc+1) - log(EDIT_EAM_15_20_mod$sectorInvent_pc_mean+1)
EDIT_EAM_15_20_mod$log_RD_pc_C <- log(EDIT_EAM_15_20_mod$RD_pc+1) - log(EDIT_EAM_15_20_mod$sectorRD_pc_mean+1)
EDIT_EAM_15_20_mod$Temporal_pc_C <-  EDIT_EAM_15_20_mod$Temporal_pc - EDIT_EAM_15_20_mod$sectorTemporal_pc_mean

#EDIT_EAM_15_20_mod$RD_int[EDIT_EAM_15_20_mod$RD_int==0]& 
#table (EDIT_EAM_15_20_mod$inn_dum[EDIT_EAM_15_20_mod$inn_dum==1])
#table(EDIT_EAM_15_20_mod$TIPOLO)


mod0 <- lm(ventas_nal_pc ~ invent_pc , data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod0)

mod0a <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C , data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod0a)

mod1 <- lm(ventas_nal_pc ~ invent_pc + RD_pc, data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod1)

mod1a <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C + log_RD_pc_C , data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod1a)

mod2 <- lm(ventas_nal_pc ~ invent_pc + RD_pc + invent_pc*RD_pc, data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod2)

mod2a <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C + log_RD_pc_C + log_invent_pc_C*log_RD_pc_C, data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod2a)

stargazer(mod0, mod0a, mod1, mod1a, mod2, mod2a,type = "text")

EDIT_EAM_15_20_mod$invent_pc_sqr <- EDIT_EAM_15_20_mod$invent_pc*EDIT_EAM_15_20_mod$invent_pc
EDIT_EAM_15_20_mod$log_invent_pc_C_sqr <- EDIT_EAM_15_20_mod$log_invent_pc_C*EDIT_EAM_15_20_mod$log_invent_pc_C
mod3 <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C + log_invent_pc_C_sqr + log_RD_pc_C + log_invent_pc_C*log_RD_pc_C, data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod3)

EDIT_EAM_15_20_mod$Temporal_pc_C_sqr <- EDIT_EAM_15_20_mod$Temporal_pc_C*EDIT_EAM_15_20_mod$Temporal_pc_C
EDIT_EAM_15_20_mod$Temporal_pc_sqr <- EDIT_EAM_15_20_mod$Temporal_pc*EDIT_EAM_15_20_mod$Temporal_pc

mod4 <- lm(ventas_nal_pc ~ invent_pc + RD_pc + Temporal_pc, data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod4)

mod4a <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C + log_RD_pc_C + Temporal_pc_C, data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod4a)

mod4b <- lm(ventas_nal_pc ~ invent_pc + RD_pc + Temporal_pc + invent_pc*RD_pc, data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod4b)

mod4b.1 <- lm(ventas_nal_pc ~ invent_pc + invent_pc_sqr +RD_pc + Temporal_pc+ Temporal_pc_sqr + invent_pc*RD_pc, data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod4b.1)

mod4b.2 <- lm(ventas_nal_pc ~ invent_pc + invent_pc_sqr +RD_pc + Temporal_pc+ Temporal_pc_sqr + invent_pc*RD_pc + invent_pc*Temporal_pc, data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod4b.2)

mod4c <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C + log_RD_pc_C + Temporal_pc_C + log_invent_pc_C*log_RD_pc_C, data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod4c)

mod4d <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C+ log_invent_pc_C_sqr + log_RD_pc_C + Temporal_pc_C + log_invent_pc_C*log_RD_pc_C, data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod4d)

mod4e <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C+ log_invent_pc_C_sqr + log_RD_pc_C + Temporal_pc_C + log_invent_pc_C*log_RD_pc_C + log_RD_pc_C*Temporal_pc_C, data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod4e)

mod4f <- lm(log_ventas_nal_pc_C ~ log_invent_pc_C+ log_invent_pc_C_sqr + log_RD_pc_C + Temporal_pc_C + Temporal_pc_C_sqr + log_invent_pc_C*log_RD_pc_C + log_RD_pc_C*Temporal_pc_C, data = EDIT_EAM_15_20_mod, na.action = na.exclude)
summary(mod4f)

stargazer(mod4b.2, mod4f, type = "text")



#Modelo conceptual inicial
Edit_2019_2020$coop_otremp <- recode(Edit_2019_2020$V3R1C1, `2` = 0) 

table(Edit_2019_2020$coop_otremp)
table(Edit_2019_2020$V3R1C1)

Edit_2019_2020$coop_provd <- recode(Edit_2019_2020$V3R2C1, `2` = 0) 
table(Edit_2019_2020$coop_provd)
table(Edit_2019_2020$coop_provd, Edit_2019_2020$coop_otremp)
Edit_2019_2020$coop_client <- recode(Edit_2019_2020$V3R3C1, `2` = 0) 
table(Edit_2019_2020$coop_client, Edit_2019_2020$coop_provd)

Edit_2019_2020$coop_compt <- recode(Edit_2019_2020$V3R4C1, `2` = 0) 
Edit_2019_2020$coop_agent <- recode(Edit_2019_2020$V3R5C1, `2` = 0)
Edit_2019_2020$coop_univ <- recode(Edit_2019_2020$V3R6C1, `2` = 0) 
table(Edit_2019_2020$coop_univ)
table(Edit_2019_2020$coop_client, Edit_2019_2020$coop_univ)
table(Edit_2019_2020$coop_provd, Edit_2019_2020$coop_univ)

Edit_2019_2020$coop_tdc <- recode(Edit_2019_2020$V3R7C1, `2` = 0) 
Edit_2019_2020$coop_autRC <- recode(Edit_2019_2020$V3R8C1, `2` = 0) 
Edit_2019_2020$coop_techpark <- recode(Edit_2019_2020$V3R9C1, `2` = 0) 
Edit_2019_2020$coop_prodcent <- recode(Edit_2019_2020$V3R10C1, `2` = 0) 
Edit_2019_2020$coop_ong <- recode(Edit_2019_2020$V3R11C1, `2` = 0) 
Edit_2019_2020$coop_gov <- recode(Edit_2019_2020$V3R12C1, `2` = 0) 
table(Edit_2019_2020$coop_gov) 
summary(Edit_2019_2020$III1R3C2>0)

max(Edit_2019_2020$III1R3C2, na.rm = TRUE)
hist(log(Edit_2019_2020$III1R3C2))

Edit_2019_2020$br_coop <- Edit_2019_2020$coop_otremp + Edit_2019_2020$coop_provd + Edit_2019_2020$coop_client + Edit_2019_2020$coop_compt + Edit_2019_2020$coop_agent + Edit_2019_2020$coop_univ + Edit_2019_2020$coop_tdc + Edit_2019_2020$coop_autRC + Edit_2019_2020$coop_techpark + Edit_2019_2020$coop_prodcent + Edit_2019_2020$coop_ong + Edit_2019_2020$coop_gov
table(Edit_2019_2020$br_coop)
hist(Edit_2019_2020$br_coop)

install.packages("systemfit")
library(systemfit)

attach(Edit_2019_2020)
#RC1= eficiencia en el uso de recursos
#RC2= orientación a la creación de valor

eq1 <- RC1 ~ br_coop
eq2 <- RC2 ~ br_coop

system <- list(eq1=eq1, eq2=eq2)

sur <- systemfit(system, method = "SUR", data = Edit_2019_2020)
summary(sur)

ols1 <- lm(RC1 ~ br_coop)
summary(ols1)

ols2 <- lm(RC2 ~ br_coop)
summary(ols2)

nrow(Edit_2019_2020)- sum(is.na(Edit_2019_2020$RC1))

#### escasez interna

attach(Edit_2019_2020)

eq1 <- RC1 ~ br_coop + escasez*br_coop
eq2 <- RC2 ~ br_coop + escasez*br_coop

system <- list(eq1=eq1, eq2=eq2)

sur <- systemfit(system, method = "SUR", data = Edit_2019_2020)
summary(sur)

ols1 <- lm(RC1 ~ br_coop + escasez*br_coop)
summary(ols1)

ols2 <- lm(RC2 ~ br_coop + escasez*br_coop)
summary(ols2)

nrow(Edit_2019_2020)- sum(is.na(Edit_2019_2020$RC1))

#### fin_const

attach(Edit_2019_2020)

eq1 <- RC1 ~ br_coop + fin_const*br_coop
eq2 <- RC2 ~ br_coop + fin_const*br_coop

system <- list(eq1=eq1, eq2=eq2)

sur <- systemfit(system, method = "SUR", data = Edit_2019_2020)
summary(sur)

ols1 <- lm(RC1 ~ br_coop + fin_const*br_coop)
summary(ols1)

ols2 <- lm(RC2 ~ br_coop + fin_const*br_coop)
summary(ols2)

nrow(Edit_2019_2020)- sum(is.na(Edit_2019_2020$RC1))

#### HR_const

attach(Edit_2019_2020)

eq1 <- RC1 ~ br_coop + HR_const*br_coop
eq2 <- RC2 ~ br_coop + HR_const*br_coop

system <- list(eq1=eq1, eq2=eq2)

sur <- systemfit(system, method = "SUR", data = Edit_2019_2020)
summary(sur)

ols1 <- lm(RC1 ~ br_coop + HR_const*br_coop)
summary(ols1)

ols2 <- lm(RC2 ~ br_coop + HR_const*br_coop)
summary(ols2)

nrow(Edit_2019_2020)- sum(is.na(Edit_2019_2020$RC1))

### todas las restricciones

attach(Edit_2019_2020)

eq1 <- RC1 ~ br_coop + escasez*br_coop + fin_const*br_coop + HR_const*br_coop
eq2 <- RC2 ~ br_coop + escasez*br_coop + fin_const*br_coop + HR_const*br_coop

system <- list(eq1=eq1, eq2=eq2)

sur <- systemfit(system, method = "SUR", data = Edit_2019_2020)
summary(sur)

ols1 <- lm(RC1 ~ br_coop + + escasez*br_coop + fin_const*br_coop + HR_const*br_coop)
summary(ols1)

ols2 <- lm(RC2 ~ br_coop + + escasez*br_coop + fin_const*br_coop + HR_const*br_coop)
summary(ols2)

####################

mod0 <- lm(ln_ventas_nal ~ ln_RD_int + inventario, data = EDIT_EAM_15_20, na.action = na.omit)
summary(mod0)

mod0 <- lm(ln_ventas_nal ~ ln_RD_int + inventario + inventario*ln_RD_int, data = EDIT_EAM_15_20, na.action = na.omit)
summary(mod0)

stargazer(mod0,type = "text")

mod1 <- lm(ln_ventas_nal ~ ln_RD_int + inventario + inventario*ln_RD_int + Temporal, data = EDIT_EAM_15_20, na.action = na.omit)
summary(mod1)

stargazer(mod0, mod1, type = "text")

mod2a <- lm(ln_ventas_nal ~ ln_RD_int + ln_inventario, data = EDIT_EAM_15_20, na.action = na.omit)
mod2 <- lm(ln_ventas_nal ~ ln_RD_int + ln_inventario + ln_inventario*ln_RD_int, data = EDIT_EAM_15_20, na.action = na.omit)
mod3 <- lm(ln_ventas_nal ~ ln_RD_int + ln_inventario + ln_inventario*ln_RD_int + Temporal, data = EDIT_EAM_15_20, na.action = na.omit)
stargazer(mod2a, mod2, mod3, type = "text")



EDIT_EAM_15_20$invent_cent <- EDIT_EAM_15_20$inventario-EDIT_EAM_15_20$Invent_prom
EDIT_EAM_15_20$invent_cent <- NULL

for(i in unique(df$ciiu3)) {
  df.ciiu3 <- df[df$ciiu3 == i, c("year", "ingresosOperacionales")]
  for(j in 1999:2014){
    df.ciiu3.window <- df.ciiu3[df.ciiu3$year >= j - 4 & df.ciiu3$year <= j, ]
    if(length(unique(df.ciiu3.window$year)) < 3) next
    model       <- lm(ingresosOperacionales ~ year, data = df.ciiu3.window)
    slope       <- as.numeric(model$coefficients[2])
    munificence <- slope / mean(df.ciiu3.window$ingresosOperacionales)
    seSlope     <- as.numeric(summary(model)$coefficients[ ,2][2])
    dynamism    <- seSlope / mean(df.ciiu3.window$ingresosOperacionales)
    regRolling  <- rbind(regRolling,
                         c(ciiu3       = i,
                           year        = j,
                           slope       = slope,
                           munificence = munificence,
                           seSlope     = seSlope,
                           dynamism    = dynamism
                         )
    )
  }
}
#tiene menú contextual

df <- df %>%
  mutate(
    avaSlack     = currentRatio / sectorCurrentRatio - 1,
    #recSlack     = recAccInventory / sectorRecAccInventory - 1,
    recSlack     = sga2sales / sectorSga2sales - 1,
    potSlack     = equity2debt / sectorEquity2debt - 1,
    
    avaSlack2    = avaSlack^2,
    recSlack2    = recSlack^2,
    #recSlack2    = recSlackSga^2,
    potSlack2    = potSlack^2,
    
    comHhi    = (1 / hhiC3) / 100,
    complexityCapint = (sectorCapint),
    complexityAssets = sectorFirmSizeAssets,
    
    comHhiXavaSlack = comHhi * avaSlack,
    comHhiXrecSlack = comHhi * recSlack,
    comHhiXpotSlack = comHhi * potSlack,
    
    comHhiXavaSlack2 = comHhi * avaSlack2,
    comHhiXrecSlack2 = comHhi * recSlack2,
    comHhiXpotSlack2 = comHhi * potSlack2
  )




EDIT_EAM_15_20 <- EDIT_EAM_15_20 %>%
  group_by(year, CIIU_2d) %>%
  mutate(
    sectorSales          = sum(ventas_nal, na.rm = TRUE),
    sectorSales_mean     = mean(ventas_nal, na.rm = TRUE),
    sectorSales_pc_mean    = mean(ventas_nal_pc, na.rm = TRUE),
    
    sectorInvent_mean    = mean(inventario, na.rm = TRUE),
    sectorInvent         = sum(inventario, na.rm = TRUE),
    sectorInvent_pc_mean    = mean(invent_pc, na.rm = TRUE),
    
    sectorEquity2debt      = mean(equity2debt, na.rm = TRUE),
    sectorSga2sales        = mean(sga2sales, na.rm = TRUE),
    
    sectorEbit             = mean(ebit, na.rm = TRUE),
    sectorRoa              = mean(roa, na.rm = TRUE),
    sectorCapint           = capint, #mean(capint, na.rm = TRUE),  # capint, #
    sectorFirmSizeAssets   = mean(firmSizeAssets, na.rm = TRUE),
    
    density                = n() / 1000,
    competSize             = log(sectorSalesC3 - ingresosOperacionales),
    
    firmSizeAssetsGroup     = cut(firmSizeAssets, breaks = 2, labels = c("Small firms", "Large firms")),
    firmSizeSalesGroup      = cut(firmSizeSales,  breaks = 2, labels = c("Small firms", "Large firms"))
  )


