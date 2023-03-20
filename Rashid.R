# Importing data to R
library(readxl)
mydata<-read_excel("C:/Users/User/Desktop/RR data (1).xlsx")

#____________________________% removal for individual componet in @ plant__________________________________________ 

# installing and loading libraries
#install.packages("dplyr")
library(magrittr)
library(dplyr)


#Nzoia_EFF with highest detection 
Nzoia_EFF<-mydata %>% filter(Nzoia_EFF>=200) %>% select(WWTPs,Nzoia_EFF)
Nzoia_EFF

#Nzoia
#Nzoia_INF with highest detection > 200 
Nzoia<-mydata %>% filter (Nzoia_INF >= 200) %>% 
  select (Compound_class,WWTPs,Nzoia_INF,Nzoia_EFF)
Nzoia

# % removal for @ chemical in Nzoia
per_removal<- ((Nzoia$Nzoia_INF - Nzoia$Nzoia_EFF)/ Nzoia$Nzoia_INF) * 100
per_removal_perchem<- data.frame(Nzoia$Compound_class,Nzoia$WWTPs,Nzoia$Nzoia_INF,Nzoia$Nzoia_EFF, per_removal)
library(knitr)
knitr::kable(per_removal_perchem)

# Average concentration & STD concentration for @ compound Class in Nzoia
Nzoia_Summary<-Nzoia %>% group_by(Compound_class) %>% 
  summarise(average_INF = mean(Nzoia_INF), 
            std_INF = sd(Nzoia_INF),
            average_EFF = mean(Nzoia_EFF),
            std_EFF = sd(Nzoia_EFF))
knitr::kable(Nzoia_Summary)

#% removal Efficiency for @ compound class in Nzoia
Nzoia_Efficiency<- Nzoia %>% group_by(Compound_class) %>%
  summarise(percentage_Efficiency = ((sum(Nzoia_INF)-sum(Nzoia_EFF))/sum(Nzoia_INF)) * 100)
  knitr::kable(Nzoia_Efficiency)


#DLK
DLK<-mydata %>% filter (DLK_INF >= 200) %>% select (Compound_class,WWTPs,DLK_INF,DLK_EFF)
DLK
#% Removal for DLK
percentage_removal<- ((DLK$DLK_INF - DLK$DLK_EFF)/ DLK$DLK_INF) * 100
per_removal_perchem<- data.frame(DLK$WWTPs,percentage_removal)
knitr::kable(per_removal_perchem)


# Average concentration & STD concentration for @ compound Class in DLK
DLK_Summary<-DLK %>% group_by(Compound_class) %>% 
  summarise(average_INF = mean(DLK_INF), 
            std_INF = sd(DLK_INF),
            average_EFF = mean(DLK_EFF),
            std_EFF = sd(DLK_EFF))
knitr::kable(DLK_Summary)

#% removal efficiency for @ compound class in DLK
DLK_Efficiency<- DLK %>% group_by(Compound_class) %>%
  summarise(percentage_Efficiency = ((sum(DLK_INF)-sum(DLK_EFF))/sum(DLK_INF)) * 100)
knitr::kable(DLK_Efficiency)

#Moi
Moi<-mydata %>% filter (Moi_INF >= 200) %>% select (Compound_class,WWTPs,Moi_INF,Moi_EFF)
# % Removal for MOi for @ chem
percentage_removal<- ((Moi$Moi_INF - Moi$Moi_EFF)/ Moi$Moi_INF) * 100
per_removal_perchem<- data.frame(Moi$WWTPs,percentage_removal)
knitr::kable(per_removal_perchem)

# Average concentration & STD concentration for @ compound Class in Moi
Moi_Summary<-Moi %>% group_by(Compound_class) %>% 
  summarise(average_INF = mean(Moi_INF), 
            std_INF = sd(Moi_INF),
            average_EFF = mean(Moi_EFF),
            std_EFF = sd(Moi_EFF))
knitr::kable(Moi_Summary)

#% removal efficiency for @ compound class in DLK
Moi_Efficiency<- Moi %>% group_by(Compound_class) %>%
  summarise(percentage_Efficiency = ((sum(Moi_INF)-sum(Moi_EFF))/sum(Moi_INF)) * 100)
knitr::kable(Moi_Efficiency)

#Eldo
Eldo<-mydata %>% filter (Eldo_INF >= 200) %>% select (Compound_class,WWTPs,Eldo_INF,Eldo_EFF)
#% Removal for @ chem in Eldo
percentage_removal<- ((Eldo$Eldo_INF - Eldo$Eldo_EFF)/ Eldo$Eldo_INF) * 100
per_removal_perchem<- data.frame(Eldo$WWTPs,percentage_removal)
knitr::kable(per_removal_perchem)

# Average concentration & STD concentration for @ compound Class in Eldo
Eldo_Summary<-Eldo %>% group_by(Compound_class) %>% 
  summarise(average_INF = mean(Eldo_INF), 
            std_INF = sd(Eldo_INF),
            average_EFF = mean(Eldo_EFF),
            std_EFF = sd(Eldo_EFF))
knitr::kable(Eldo_Summary)

#% removal efficiency for @ compound class in DLK
Eldo_Efficiency<- Eldo %>% group_by(Compound_class) %>%
  summarise(percentage_Efficiency = ((sum(Eldo_INF)-sum(Eldo_EFF))/sum(Eldo_INF)) * 100)
knitr::kable(Eldo_Efficiency)
