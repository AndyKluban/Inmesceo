library(dplyr)
library(writexl)
library(tidyr)
library(ggplot2)
library(plotly)
library(dplyr)
library(stargazer)
library(readxl)

data <- read_excel("~/Desktop/Data.xlsx")
View(data)

sample_companies <- sample_n(tbl = data, size = 400)
head(sample_companies)
write_xlsx(sample_companies, 'sample_companies.xlsx')


#########################
#Geographic distribution#
#########################

#Let's work with dataset (24,000)
#Subset <- only company name + region
data1 <- data[,c(1,7)]
data1 <- na.omit(data1)
View(data1)
colnames(data1) <- c("Name", "Postcode")

data2 <- data1 %>%
  separate(Postcode, c("Area_region", "Street"), " ")
#NAs in 66 rows, deleting
data2 <- na.omit(data2)

data3 <- data2 %>%
  separate(Area_region, 
           into = c("Area", "Region"), 
           sep = "(?<=[A-Za-z])(?=[0-9])"
  )
data4 <- data3[,c(1,2)]

#Getting areas from https://ideal-postcodes.co.uk/guides/postcode-areas
areas <- read_excel("Areas.xlsx")
table(areas$Region)
scotland <- data.frame(matrix(nrow = 125, ncol = 0))
scotland$Postcode <- ifelse(areas$Region == 'Scotland', areas$`Postcode Area`,"")
scotland[!apply(scotland == "", 1, all), ]

East_Midlands <- data.frame(matrix(nrow = 125, ncol = 0))
East_Midlands$Postcode <- ifelse(areas$Region == 'East Midlands', areas$`Postcode Area`,"")
East_Midlands[!apply(East_Midlands == "", 1, all), ]

East_of_England <- data.frame(matrix(nrow = 125, ncol = 0))
East_of_England$Postcode <- ifelse(areas$Region == 'East of England', areas$`Postcode Area`,"")
East_of_England[!apply(East_of_England == "", 1, all), ]

North_East <- data.frame(matrix(nrow = 125, ncol = 0))
North_East$Postcode <- ifelse(areas$Region == 'North East', areas$`Postcode Area`,"")
North_East[!apply(North_East == "", 1, all), ]

North_West <- data.frame(matrix(nrow = 125, ncol = 0))
North_West$Postcode <- ifelse(areas$Region == 'North West', areas$`Postcode Area`,"")
North_West[!apply(North_West == "", 1, all), ]

South_East <- data.frame(matrix(nrow = 125, ncol = 0))
South_East$Postcode <- ifelse(areas$Region == 'South East', areas$`Postcode Area`,"")
South_East[!apply(South_East == "", 1, all), ]

South_West <- data.frame(matrix(nrow = 125, ncol = 0))
South_West$Postcode <- ifelse(areas$Region == 'South West', areas$`Postcode Area`,"")
South_West[!apply(South_West == "", 1, all), ]

Wales <- data.frame(matrix(nrow = 125, ncol = 0))
Wales$Postcode <- ifelse(areas$Region == 'Wales', areas$`Postcode Area`,"")
Wales[!apply(Wales == "", 1, all), ]

West_Midlands <- data.frame(matrix(nrow = 125, ncol = 0))
West_Midlands$Postcode <- ifelse(areas$Region == 'West Midlands', areas$`Postcode Area`,"")
West_Midlands[!apply(West_Midlands == "", 1, all), ]

#Getting regions
data4$Region <- ifelse(data4$Area == "AB" | data4$Area == "DD"| data4$Area == "DG" |data4$Area == "EH"| 
                         data4$Area =="FK" |data4$Area == "G"  |data4$Area == "HS" |data4$Area == "IV" |data4$Area == "KA"| 
                         data4$Area =="KW" |data4$Area =="KY" |data4$Area =="ML" |data4$Area =="PA" |data4$Area =="PH" |
                         data4$Area =="TD" |data4$Area =="ZE", "Scotland",
                       ifelse(data4$Area == "GY" | data4$Area == "JE", "Channel Islands", 
                              ifelse(data4$Area == "PE", "East England",
                                     ifelse(data4$Area == "DE" | data4$Area =="DN" |data4$Area =="LE" |
                                              data4$Area =="LN" |data4$Area =="NG" |data4$Area =="S" , "East Midlands",
                                            ifelse(data4$Area == "AL" | data4$Area == "CB"| data4$Area == "CM" |
                                                     data4$Area == "CO" |data4$Area == "HP"|
                                                     data4$Area == "IP" |data4$Area == "LU" |data4$Area == "NR" |data4$Area == "SG" |
                                                     data4$Area == "SS", "East of England",
                                                   ifelse(data4$Area == "IM", "Isle of Man",
                                                          ifelse(data4$Area == "DH" |data4$Area == "DL" |data4$Area == "HG" |data4$Area == "HU" |
                                                                   data4$Area == "LS" |data4$Area == "NE"| data4$Area == "SR"| data4$Area == "TS"|
                                                                   data4$Area == "WF"| data4$Area == "YO", "North East",
                                                                 ifelse(data4$Area == "BB" |data4$Area =="BD" |data4$Area =="BL" |data4$Area =="CA"|
                                                                          data4$Area =="CH" |data4$Area =="CW" |data4$Area =="FY" |data4$Area =="HD" |
                                                                          data4$Area =="HX" |data4$Area =="L"|
                                                                          data4$Area =="LA"| data4$Area =="M" | data4$Area =="OL"| data4$Area =="PR" |data4$Area =="SK"|
                                                                          data4$Area =="WA" |data4$Area =="WN", "North West",
                                                                        ifelse(data4$Area == "BT", "Northern Ireland",
                                                                               ifelse(data4$Area == "BN" |data4$Area == "CT" |data4$Area == "GU" |data4$Area == "ME"| data4$Area == "MK"|
                                                                                        data4$Area == "OX" |data4$Area == "PO" |data4$Area == "RG" |data4$Area == "RH" |
                                                                                        data4$Area == "SL" |data4$Area == "SO" |data4$Area == "TN", "South East",
                                                                                      ifelse(data4$Area == "BA" |data4$Area == "BH" |data4$Area == "BS" |data4$Area == "DT"| data4$Area == "EX"|
                                                                                               data4$Area == "GL"| data4$Area == "PL"| data4$Area == "SN"|data4$Area ==  "SP"| data4$Area == "TA"|
                                                                                               data4$Area == "TQ"| data4$Area == "TR", "South West",
                                                                                             ifelse(data4$Area == "CF" | data4$Area == "LD" | data4$Area == "LL" | data4$Area == "NP" |
                                                                                                      data4$Area == "SA" | data4$Area == "SY", "Wales",
                                                                                                    ifelse(data4$Area == "B" | data4$Area == "CV" |data4$Area == "DY"| data4$Area == "HR"|data4$Area ==  "NN"|
                                                                                                             data4$Area ==  "ST"|data4$Area ==  "TF" |data4$Area == "WR" |
                                                                                                             data4$Area == "WS"| data4$Area == "WV", "West Midlands", "Greater London"
                                                                                             )))))))))))))


table(data4$Region)

#Plotting
data5 <- data4[,c(1,3)]
data5$Count <- 1
View(data5)
#Number of companies by region overall
data6 <- data5 %>% group_by(Region) %>%
  summarize_at(vars(Count), function(x) sum(x))
sum(data6$Count)

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
          "#F8766D","#CD9600","#619CFF","#00BFC4"
          )

ggplot(data6, aes(x=reorder(Region, Count), y=Count, fill=Region)) +
  geom_bar(stat="identity", colour="black") +
  xlab("UK region") + ylab("Number of companies") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 22)),
        axis.title.x = element_text(margin = margin(t=-10)),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values=cbp1) + 
  geom_text(aes(label=Count), vjust=-0.5, colour="black",
            position=position_dodge(.9), size=3)


#########################
#Size of companies (emp)#
#########################
View(data)
data7 <- data[,c(1,12)]
data8 <- na.omit(data7)
colnames(data8) <- c("Name", "Size_empl")
data8$Size <- ifelse(data8$Size_empl >= 0 & data8$Size_empl < 10, "Small (0-10)", 
                     ifelse(data8$Size_empl >= 10 & data8$Size_empl < 100, "Medium (10-100)","Large (100-1,000)"
                            ))

#Plotting
data9 <- data8[,c(1,3)]
data9$Count <- 1
sum(data9$Count)
#View(data10)
#Number of companies by region overall
data10 <- data9 %>% group_by(Size) %>%
  summarize_at(vars(Count), function(x) sum(x))
sum(data10$Count)

cbp2 <- c("#CD9600", "#56B4E9", "#009E73"
)

# Make the pie chart
ggplot(data10, aes(x = "", y = Count, fill = Size)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values=cbp2) +
  ylab("Training providers by number of employees (N =18,149) ") + 
  xlab("")



#################################
#WORKING WITH RANDOM DATA SAMPLE#
#################################
df <- read_excel("~/Desktop/sample_companies2.xlsx")
View(df)

df1 <- df[,-c(2,3,4,5,6,8,9,10,40,11,12,13)]
colnames(df1) <- c("Name", "Postcode", "Ind_Energy", "Ind_Construction", "Ind_Technology", "Ind_Consumer", "Ind_Law",
                   "Ind_Hospitality", "Ind_Agriculture", "Ind_Education_Academics", "Ind_Healthcare", "Ind_Business_Services",
                   "Ind_Housing_Property", "Ind_Social_enterprise_NPO", "Ind_Aerospace",
                   "Ind_Media", "Mode_Online", "Mode_Classroom", "Mode_Blended", "Mode_V_ILT",
                   "Skill_Health_safety", "Skill_Prof_tech", "Skill_Manage_org", "Skill_Functional", 
                   "Skill_HR_personal", "Skill_Language", "Skill_IT", "Non_grata")
df2 <- df1
df2[is.na(df2)]<- "No"

df3 <- df2
View(df3)
names(df3)
df3$Ind_Energy <- ifelse(df3$Ind_Energy == "Yes" | df3$Ind_Energy == "yes", 1, 0)
df3$Ind_Construction <- ifelse(df3$Ind_Construction == "Yes" | df3$Ind_Construction == "yes", 1, 0)
df3$Ind_Technology <- ifelse(df3$Ind_Technology == "Yes" | df3$Ind_Technology == "yes", 1, 0)
df3$Ind_Consumer <- ifelse(df3$Ind_Consumer == "Yes" | df3$Ind_Consumer == "yes", 1, 0)
df3$Ind_Law <- ifelse(df3$Ind_Law == "Yes" | df3$Ind_Law == "yes", 1, 0)
df3$Ind_Hospitality <- ifelse(df3$Ind_Hospitality == "Yes" | df3$Ind_Hospitality == "yes", 1, 0)
df3$Ind_Agriculture <- ifelse(df3$Ind_Agriculture == "Yes" | df3$Ind_Agriculture == "yes", 1, 0)
df3$Ind_Education_Academics <- ifelse(df3$Ind_Education_Academics == "Yes" | df3$Ind_Education_Academics == "yes", 1, 0)
df3$Ind_Healthcare <- ifelse(df3$Ind_Healthcare == "Yes" | df3$Ind_Healthcare == "yes", 1, 0)
df3$Ind_Business_Services <- ifelse(df3$Ind_Business_Services == "Yes" | df3$Ind_Business_Services == "yes", 1, 0)
df3$Ind_Housing_Property <- ifelse(df3$Ind_Housing_Property == "Yes" | df3$Ind_Housing_Property == "yes", 1, 0)
df3$Ind_Social_enterprise_NPO <- ifelse(df3$Ind_Social_enterprise_NPO == "Yes" | df3$Ind_Social_enterprise_NPO == "yes", 1, 0)
df3$Ind_Aerospace <- ifelse(df3$Ind_Aerospace == "Yes" | df3$Ind_Aerospace == "yes", 1, 0)
df3$Ind_Media <- ifelse(df3$Ind_Media == "Yes" | df3$Ind_Media == "yes", 1, 0)
df3$Mode_Online <- ifelse(df3$Mode_Online == "Yes" | df3$Mode_Online == "yes", 1, 0)
df3$Mode_Classroom <- ifelse(df3$Mode_Classroom == "Yes" | df3$Mode_Classroom == "yes", 1, 0)
df3$Mode_Blended <- ifelse(df3$Mode_Blended == "Yes" | df3$Mode_Blended == "yes", 1, 0)
df3$Mode_V_ILT <- ifelse(df3$Mode_V_ILT == "Yes" | df3$Mode_V_ILT == "yes", 1, 0)
df3$Skill_Health_safety <- ifelse(df3$Skill_Health_safety == "Yes" | df3$Skill_Health_safety == "yes", 1, 0)
df3$Skill_Prof_tech <- ifelse(df3$Skill_Prof_tech == "Yes" | df3$Skill_Prof_tech == "yes", 1, 0)
df3$Skill_Manage_org <- ifelse(df3$Skill_Manage_org == "Yes" | df3$Skill_Manage_org == "yes", 1, 0)
df3$Skill_Functional <- ifelse(df3$Skill_Functional == "Yes" | df3$Skill_Functional == "yes", 1, 0)
df3$Skill_HR_personal <- ifelse(df3$Skill_HR_personal == "Yes" | df3$Skill_HR_personal == "yes", 1, 0)
df3$Skill_Language <- ifelse(df3$Skill_Language == "Yes" | df3$Skill_Language == "yes", 1, 0)
df3$Skill_IT <- ifelse(df3$Skill_IT == "Yes" | df3$Skill_IT == "yes", 1, 0)
df3$Non_grata <- ifelse(df3$Non_grata == "Yes" | df3$Non_grata == "yes", 1, 0)

Relevant_companies_total <- count(df3) - sum(df3$Non_grata)

########
#Skills#
########
View(df3)
names(df3)

df4<- df3[,c(1,21,22,23, 24, 25, 26, 27)]
df4 <- na.omit(df4)
names(df4)
View(df4)
#From sum info, we can make graph of number of companies, training each skill
df4 <- df4[,-1]
colSums(df4)

df5 <- data.frame(colSums(df4))
Relevant_companies_total <- count(df3) - sum(df3$Non_grata)
Relevant_companies_total
df5$Relative_freq <- df5$colSums.df4./237
df5$Skills <- c("Health safety", "Prof&tech", "Management",
                "Functional learning", "Human Resources", "Language", "IT")
#View(df5)

#Plot
cbp3 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
          "#F8766D","#CD9600","#619CFF","#00BFC4"
)

names(df5)
#Bar
ggplot(df5, aes(x=reorder(Skills, Relative_freq), y=Relative_freq, fill=Skills)) +
  geom_bar(stat="identity", colour="black") +
  xlab("Skill") + ylab("Share of companies") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 22)),
        axis.title.x = element_text(margin = margin(t=-15)),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values=cbp3) 

# + geom_text(aes(label=Relative_freq), vjust=-0.5, colour="black",
#            position=position_dodge(.9), size=3)


##########
#Industry#
##########

#From sum info, we can make graph of number of companies, training in each indusrty

names(df3)

df6<- df3[,c(1,3:16)]
df6 <- na.omit(df6)
names(df6)
#View(df6)
#From sum info, we can make graph of number of companies, training each skill
df6 <- df6[,-1]
colSums(df6)

df7 <- data.frame(colSums(df6))
Relevant_companies_total <- count(df3) - sum(df3$Non_grata)
Relevant_companies_total
df7$Relative_freq <- df7$colSums.df6./237
df7$Industry <- c("Energy", "Construction", "Technology",
                "Consumer", "Law", "Hospitality ", "Agriculture",
                "Academics", "Healthcare ", "Business",
                "Property", "NPO", "Aerospace",
                "Media")
#View(df7)

#Plot
cbp4 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
          "#F8766D","#CD9600","#619CFF","#00BFC4",
          "#000000", 	"#ABABAB"
)

# Make the pie chart
ggplot(df7, aes(x=reorder(Industry, Relative_freq), y=Relative_freq, fill=Industry)) +
  geom_bar(stat="identity", colour="black") +
  xlab("Industry") + ylab("Share of companies") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 22)),
        axis.title.x = element_text(margin = margin(t=-10)),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values=cbp4) 

# + geom_text(aes(label=Relative_freq), vjust=-0.5, colour="black",
#            position=position_dodge(.9), size=3)


ggplot(df7, aes(x=Relative_freq, y=reorder(Industry,Relative_freq))) +
  geom_point(size=3) +                        # Use a larger dot
  theme_bw() +
  xlab("Share of companies") + ylab("Industry") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="darkblue", linetype="dashed")) +
  geom_point(size=3, colour="coral2")



#######
#Modes#
#######
names(df3)

df8<- df3[,c(1,17:20)]
df8 <- na.omit(df8)
names(df8)
#View(df8)
#From sum info, we can make graph of number of companies, training each skill
df8 <- df8[,-1]
colSums(df8)

df9 <- data.frame(colSums(df8))
Relevant_companies_total <- count(df3) - sum(df3$Non_grata)
Relevant_companies_total
df9$Relative_freq <- df9$colSums.df8./237
df9$Mode <- c("Online", "Classroom", "Blended", "V-ILT")
#View(df9)

#Plot
cbp4 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
          "#F8766D","#CD9600","#619CFF","#00BFC4",
          "#000000", 	"#ABABAB"
)

ggplot(df9, aes(x=reorder(Mode, Relative_freq), y=Relative_freq, fill=Mode)) +
  geom_bar(stat="identity", colour="black") +
  xlab("Learning mode") + ylab("Share of companies") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 22)),
        axis.title.x = element_text(margin = margin(t=-10)),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values=cbp4) 

# + geom_text(aes(label=Relative_freq), vjust=-0.5, colour="black",
#            position=position_dodge(.9), size=3)

###########
#Non grata#
###########
names(df3)

df10<- df3[,c(1,28)]
df10 <- na.omit(df10)
names(df10)
#View(df8)
#From sum info, we can make graph of number of companies, training each skill
df10 <- df10[,-1]
colSums(df10)
Company_relevance <- c("Relevant", "Not relevant")
Company_relevance_num <- c(237, 55)
df11 <- data.frame(Company_relevance,Company_relevance_num)
df11$Company_relevance_rel <- c(237/292,55/292)
View(df11)

#Plot
cbp5 <- c("#009E73",
           "#0072B2", "#D55E00", "#CC79A7",
          "#F8766D","#CD9600","#619CFF","#00BFC4",
          "#000000", 	"#ABABAB"
)

# Make the pie chart
ggplot(df11, aes(x = "", y = Company_relevance_rel, fill = Company_relevance)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values=cbp4) +
  ylab("Companies relevance") + 
  xlab("")

ggplot(df11, aes(x = "", y = Company_relevance_rel)) +
  geom_col(aes(fill = Company_relevance), width = 1, color = NA) +
  labs(x = "", y = "Companies' relevance") +
  #coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Set1", name = "Season:") +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank())+
  scale_fill_manual(values=cbp5)



 