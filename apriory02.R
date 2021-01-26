#Analyse global terrorist attack data set to see association of terrorist organisation with type of attack, firearms used and target.

library("readxl")

library("tidyverse")

library(arules)


#Loading database
terrorist.attack <- read_xlsx("globalterrorism_2001-2018.xlsx")

#Filtering columns needed for the project and saved to new data frame
terrorist.organisation <- filter(terrorist.attack[,c("attacktype1_txt","targtype1_txt","weaptype1_txt","gname")])

summary(terrorist.organisation)

#Converts data type to factor
terrorist.organisation <- as.data.frame(sapply(terrorist.organisation, as.factor))

#Removing rows with unknown data
terrorist.organisation <- terrorist.organisation %>% filter(attacktype1_txt != "Unknown")
terrorist.organisation <- terrorist.organisation %>% filter(targtype1_txt != "Unknown" )
terrorist.organisation <- terrorist.organisation %>% filter(weaptype1_txt != "Unknown" )  
terrorist.organisation <- terrorist.organisation %>% filter(gname != "Unknown" )

#Finding top 10 terrorist organizations based on numbe of terror attacks.
top.terror.organisation.top10 <- top_n(terrorist.organisation %>% group_by(gname) %>% summarise(n=n()) %>% arrange(desc(n)),10)
#convert top terrorist organisation in to vector for filtering
top.terror.organisation.top10 <- as.vector(top.terror.organisation.top10$gname)


#Subsetting dataset terrorist.organisation to include only top 10 terror organization

top.terror.organisation <- terrorist.organisation %>% filter(gname %in% c(top.terror.organisation.top10))

summary(top.terror.organisation)

#Write top.terror.organisation data frame in to a csv file

write.csv(top.terror.organisation,"C:/Users/SANTHOSH/Documents/apriory.csv",row.names = FALSE)

#Read csv file in to apriory.data data frame and convert in to transaction type to run apriori algorithm

apriori.data <- read.transactions("C:/Users/SANTHOSH/Documents/apriory.csv",format = 'basket',sep = ',')

summary(apriori.data)

apriori.rules <- apriori(apriori.data,parameter = list(sup=0.10,conf=0.001))

inspect(sort(apriori.rules, by = "lift"))


