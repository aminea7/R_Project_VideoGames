   #Importing datasets
vg_df = read.csv("resources/videogamesales_rates.csv", header = TRUE, sep = ",")
data = read.csv("resources/videogamesales_rates.csv", header = TRUE, sep = ",")

    #Importing packaging
require("ggplot2")
require("plyr")
library("RColorBrewer")
library("scales")
#http://www.sthda.com/english/wiki/colors-in-r

          #######       Question 1: Best Genre each year, in the different regions + in the World, with its sales   #######
          #######                                                                                                   #######
          #################################################################################################################

#Reshaping the data frame, removing the NA and games published after 2016 as there are few, and before 1994 so that the plot still comprehensible
vg_df_year=vg_df
vg_df_year = vg_df_year[vg_df_year$Year_of_Release !="N/A",]
vg_df_year[, 3] <- as.numeric(as.character( vg_df_year[, 3] ))
vg_df_year = vg_df_year[vg_df_year$Year_of_Release <="2015",]
vg_df_year = vg_df_year[vg_df_year$Year_of_Release >="1994",]

#World: For each year, each genre, the total sales in the world, and then keeping the best genre (in terms of sales) + its sales
global_genre_sales = ddply(vg_df_year,.(Year_of_Release,Genre),summarize,TotalSales = sum(Global_Sales))
global_genre = ddply(global_genre_sales,.(Year_of_Release),function(x) x[which.max(x$TotalSales),])

#North America (The same as before)
na_genre_sales = ddply(vg_df_year,.(Year_of_Release,Genre),summarize,TotalSales = sum(NA_Sales))
na_genre = ddply(na_genre_sales,.(Year_of_Release),function(x) x[which.max(x$TotalSales),])
na_genre["Region"] <- "NA"

#Europe  (The same as before)
eu_genre_sales = ddply(vg_df_year,.(Year_of_Release,Genre),summarize,TotalSales = sum(EU_Sales))
eu_genre = ddply(eu_genre_sales,.(Year_of_Release),function(x) x[which.max(x$TotalSales),])
eu_genre["Region"] <- "EU"

#Japan  (The same as before)
jp_genre_sales = ddply(vg_df_year,.(Year_of_Release,Genre),summarize,TotalSales = sum(JP_Sales))
jp_genre = ddply(jp_genre_sales,.(Year_of_Release),function(x) x[which.max(x$TotalSales),])
jp_genre["Region"] <- "JP"

#Regrouping Europe, Japan, North America
comparaison_genre = rbind(jp_genre,eu_genre,na_genre)

       #Plot the comparaison between Europe, Japan, North America
qplot() + geom_bar(data=comparaison_genre, aes(x=Year_of_Release, y=TotalSales, fill = Genre), colour="black", stat = "identity") + 
  geom_text(data=comparaison_genre,aes(x = Year_of_Release, y = TotalSales,label =Region),  position=position_stack(vjust=0.8)) + 
  labs(title = "\nBest Genre Sales in North America - Europe - Japan, from 1994 to 2015\n", x = "Year", y = "Sales (in Millions)") +
  scale_x_continuous(breaks=1994:2015)+geom_point() + 
  scale_y_continuous(breaks=seq(0, 200, by = 20))+geom_point() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Set1")+  
  theme(text = element_text(size = 18))

####### Bonus: (Not in the report)

    #Plot the best genre sales in the World
ggplot() + geom_bar(data=global_genre, aes(x=Year_of_Release, y=TotalSales,color=Genre,fill=Genre), colour="black", stat = "identity") + 
  labs(title = "\nBest Genre Sales in the World, from 1994 to 2015\n", x = "Year", y = "Sales (in Millions)") +
  scale_x_continuous(breaks=1994:2015)+geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Set1")

#Plot for North America
ggplot() + geom_bar(data=na_genre, aes(x=Year_of_Release, y=TotalSales,color=Genre,fill=Genre), colour="black", stat = "identity") + 
  labs(title = "\nBest Genre Sales in North America, from 1994 to 2015\n", x = "Year", y = "Sales (in Millions)") + 
  scale_x_continuous(breaks=1994:2015)+geom_point() + 
  theme(plot.title = element_text(hjust = 0.5))
#Plot for Europe
ggplot() + geom_bar(data=eu_genre, aes(x=Year_of_Release, y=TotalSales,color=Genre,fill=Genre), colour="black", stat = "identity") + 
  labs(title = "\nBest Genre Sales in Europe, from 1994 to 2015\n", x = "Year", y = "Sales (in Millions)") +
  scale_x_continuous(breaks=1994:2015)+geom_point() +
  theme(plot.title = element_text(hjust = 0.5))
#Plot for Japan
ggplot() + geom_bar(data=jp_genre, aes(x=Year_of_Release, y=TotalSales,color=Genre,fill=Genre), colour="black", stat = "identity") + 
  labs(title = "\nBest Genre Sales in Japan, from 1994 to 2015\n", x = "Year", y = "Sales (in Millions)") +
  scale_x_continuous(breaks=1994:2015)+geom_point() +
  theme(plot.title = element_text(hjust = 0.5))

#######                                  Question 2: Which the best console ?                             #######
#######                                                                                                   #######
#################################################################################################################

#Reshaping a new dataframe with all the games having users notes, removing the NA or others empty values
vg_df_marks = vg_df[, c("Name","Platform","User_Score","Critic_Score","Year_of_Release")]
vg_df_marks = vg_df_marks[vg_df_marks$Year_of_Release !="N/A",]
vg_df_marks = subset(vg_df_marks, (User_Score != "")  & (User_Score !="tbd") & (User_Score!="NA"))
vg_df_marks[, 3] <- as.numeric(as.character( vg_df_marks[, 3] ))

#For each platform, counting the nombre of games with a rate >=8/10, then keeping the top 10 platforms following this criteria 
#Per platform: Average games + percentage of the best games
count_plateform = ddply(vg_df_marks,.(Platform),summarize, Total=sum(User_Score>=8), AverageMarks=mean(User_Score))
count_plateform = arrange(count_plateform,desc(Total))[1:10,]
count_plateform["Percentage"]=signif((count_plateform$Total/sum(count_plateform$Total))*100,3)

#Plot for percentage of prefered games
ggplot(count_plateform, aes(x=1, y=Total,fill=Platform)) +
  geom_col(position = 'stack',colour="black") +
  geom_text(aes(label = paste(Percentage,'%\n(',signif(AverageMarks,3),')')), position = position_stack(vjust = 0.5),size=4.4) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "\nPlateforms having the most prefered games, from 1990 to 2016") + #, x = "Year", y = "Sales (in Millions)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") +
  theme(text = element_text(size = 18))


#######                                  Question 3: Who is the developper ?                              #######
#######                                                                                                   #######
#################################################################################################################

# "nettoyer" le dataset
data_naYear=subset(data,Year_of_Release != 'N/A')
data_naYear$Year_of_Release = as.numeric(as.character(data_naYear$Year_of_Release))
data_less2016= subset(data_naYear,Year_of_Release <= 2016 & Year_of_Release >= 2000)
data_naDev=subset(data_less2016,Developer != '')
data_naUserScore=subset(data_naDev,User_Score != '' & User_Score != 'tbd')
data_naUserScore$User_Score = as.numeric(as.character(data_naUserScore$User_Score))

# prendre les 20 développeurs qui produissent le plus
nbDev=ddply(data_naUserScore,.(Developer),summarize,nb=length(Developer))
nbDev=arrange(nbDev,desc(nb))
nbDev = head(nbDev,n=20)
dataDev=subset(data_naUserScore,data_naUserScore$Developer %in% nbDev$Developer)
# Calculer la moyenne des notes des utilisateurs par jeu et les classer par année
bestDev = ddply(dataDev,.(Year_of_Release,Developer),summarize,moyenneScore =mean(User_Score))
bestDev = arrange(bestDev,Year_of_Release,desc(moyenneScore))
bestDev = by(bestDev, bestDev["Year_of_Release"], head, n=3)
bestDev = Reduce(rbind, bestDev) #convert to a dataframe
# Affichage de l'histogramme
colourCount = length(unique(bestDev$Developer))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
ggplot(bestDev,aes(Year_of_Release,moyenneScore))+
  geom_bar(stat = "identity",colour="black", position = "dodge",width = 0.7,aes(fill=Developer))+
  coord_cartesian(ylim = c(6.5, 9.5))+
  scale_fill_manual(values = getPalette(colourCount))+
  scale_x_continuous(labels = as.character(bestDev$Year_of_Release), breaks = bestDev$Year_of_Release)+
  theme(axis.text.x = element_text(angle=65, vjust=0.5))+
  labs(title ="Top 3 best developer according to User score ")+
  xlab("Year")+
  ylab("User score")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 18))

#######          Question 4: The percentage of overrating and underrating from critics score?             #######
#######                                                                                                   #######
#################################################################################################################
 
#Clearing the previous dataset of Question.2 from the NA values of critic_score
vg_df_marks = subset(vg_df_marks, Critic_Score !="NA")
vg_df_marks$User_Score =  as.numeric(lapply(vg_df_marks$User_Score,function(x) x*10))

#Each year, the Nombre of overrated and the Underrated games by the critics
year_diff = ddply(vg_df_marks,.(Year_of_Release),summarize, Name=Name,Year_of_Release=Year_of_Release,
                                                  DifferentPlus=Critic_Score >= User_Score+15, DifferentMinus=Critic_Score <= User_Score-15, 
                                                  Different=(Critic_Score >= User_Score+15) | (Critic_Score <= User_Score-15))

#Each year, the percentage of the overrated/underrated games over all the games + their sum("Total")
year_diff = ddply(year_diff,.(Year_of_Release),summarize, Total=length(Different),
                                                TotalPlus=length(which(DifferentPlus=="TRUE")), TotalMinus=length(which(DifferentMinus=="TRUE")),
                                                PercentagePlus=(TotalPlus/length(Different)*100), PercentageMinus=(TotalMinus/length(Different)*100),
                                                Percentage=PercentagePlus+PercentageMinus)

#Reshaping as before 1997, there are a few critics
year_diff[, 1] <- as.numeric(as.character( year_diff[, 1] ))
year_diff = year_diff[year_diff$Year_of_Release >="1997",]

#Plot of overrating/underraing percentage over time + their sum
ggplot() +                    
  geom_line(year_diff, mapping=aes(x=Year_of_Release, y=Percentage,color='Total',group=1),size=1) +
  geom_line(year_diff, mapping=aes(x=Year_of_Release, y=PercentagePlus, color="Higher by 15+ points",group=1),size=1) +  
  geom_line(year_diff, mapping=aes(x=Year_of_Release, y=PercentageMinus, color="Less by 15- points",group=1),size=1) +
  scale_x_continuous(breaks=1997:2016)+geom_point() + 
  labs(title = "\nPercentage of big differences between User marks and Critic marks, from 1997 to 2016\n", x = "Year", y = "Percentage (in %)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") + 
  theme(text = element_text(size = 18))

#######          Question 5: The percentage of overrating and underrating from critics score?             #######
#######                                                                                                   #######
#################################################################################################################

genreEvolution = ddply(data_less2016,.(Genre,Year_of_Release),summarize,sales=sum(Global_Sales))
ggplot(genreEvolution, aes(Year_of_Release,sales, group=Genre, colour=Genre))+
  geom_line(size=1)+
  facet_grid(. ~ Genre)+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  labs(title ="Evolution of video games sales per genre")+
  xlab("Year")+
  ylab("Sales (million $)")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 18))
  

#######                       Question 6 - Bonus  : The more active publisher                             #######
#######                                                                                                   #######
#################################################################################################################

data_less2016=subset(data_less2016,Publisher != 'Unknown')
MostActifPub=ddply(data_less2016,.(Publisher,Year_of_Release),summarize,nb=length(unique(Name)))
MostActifPub = arrange(MostActifPub,Year_of_Release,desc(nb))
MostActifPub = by(MostActifPub, MostActifPub["Year_of_Release"], head, n=3)
MostActifPub = Reduce(rbind, MostActifPub) #convert to a dataframe
ggplot(MostActifPub,aes(Year_of_Release,nb))+
  geom_bar(stat = "identity",colour="black", position = "dodge",width = 0.7,aes(fill=Publisher))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  scale_x_continuous(labels = as.character(MostActifPub$Year_of_Release), breaks = MostActifPub$Year_of_Release)+
  scale_fill_manual(values = getPalette(colourCount))+
  labs(title ="Publishers that produce the most video games")+
  xlab("Year")+
  ylab("Videogames number")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size = 18))

