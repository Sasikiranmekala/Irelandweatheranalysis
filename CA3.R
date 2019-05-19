# importing historical dataset and storing it into iwa
# since we are having 2nd column as measurements skip to import it

iwa<-(read.csv("IWBNetwork_6b6c_27b1_6573.csv", header = TRUE))
str(iwa)
head(iwa)
#collecting the required aspects which are location ids and air temp
data1<-iwa[c(1,13)]
str(data1)
#omitting Not available values
data1<-na.omit(data1)
str(data1)

#we are forming stripchart and making station id as factor for plotting overview on all  stations
stripchart(AirTemperature~as.factor(station_id),data = data1, vertical=T,xlab= "station ID", ylab="Air Temporatures", method="jitter", jitter=0.04)
#Hence we have more than 3 independent groups (stations) here im applying parametric one wat anova test
analysis<-lm(AirTemperature~as.factor(station_id), data=data1)  
anova(analysis)

plot(analysis,which = 1)

# QQ plot
plot(analysis, which = 2)

data1$station_id<-as.factor(data1$station_id)

#for the name record here we are mentioning diffrent station ids as diffrent cities

data1$station_id=factor(data1$station_id,labels = c("city1","city2","city3","city4","city5","city6","city7","city8","city9"))
#ensuring the station id as factor
class(data1$station_id)

#individual group data pulling from main dataset and sets their names as group1 group2 etc ..

group1<-subset(data1, station_id=="city1")
group2<-subset(data1, station_id=="city2")
group3<-subset(data1, station_id=="city3")

# applying qq graph for checking the weather temporature distribution for diffrent groups
qqnorm(group1$AirTemperature)
qqline(group1$AirTemperature)

qqnorm(group2$AirTemperature)
qqline(group2$AirTemperature)

qqnorm(group3$AirTemperature)
qqline(group3$AirTemperature)

# as it has more than 3 groups, kruskal wallis test applies  

kruskal.test(data1$AirTemperature ~data1$station_id)

# The p values is far less than the standard error value 0.05 here we are rejeting the null hypothesis.