data_2 <- read.csv("world-happiness-report-edited (1).csv")

country_factor<-matrix(nrow = 40, ncol = 6)#matrix to store 40 countries and 6 factors

#calculate correlation of the 6 factors for each country over 10 years
for (j in 1:40){
  for (i in 4:9){
    #x should be variable to save 40 countries ladder value 
    x<-data_2[(1+10*(j-1)):(10*j),3]
    #y should be variable to save feature's data
    y<-data_2[(1+10*(j-1)):(10*j),i]
    #calculate correlation for one country's one factor
    #the absolute value of correlations are used for further comparison
    country_factor[j,(i-3)]<-abs(cor(x,y))*data_2[10*j,10]
  }
}

#add up the results of countries in the same region
region_factor<-matrix(nrow = 10, ncol = 6)#matrix to store 10 regions and 6 factors
for (m in 1:10){
  for (n in 1:6) {
    region_factor[m,n]<-sum(country_factor[(1+4*(m-1)):(4*m),n],na.rm=TRUE)#na.rm is used to skip missing data
  }
}

#get factors of the maximum value for each region
factor<-c("Log GDP per capita","Social support","Healthy life expectancy at birth","Freedom to make life choices","Generosity","Perceptions of corruption")
region<-(1:10)#store the name of 10 regions
final<-(1:10)#store the name of the biggest factors
for (i in 1:10){
  #here we find the most important factors per region
  final[i]=factor[which(region_factor[i,]==max(region_factor[i,]))]
  region[i]=data_2[40*i,11]
}

#print the results
result<-data.frame("Region"=region,"Biggest Factor"=final)
print(result)
