House <- read.csv("Downloads/House_Price_data (1).csv",header=TRUE,sep=",")
Utilities <- House$Utilities
Heating <- House$Heating
HeatingQC <- House$HeatingQC
GrLivArea <- (House$GrLivArea)
Fullbath <- House$FullBath
Halfbath <- House$HalfBath
Bedroomabvgr <- House$BedroomAbvGr
Kitchenabvgr <- House$KitchenAbvGr
KitchenQual<- House$KitchenQual
GarageType <- House$GarageType
FireplaceQu <- House$FireplaceQu
CentralAir <- House$CentralAir
head(Utilities)
BldgType <- House$BldgType
levels(BldgType)
dummy_heating <- as.numeric(HeatingQC == 'Ex'|HeatingQC == 'Fa'|HeatingQC == 'Gd')
dummy_KitchenQual<- as.numeric(HeatingQC == 'Ex'|KitchenQual == 'Fa'| KitchenQual == 'Gd'|
                                 KitchenQual=='TA')
dummy_fire <- as.numeric(FireplaceQu == 'Ex'|FireplaceQu == 'Fa'|FireplaceQu == 'Gd')
dummy_air<- as.numeric(CentralAir=='Y')
dummy_uti<- as.numeric(Utilities=='AllPub')
dummy_gar<-as.numeric(GarageType=='Attchd'|GarageType=='Detchd')

library(dplyr)
Quality <- House$OverallQual
Year<- House$YearBuilt
Hs <- House$HouseStyle
Hs<- as.factor(Hs)
final_data <- data.frame(Year,GrLivArea,BldgType,House$SalePrice,Hs,Quality)
df1 <- (filter(final_data,dummy_uti==1,dummy_air==1,dummy_fire==1,
               dummy_KitchenQual==1,dummy_heating==1,
               Fullbath>=1,Halfbath>=1,GrLivArea>=1000,
               Bedroomabvgr>=1,Kitchenabvgr>=1))

head(df1)
tail(df1)

df1<-na.omit(df1)
print(is.factor(df1$BldgType))
df1$Hs<-as.factor(df1$Hs)
df1$Hs<- as.numeric(df1$Hs)
df1$BldgType<- as.numeric(df1$BldgType)


n_clusters <- 5
clusters <- kmeans(df1[,(1:2)], n_clusters, nstart = 30)

clusters <- factor(clusters$cluster)
print(table(df1$BldgType, clusters))
library(ggplot2)
library(plotly)
Hs <- factor(Hs)
g <- ggplot(data = df1, 
            aes(x = GrLivArea, 
                y = House.SalePrice,
                colour = clusters))+ 
                 geom_point(data = df1, 
                 aes(size=1))+theme_classic()+ 
  theme(legend.background = element_rect(fill="lightyellow", 
                                         size=0.5, linetype="solid"))+
  ggtitle("Clustering Between Living Area and Sale Price") +
  xlab("Living Area (sqft)") + ylab("Sale Price")

ggplotly(g)



