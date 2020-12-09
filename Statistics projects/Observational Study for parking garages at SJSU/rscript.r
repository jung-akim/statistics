setwd('Math261B Project')
park=readxl::read_excel('parkstash.xlsx')
str(park)
park$`Class Time`=as.factor(park$`Class Time`)
park$Garage=as.factor(park$Garage)
park$`Class Days`=as.factor(park$`Class Days`)
park$`Arrival Time`=as.factor(park$`Arrival Time`)
str(park)

north=as.data.frame(park[park$Garage=='N',c(2,5)])
p= ggplot(north)+ggtitle('North Garage')+geom_boxplot(aes(x=`Class Time`,y=Ratio))+theme_bw()
p= p+theme(title=element_text(size=30),axis.title=element_text(size=30))
p= p+theme(axis.text=element_text(size=35))
p
west=as.data.frame(park[park$Garage=='W',c(2,5)])
p= ggplot(west)+ggtitle('West Garage')+geom_boxplot(aes(x=`Class Time`,y=Ratio))+theme_bw()
p= p+theme(title=element_text(size=30),axis.title=element_text(size=30))
p= p+theme(axis.text=element_text(size=35))
p
south=as.data.frame(park[park$Garage=='S',c(2,5)])
p= ggplot(south)+ggtitle('South Garage')+geom_boxplot(aes(x=`Class Time`,y=Ratio))+theme_bw()
p= p+theme(title=element_text(size=30),axis.title=element_text(size=30))
p= p+theme(axis.text=element_text(size=35))
p
pr=as.data.frame(park[park$Garage=='P',c(2,5)])
p= ggplot(pr)+ggtitle('Park & Ride')+geom_boxplot(aes(x=`Class Time`,y=Ratio))+theme_bw()
p= p+theme(title=element_text(size=30),axis.title=element_text(size=30))
p= p+theme(axis.text=element_text(size=35))
p
