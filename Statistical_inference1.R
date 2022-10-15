data= read.csv("banknotes.csv")
fake= data[data$conterfeit==0,]
real= data[data$conterfeit==1,]

