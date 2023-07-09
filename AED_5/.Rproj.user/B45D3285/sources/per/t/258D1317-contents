library(dplyr)
irisN<-iris%>%mutate(Petal.LengthN=(Petal.Length-mean(Petal.Length))/sd(Petal.Length))
irisN
irisN<-iris%>%transmute(Petal.LengthN=(Petal.Length-mean(Petal.Length))/sd(Petal.Length))
irisN

gapminder::gapminder%>%group_by(country)%>%summarize(mean_lifeExp=mean(lifeExp), SD_LIFEeXP=sd(lifeExp))
gapminder%>%group_by(continent, country)%>%summarise(Evm=mean(lifeExp))

gapminder%>%group_by(continent, country)%>%summarise(Evm=mean(lifeExp))%>%arrange(Evm)

gapminder%>%filter(continent=="Asia")%>%select("lifeExp")

funciones1<-list(max, min, median)
funciones2<-list(maxi=max, mini=min, med=median)

iris%>%
  group_by(Species)%>%
  summarise(across(.cols=starts_with("S"), .fns=funciones2, na.rm=TRUE))

