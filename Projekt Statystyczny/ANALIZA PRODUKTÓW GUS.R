library("ggplot2")
library("tidyr")
library("dplyr")
library("readxl")

ceny<-readxl::read_xlsx ("C:/DANE/CENY_2917_XPIV_20200202120024.xlsx",sheet=2)
ceny<-ceny[,-9]
ceny$Wartosc <- as.numeric(ceny$Wartosc)
ceny <- na.omit(ceny) 
ceny$Rok <- as.numeric(ceny$Rok)
reg <- lm(ceny$Wartosc~ceny$Rok) 
print(reg)           

rok2006 <- filter(ceny, Rok == 2006)
rok2007 <- filter(ceny, Rok == 2007)
rok2008 <- filter(ceny, Rok == 2008)
rok2009 <- filter(ceny, Rok == 2009)
rok2010 <- filter(ceny, Rok == 2010)
rok2011 <- filter(ceny, Rok == 2011)
rok2012 <- filter(ceny, Rok == 2012)
rok2013 <- filter(ceny, Rok == 2013)
rok2014 <- filter(ceny, Rok == 2014)
rok2015 <- filter(ceny, Rok == 2015)
rok2016 <- filter(ceny, Rok == 2016)
rok2017 <- filter(ceny, Rok == 2017)
rok2018 <- filter(ceny, Rok == 2018)
rok2019 <- filter(ceny, Rok == 2019)

wartosci2006 <- mean(rok2006$Wartosc)
wartosci2007 <- mean(rok2007$Wartosc)
wartosci2008 <- mean(rok2008$Wartosc)
wartosci2009 <- mean(rok2009$Wartosc)
wartosci2010 <- mean(rok2010$Wartosc)
wartosci2011 <- mean(rok2011$Wartosc)
wartosci2012 <- mean(rok2012$Wartosc)
wartosci2013 <- mean(rok2013$Wartosc)
wartosci2014 <- mean(rok2014$Wartosc)
wartosci2015 <- mean(rok2015$Wartosc)
wartosci2016 <- mean(rok2016$Wartosc)
wartosci2017 <- mean(rok2017$Wartosc)
wartosci2018 <- mean(rok2018$Wartosc)
wartosci2019 <- mean(rok2019$Wartosc)

sredniawartosc<-c(wartosci2006, wartosci2007,wartosci2008,wartosci2009,wartosci2010,wartosci2011,wartosci2012,wartosci2013,wartosci2014,wartosci2015,wartosci2016,wartosci2017,wartosci2018,wartosci2019)
ex<-c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
plot(ex,sredniawartosc)
zmienna <- c(rok2006$Wartosc,rok2007$Wartosc,rok2008$Wartosc,rok2009$Wartosc,rok2010$Wartosc,rok2011$Wartosc,rok2012$Wartosc,rok2013$Wartosc,rok14$Wartosc,rok2015$Wartosc,rok2016$Wartosc,rok2017$Wartosc,rok2018$Wartosc,rok2019$Wartosc)
plot(zmienna)
sredniacena<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), sredniawartosc= c(wartosci2006, wartosci2007,wartosci2008,wartosci2009,wartosci2010,wartosci2011,wartosci2012,wartosci2013,wartosci2014,wartosci2015,wartosci2016,wartosci2017,wartosci2018,wartosci2019))
plot(sredniacena)

#ZACHODNIOPOMORSKIE WOJEWÓDZTWO
zachodnie<-ceny %>%
  filter(Nazwa == "ZACHODNIOPOMORSKIE") %>%
  select(Rok, Wartosc)

zachodniorok2006 <- filter(zachodnie, Rok == 2006)
zachodniorok2007 <- filter(zachodnie, Rok == 2007)
zachodniorok2008 <- filter(zachodnie, Rok == 2008)
zachodniorok2009 <- filter(zachodnie, Rok == 2009)
zachodniorok2010 <- filter(zachodnie, Rok == 2010)
zachodniorok2011 <- filter(zachodnie, Rok == 2011)
zachodniorok2012 <- filter(zachodnie, Rok == 2012)
zachodniorok2013 <- filter(zachodnie, Rok == 2013)
zachodniorok2014 <- filter(zachodnie, Rok == 2014)
zachodniorok2015 <- filter(zachodnie, Rok == 2015)
zachodniorok2016 <- filter(zachodnie, Rok == 2016)
zachodniorok2017 <- filter(zachodnie, Rok == 2017)
zachodniorok2018 <- filter(zachodnie, Rok == 2018)
zachodniorok2019 <- filter(zachodnie, Rok == 2019)

zachodniowartosc2006 <- mean(zachodniorok2006$Wartosc)
zachodniowartosc2007 <- mean(zachodniorok2007$Wartosc)
zachodniowartosc2008 <- mean(zachodniorok2008$Wartosc)
zachodniowartosc2009 <- mean(zachodniorok2009$Wartosc)
zachodniowartosc2010 <- mean(zachodniorok2010$Wartosc)
zachodniowartosc2011 <- mean(zachodniorok2011$Wartosc)
zachodniowartosc2012 <- mean(zachodniorok2012$Wartosc)
zachodniowartosc2013 <- mean(zachodniorok2013$Wartosc)
zachodniowartosc2014 <- mean(zachodniorok2014$Wartosc)
zachodniowartosc2015 <- mean(zachodniorok2015$Wartosc)
zachodniowartosc2016 <- mean(zachodniorok2016$Wartosc)
zachodniowartosc2017 <- mean(zachodniorok2017$Wartosc)
zachodniowartosc2018 <- mean(zachodniorok2018$Wartosc)
zachodniowartosc2019 <- mean(zachodniorok2019$Wartosc)

ZACHODNIOPOMORSKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(zachodniowartosc2006,zachodniowartosc2007,zachodniowartosc2008,zachodniowartosc2009,zachodniowartosc2010,zachodniowartosc2011,zachodniowartosc2012,zachodniowartosc2013,zachodniowartosc2014,zachodniowartosc2015,zachodniowartosc2016,zachodniowartosc2017,zachodniowartosc2018,zachodniowartosc2019))


plot(ZACHODNIOPOMORSKIE, aes(x=ZACHODNIOPOMORSKIE$Rok, y=ZACHODNIOPOMORSKIE$srednia))+
  geom_line()


#DOLNOŒL¥SKIE WOJEWÓDZTWO
dolno<-ceny %>%
  filter(Nazwa == "DOLNOŒL¥SKIE") %>%
  select(Rok, Wartosc)

dolnorok2006 <- filter(dolno, Rok == 2006)
dolnorok2007 <- filter(dolno, Rok == 2007)
dolnorok2008 <- filter(dolno, Rok == 2008)
dolnorok2009 <- filter(dolno, Rok == 2009)
dolnorok2010 <- filter(dolno, Rok == 2010)
dolnorok2011 <- filter(dolno, Rok == 2011)
dolnorok2012 <- filter(dolno, Rok == 2012)
dolnorok2013 <- filter(dolno, Rok == 2013)
dolnorok2014 <- filter(dolno, Rok == 2014)
dolnorok2015 <- filter(dolno, Rok == 2015)
dolnorok2016 <- filter(dolno, Rok == 2016)
dolnorok2017 <- filter(dolno, Rok == 2017)
dolnorok2018 <- filter(dolno, Rok == 2018)
dolnorok2019 <- filter(dolno, Rok == 2019)

dolnowartosc2006 <- mean(dolnorok2006$Wartosc)
dolnowartosc2007 <- mean(dolnorok2007$Wartosc)
dolnowartosc2008 <- mean(dolnorok2008$Wartosc)
dolnowartosc2009 <- mean(dolnorok2009$Wartosc)
dolnowartosc2010 <- mean(dolnorok2010$Wartosc)
dolnowartosc2011 <- mean(dolnorok2011$Wartosc)
dolnowartosc2012 <- mean(dolnorok2012$Wartosc)
dolnowartosc2013 <- mean(dolnorok2013$Wartosc)
dolnowartosc2014 <- mean(dolnorok2014$Wartosc)
dolnowartosc2015 <- mean(dolnorok2015$Wartosc)
dolnowartosc2016 <- mean(dolnorok2016$Wartosc)
dolnowartosc2017 <- mean(dolnorok2017$Wartosc)
dolnowartosc2018 <- mean(dolnorok2018$Wartosc)
dolnowartosc2019 <- mean(dolnorok2019$Wartosc)

DOLNOŒL¥SKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(dolnowartosc2006,dolnowartosc2007,dolnowartosc2008,dolnowartosc2009,dolnowartosc2010,dolnowartosc2011,dolnowartosc2012,dolnowartosc2013,dolnowartosc2014,dolnowartosc2015,dolnowartosc2016,dolnowartosc2017,dolnowartosc2018,dolnowartosc2019))

plot(DOLNOŒL¥SKIE, aes(x=DOLNOŒL¥SKIE$Rok, y=DOLNOŒL¥SKIE$srednia))+
  geom_line()


#LUBUSKIE WOJEWÓDZTWO
lubu<-ceny %>%
  filter(Nazwa == "LUBUSKIE") %>%
  select(Rok, Wartosc)

luburok2006 <- filter(lubu, Rok == 2006)
luburok2007 <- filter(lubu, Rok == 2007)
luburok2008 <- filter(lubu, Rok == 2008)
luburok2009 <- filter(lubu, Rok == 2009)
luburok2010 <- filter(lubu, Rok == 2010)
luburok2011 <- filter(lubu, Rok == 2011)
luburok2012 <- filter(lubu, Rok == 2012)
luburok2013 <- filter(lubu, Rok == 2013)
luburok2014 <- filter(lubu, Rok == 2014)
luburok2015 <- filter(lubu, Rok == 2015)
luburok2016 <- filter(lubu, Rok == 2016)
luburok2017 <- filter(lubu, Rok == 2017)
luburok2018 <- filter(lubu, Rok == 2018)
luburok2019 <- filter(lubu, Rok == 2019)

lubuwartosc2006 <- mean(luburok2006$Wartosc)
lubuwartosc2007 <- mean(luburok2007$Wartosc)
lubuwartosc2008 <- mean(luburok2008$Wartosc)
lubuwartosc2009 <- mean(luburok2009$Wartosc)
lubuwartosc2010 <- mean(luburok2010$Wartosc)
lubuwartosc2011 <- mean(luburok2011$Wartosc)
lubuwartosc2012 <- mean(luburok2012$Wartosc)
lubuwartosc2013 <- mean(luburok2013$Wartosc)
lubuwartosc2014 <- mean(luburok2014$Wartosc)
lubuwartosc2015 <- mean(luburok2015$Wartosc)
lubuwartosc2016 <- mean(luburok2016$Wartosc)
lubuwartosc2017 <- mean(luburok2017$Wartosc)
lubuwartosc2018 <- mean(luburok2018$Wartosc)
lubuwartosc2019 <- mean(luburok2019$Wartosc)

LUBUSKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(lubuwartosc2006,lubuwartosc2007,lubuwartosc2008,lubuwartosc2009,lubuwartosc2010,lubuwartosc2011,lubuwartosc2012,lubuwartosc2013,lubuwartosc2014,lubuwartosc2015,lubuwartosc2016,lubuwartosc2017,lubuwartosc2018,lubuwartosc2019))

plot(LUBUSKIE, aes(x=LUBUSKIE$Rok, y=LUBUSKIE$srednia))+
  geom_line()


#LUBELSKIE  WOJEWÓDZTWO
lubel<-ceny %>%
  filter(Nazwa == "LUBELSKIE") %>%
  select(Rok, Wartosc)

lubelrok2006 <- filter(lubel, Rok == 2006)
lubelrok2007 <- filter(lubel, Rok == 2007)
lubelrok2008 <- filter(lubel, Rok == 2008)
lubelrok2009 <- filter(lubel, Rok == 2009)
lubelrok2010 <- filter(lubel, Rok == 2010)
lubelrok2011 <- filter(lubel, Rok == 2011)
lubelrok2012 <- filter(lubel, Rok == 2012)
lubelrok2013 <- filter(lubel, Rok == 2013)
lubelrok2014 <- filter(lubel, Rok == 2014)
lubelrok2015 <- filter(lubel, Rok == 2015)
lubelrok2016 <- filter(lubel, Rok == 2016)
lubelrok2017 <- filter(lubel, Rok == 2017)
lubelrok2018 <- filter(lubel, Rok == 2018)
lubelrok2019 <- filter(lubel, Rok == 2019)

lubelwartosc2006 <- mean(lubelrok2006$Wartosc)
lubelwartosc2007 <- mean(lubelrok2007$Wartosc)
lubelwartosc2008 <- mean(lubelrok2008$Wartosc)
lubelwartosc2009 <- mean(lubelrok2009$Wartosc)
lubelwartosc2010 <- mean(lubelrok2010$Wartosc)
lubelwartosc2011 <- mean(lubelrok2011$Wartosc)
lubelwartosc2012 <- mean(lubelrok2012$Wartosc)
lubelwartosc2013 <- mean(lubelrok2013$Wartosc)
lubelwartosc2014 <- mean(lubelrok2014$Wartosc)
lubelwartosc2015 <- mean(lubelrok2015$Wartosc)
lubelwartosc2016 <- mean(lubelrok2016$Wartosc)
lubelwartosc2017 <- mean(lubelrok2017$Wartosc)
lubelwartosc2018 <- mean(lubelrok2018$Wartosc)
lubelwartosc2019 <- mean(lubelrok2019$Wartosc)

LUBELSKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(lubelwartosc2006,lubelwartosc2007,lubelwartosc2008,lubelwartosc2009,lubelwartosc2010,lubelwartosc2011,lubelwartosc2012,lubelwartosc2013,lubelwartosc2014,lubelwartosc2015,lubelwartosc2016,lubelwartosc2017,lubelwartosc2018,lubelwartosc2019))

plot(LUBELSKIE, aes(x=LUBELSKIE$Rok, y=LUBELSKIE$srednia))+
  geom_line()


#MA£OPOLSKIE WOJEWÓDZTWO
malo<-ceny %>%
  filter(Nazwa == "MA£OPOLSKIE") %>%
  select(Rok, Wartosc)

malorok2006 <- filter(malo, Rok == 2006)
malorok2007 <- filter(malo, Rok == 2007)
malorok2008 <- filter(malo, Rok == 2008)
malorok2009 <- filter(malo, Rok == 2009)
malorok2010 <- filter(malo, Rok == 2010)
malorok2011 <- filter(malo, Rok == 2011)
malorok2012 <- filter(malo, Rok == 2012)
malorok2013 <- filter(malo, Rok == 2013)
malorok2014 <- filter(malo, Rok == 2014)
malorok2015 <- filter(malo, Rok == 2015)
malorok2016 <- filter(malo, Rok == 2016)
malorok2017 <- filter(malo, Rok == 2017)
malorok2018 <- filter(malo, Rok == 2018)
malorok2019 <- filter(malo, Rok == 2019)

malowartosc2006 <- mean(malorok2006$Wartosc)
malowartosc2007 <- mean(malorok2007$Wartosc)
malowartosc2008 <- mean(malorok2008$Wartosc)
malowartosc2009 <- mean(malorok2009$Wartosc)
malowartosc2010 <- mean(malorok2010$Wartosc)
malowartosc2011 <- mean(malorok2011$Wartosc)
malowartosc2012 <- mean(malorok2012$Wartosc)
malowartosc2013 <- mean(malorok2013$Wartosc)
malowartosc2014 <- mean(malorok2014$Wartosc)
malowartosc2015 <- mean(malorok2015$Wartosc)
malowartosc2016 <- mean(malorok2016$Wartosc)
malowartosc2017 <- mean(malorok2017$Wartosc)
malowartosc2018 <- mean(malorok2018$Wartosc)
malowartosc2019 <- mean(malorok2019$Wartosc)

MA£OPOLSKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(malowartosc2006,malowartosc2007,malowartosc2008,malowartosc2009,malowartosc2010,malowartosc2011,malowartosc2012,malowartosc2013,malowartosc2014,malowartosc2015,malowartosc2016,malowartosc2017,malowartosc2018,malowartosc2019))

plot(MA£OPOLSKIE, aes(x=MA£OPOLSKIE$Rok, y=MA£OPOLSKIE$srednia))+
  geom_line()


#OPOLSKIE WOJEWÓDZTWO
opol<-ceny %>%
  filter(Nazwa == "OPOLSKIE") %>%
  select(Rok, Wartosc)

opolrok2006 <- filter(opol, Rok == 2006)
opolrok2007 <- filter(opol, Rok == 2007)
opolrok2008 <- filter(opol, Rok == 2008)
opolrok2009 <- filter(opol, Rok == 2009)
opolrok2010 <- filter(opol, Rok == 2010)
opolrok2011 <- filter(opol, Rok == 2011)
opolrok2012 <- filter(opol, Rok == 2012)
opolrok2013 <- filter(opol, Rok == 2013)
opolrok2014 <- filter(opol, Rok == 2014)
opolrok2015 <- filter(opol, Rok == 2015)
opolrok2016 <- filter(opol, Rok == 2016)
opolrok2017 <- filter(opol, Rok == 2017)
opolrok2018 <- filter(opol, Rok == 2018)
opolrok2019 <- filter(opol, Rok == 2019)

opolwartosc2006 <- mean(opolrok2006$Wartosc)
opolwartosc2007 <- mean(opolrok2007$Wartosc)
opolwartosc2008 <- mean(opolrok2008$Wartosc)
opolwartosc2009 <- mean(opolrok2009$Wartosc)
opolwartosc2010 <- mean(opolrok2010$Wartosc)
opolwartosc2011 <- mean(opolrok2011$Wartosc)
opolwartosc2012 <- mean(opolrok2012$Wartosc)
opolwartosc2013 <- mean(opolrok2013$Wartosc)
opolwartosc2014 <- mean(opolrok2014$Wartosc)
opolwartosc2015 <- mean(opolrok2015$Wartosc)
opolwartosc2016 <- mean(opolrok2016$Wartosc)
opolwartosc2017 <- mean(opolrok2017$Wartosc)
opolwartosc2018 <- mean(opolrok2018$Wartosc)
opolwartosc2019 <- mean(opolrok2019$Wartosc)

OPOLSKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(opolwartosc2006,opolwartosc2007,opolwartosc2008,opolwartosc2009,opolwartosc2010,opolwartosc2011,opolwartosc2012,opolwartosc2013,opolwartosc2014,opolwartosc2015,opolwartosc2016,opolwartosc2017,opolwartosc2018,opolwartosc2019))

plot(OPOLSKIE, aes(x=OPOLSKIE$Rok, y=OPOLSKIE$srednia))+
  geom_line()


#POMORSKIE WOJEWÓDZTWO
pomo<-ceny %>%
  filter(Nazwa == "POMORSKIE") %>%
  select(Rok, Wartosc)

pomorok2006 <- filter(pomo, Rok == 2006)
pomorok2007 <- filter(pomo, Rok == 2007)
pomorok2008 <- filter(pomo, Rok == 2008)
pomorok2009 <- filter(pomo, Rok == 2009)
pomorok2010 <- filter(pomo, Rok == 2010)
pomorok2011 <- filter(pomo, Rok == 2011)
pomorok2012 <- filter(pomo, Rok == 2012)
pomorok2013 <- filter(pomo, Rok == 2013)
pomorok2014 <- filter(pomo, Rok == 2014)
pomorok2015 <- filter(pomo, Rok == 2015)
pomorok2016 <- filter(pomo, Rok == 2016)
pomorok2017 <- filter(pomo, Rok == 2017)
pomorok2018 <- filter(pomo, Rok == 2018)
pomorok2019 <- filter(pomo, Rok == 2019)

pomowartosc2006 <- mean(pomorok2006$Wartosc)
pomowartosc2007 <- mean(pomorok2007$Wartosc)
pomowartosc2008 <- mean(pomorok2008$Wartosc)
pomowartosc2009 <- mean(pomorok2009$Wartosc)
pomowartosc2010 <- mean(pomorok2010$Wartosc)
pomowartosc2011 <- mean(pomorok2011$Wartosc)
pomowartosc2012 <- mean(pomorok2012$Wartosc)
pomowartosc2013 <- mean(pomorok2013$Wartosc)
pomowartosc2014 <- mean(pomorok2014$Wartosc)
pomowartosc2015 <- mean(pomorok2015$Wartosc)
pomowartosc2016 <- mean(pomorok2016$Wartosc)
pomowartosc2017 <- mean(pomorok2017$Wartosc)
pomowartosc2018 <- mean(pomorok2018$Wartosc)
pomowartosc2019 <- mean(pomorok2019$Wartosc)

POMORSKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(pomowartosc2006,pomowartosc2007,pomowartosc2008,pomowartosc2009,pomowartosc2010,pomowartosc2011,pomowartosc2012,pomowartosc2013,pomowartosc2014,pomowartosc2015,pomowartosc2016,pomowartosc2017,pomowartosc2018,pomowartosc2019))

plot(POMORSKIE, aes(x=POMORSKIE$Rok, y=POMORSKIE$srednia))+
  geom_line()


#MAZOWIECKIE WOJEWÓDZTWO
mazo<-ceny %>%
  filter(Nazwa == "MAZOWIECKIE") %>%
  select(Rok, Wartosc)

mazorok2006 <- filter(mazo, Rok == 2006)
mazorok2007 <- filter(mazo, Rok == 2007)
mazorok2008 <- filter(mazo, Rok == 2008)
mazorok2009 <- filter(mazo, Rok == 2009)
mazorok2010 <- filter(mazo, Rok == 2010)
mazorok2011 <- filter(mazo, Rok == 2011)
mazorok2012 <- filter(mazo, Rok == 2012)
mazorok2013 <- filter(mazo, Rok == 2013)
mazorok2014 <- filter(mazo, Rok == 2014)
mazorok2015 <- filter(mazo, Rok == 2015)
mazorok2016 <- filter(mazo, Rok == 2016)
mazorok2017 <- filter(mazo, Rok == 2017)
mazorok2018 <- filter(mazo, Rok == 2018)
mazorok2019 <- filter(mazo, Rok == 2019)

mazowartosc2006 <- mean(mazorok2006$Wartosc)
mazowartosc2007 <- mean(mazorok2007$Wartosc)
mazowartosc2008 <- mean(mazorok2008$Wartosc)
mazowartosc2009 <- mean(mazorok2009$Wartosc)
mazowartosc2010 <- mean(mazorok2010$Wartosc)
mazowartosc2011 <- mean(mazorok2011$Wartosc)
mazowartosc2012 <- mean(mazorok2012$Wartosc)
mazowartosc2013 <- mean(mazorok2013$Wartosc)
mazowartosc2014 <- mean(mazorok2014$Wartosc)
mazowartosc2015 <- mean(mazorok2015$Wartosc)
mazowartosc2016 <- mean(mazorok2016$Wartosc)
mazowartosc2017 <- mean(mazorok2017$Wartosc)
mazowartosc2018 <- mean(mazorok2018$Wartosc)
mazowartosc2019 <- mean(mazorok2019$Wartosc)

MAZOWIECKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(mazowartosc2006,mazowartosc2007,mazowartosc2008,mazowartosc2009,mazowartosc2010,mazowartosc2011,mazowartosc2012,mazowartosc2013,mazowartosc2014,mazowartosc2015,mazowartosc2016,mazowartosc2017,mazowartosc2018,mazowartosc2019))

plot(MAZOWIECKIE, aes(x=MAZOWIECKIE$Rok, y=MAZOWIECKIE$srednia))+
  geom_line()


#ŒL¥SKIE WOJEWÓDZTWO
slas<-ceny %>%
  filter(Nazwa == "ŒL¥SKIE") %>%
  select(Rok, Wartosc)

slasrok2006 <- filter(slas, Rok == 2006)
slasrok2007 <- filter(slas, Rok == 2007)
slasrok2008 <- filter(slas, Rok == 2008)
slasrok2009 <- filter(slas, Rok == 2009)
slasrok2010 <- filter(slas, Rok == 2010)
slasrok2011 <- filter(slas, Rok == 2011)
slasrok2012 <- filter(slas, Rok == 2012)
slasrok2013 <- filter(slas, Rok == 2013)
slasrok2014 <- filter(slas, Rok == 2014)
slasrok2015 <- filter(slas, Rok == 2015)
slasrok2016 <- filter(slas, Rok == 2016)
slasrok2017 <- filter(slas, Rok == 2017)
slasrok2018 <- filter(slas, Rok == 2018)
slasrok2019 <- filter(slas, Rok == 2019)

slaswartosc2006 <- mean(slasrok2006$Wartosc)
slaswartosc2007 <- mean(slasrok2007$Wartosc)
slaswartosc2008 <- mean(slasrok2008$Wartosc)
slaswartosc2009 <- mean(slasrok2009$Wartosc)
slaswartosc2010 <- mean(slasrok2010$Wartosc)
slaswartosc2011 <- mean(slasrok2011$Wartosc)
slaswartosc2012 <- mean(slasrok2012$Wartosc)
slaswartosc2013 <- mean(slasrok2013$Wartosc)
slaswartosc2014 <- mean(slasrok2014$Wartosc)
slaswartosc2015 <- mean(slasrok2015$Wartosc)
slaswartosc2016 <- mean(slasrok2016$Wartosc)
slaswartosc2017 <- mean(slasrok2017$Wartosc)
slaswartosc2018 <- mean(slasrok2018$Wartosc)
slaswartosc2019 <- mean(slasrok2019$Wartosc)

ŒL¥SKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(slaswartosc2006,slaswartosc2007,slaswartosc2008,slaswartosc2009,slaswartosc2010,slaswartosc2011,slaswartosc2012,slaswartosc2013,slaswartosc2014,slaswartosc2015,slaswartosc2016,slaswartosc2017,slaswartosc2018,slaswartosc2019))

plot(ŒL¥SKIE, aes(x=ŒL¥SKIE$Rok, y=ŒL¥SKIE$srednia))+
  geom_line()


#PODKARPACKIE WOJEWÓDZTWO
podk<-ceny %>%
  filter(Nazwa == "PODKARPACKIE") %>%
  select(Rok, Wartosc)

podkrok2006 <- filter(podk, Rok == 2006)
podkrok2007 <- filter(podk, Rok == 2007)
podkrok2008 <- filter(podk, Rok == 2008)
podkrok2009 <- filter(podk, Rok == 2009)
podkrok2010 <- filter(podk, Rok == 2010)
podkrok2011 <- filter(podk, Rok == 2011)
podkrok2012 <- filter(podk, Rok == 2012)
podkrok2013 <- filter(podk, Rok == 2013)
podkrok2014 <- filter(podk, Rok == 2014)
podkrok2015 <- filter(podk, Rok == 2015)
podkrok2016 <- filter(podk, Rok == 2016)
podkrok2017 <- filter(podk, Rok == 2017)
podkrok2018 <- filter(podk, Rok == 2018)
podkrok2019 <- filter(podk, Rok == 2019)

podkwartosc2006 <- mean(podkrok2006$Wartosc)
podkwartosc2007 <- mean(podkrok2007$Wartosc)
podkwartosc2008 <- mean(podkrok2008$Wartosc)
podkwartosc2009 <- mean(podkrok2009$Wartosc)
podkwartosc2010 <- mean(podkrok2010$Wartosc)
podkwartosc2011 <- mean(podkrok2011$Wartosc)
podkwartosc2012 <- mean(podkrok2012$Wartosc)
podkwartosc2013 <- mean(podkrok2013$Wartosc)
podkwartosc2014 <- mean(podkrok2014$Wartosc)
podkwartosc2015 <- mean(podkrok2015$Wartosc)
podkwartosc2016 <- mean(podkrok2016$Wartosc)
podkwartosc2017 <- mean(podkrok2017$Wartosc)
podkwartosc2018 <- mean(podkrok2018$Wartosc)
podkwartosc2019 <- mean(podkrok2019$Wartosc)

PODKARPACKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(podkwartosc2006,podkwartosc2007,podkwartosc2008,podkwartosc2009,podkwartosc2010,podkwartosc2011,podkwartosc2012,podkwartosc2013,podkwartosc2014,podkwartosc2015,podkwartosc2016,podkwartosc2017,podkwartosc2018,podkwartosc2019))

plot(PODKARPACKIE, aes(x=PODKARPACKIE$Rok, y=PODKARPACKIE$srednia))+
  geom_line()


#£ÓDZKIE WOJEWÓDZTWO
lodz<-ceny %>%
  filter(Nazwa == "£ÓDZKIE") %>%
  select(Rok, Wartosc)

lodzrok2006 <- filter(lodz, Rok == 2006)
lodzrok2007 <- filter(lodz, Rok == 2007)
lodzrok2008 <- filter(lodz, Rok == 2008)
lodzrok2009 <- filter(lodz, Rok == 2009)
lodzrok2010 <- filter(lodz, Rok == 2010)
lodzrok2011 <- filter(lodz, Rok == 2011)
lodzrok2012 <- filter(lodz, Rok == 2012)
lodzrok2013 <- filter(lodz, Rok == 2013)
lodzrok2014 <- filter(lodz, Rok == 2014)
lodzrok2015 <- filter(lodz, Rok == 2015)
lodzrok2016 <- filter(lodz, Rok == 2016)
lodzrok2017 <- filter(lodz, Rok == 2017)
lodzrok2018 <- filter(lodz, Rok == 2018)
lodzrok2019 <- filter(lodz, Rok == 2019)

lodzwartosc2006 <- mean(lodzrok2006$Wartosc)
lodzwartosc2007 <- mean(lodzrok2007$Wartosc)
lodzwartosc2008 <- mean(lodzrok2008$Wartosc)
lodzwartosc2009 <- mean(lodzrok2009$Wartosc)
lodzwartosc2010 <- mean(lodzrok2010$Wartosc)
lodzwartosc2011 <- mean(lodzrok2011$Wartosc)
lodzwartosc2012 <- mean(lodzrok2012$Wartosc)
lodzwartosc2013 <- mean(lodzrok2013$Wartosc)
lodzwartosc2014 <- mean(lodzrok2014$Wartosc)
lodzwartosc2015 <- mean(lodzrok2015$Wartosc)
lodzwartosc2016 <- mean(lodzrok2016$Wartosc)
lodzwartosc2017 <- mean(lodzrok2017$Wartosc)
lodzwartosc2018 <- mean(lodzrok2018$Wartosc)
lodzwartosc2019 <- mean(lodzrok2019$Wartosc)

£ÓDZKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(lodzwartosc2006,lodzwartosc2007,lodzwartosc2008,lodzwartosc2009,lodzwartosc2010,lodzwartosc2011,lodzwartosc2012,lodzwartosc2013,lodzwartosc2014,lodzwartosc2015,lodzwartosc2016,lodzwartosc2017,lodzwartosc2018,lodzwartosc2019))

plot(£ÓDZKIE, aes(x=£ÓDZKIE$Rok, y=£ÓDZKIE$srednia))+
  geom_line()


#KUJAWSKO-POMORSKIE WOJEWÓDZTWO
kuja<-ceny %>%
  filter(Nazwa == "KUJAWSKO-POMORSKIE") %>%
  select(Rok, Wartosc)

kujarok2006 <- filter(kuja, Rok == 2006)
kujarok2007 <- filter(kuja, Rok == 2007)
kujarok2008 <- filter(kuja, Rok == 2008)
kujarok2009 <- filter(kuja, Rok == 2009)
kujarok2010 <- filter(kuja, Rok == 2010)
kujarok2011 <- filter(kuja, Rok == 2011)
kujarok2012 <- filter(kuja, Rok == 2012)
kujarok2013 <- filter(kuja, Rok == 2013)
kujarok2014 <- filter(kuja, Rok == 2014)
kujarok2015 <- filter(kuja, Rok == 2015)
kujarok2016 <- filter(kuja, Rok == 2016)
kujarok2017 <- filter(kuja, Rok == 2017)
kujarok2018 <- filter(kuja, Rok == 2018)
kujarok2019 <- filter(kuja, Rok == 2019)

kujawartosc2006 <- mean(kujarok2006$Wartosc)
kujawartosc2007 <- mean(kujarok2007$Wartosc)
kujawartosc2008 <- mean(kujarok2008$Wartosc)
kujawartosc2009 <- mean(kujarok2009$Wartosc)
kujawartosc2010 <- mean(kujarok2010$Wartosc)
kujawartosc2011 <- mean(kujarok2011$Wartosc)
kujawartosc2012 <- mean(kujarok2012$Wartosc)
kujawartosc2013 <- mean(kujarok2013$Wartosc)
kujawartosc2014 <- mean(kujarok2014$Wartosc)
kujawartosc2015 <- mean(kujarok2015$Wartosc)
kujawartosc2016 <- mean(kujarok2016$Wartosc)
kujawartosc2017 <- mean(kujarok2017$Wartosc)
kujawartosc2018 <- mean(kujarok2018$Wartosc)
kujawartosc2019 <- mean(kujarok2019$Wartosc)

KUJAWSKOPOMORSKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(kujawartosc2006,kujawartosc2007,kujawartosc2008,kujawartosc2009,kujawartosc2010,kujawartosc2011,kujawartosc2012,kujawartosc2013,kujawartosc2014,kujawartosc2015,kujawartosc2016,kujawartosc2017,kujawartosc2018,kujawartosc2019))

plot(KUJAWSKOPOMORSKIE, aes(x=KUJAWSKOPOMORSKIE$Rok, y=KUJAWSKOPOMORSKIE$srednia))+
  geom_line()


#ŒWIÊTOKRZYSKIE WOJEWÓDZTWO
swie<-ceny %>%
  filter(Nazwa == "ŒWIÊTOKRZYSKIE") %>%
  select(Rok, Wartosc)

swierok2006 <- filter(swie, Rok == 2006)
swierok2007 <- filter(swie, Rok == 2007)
swierok2008 <- filter(swie, Rok == 2008)
swierok2009 <- filter(swie, Rok == 2009)
swierok2010 <- filter(swie, Rok == 2010)
swierok2011 <- filter(swie, Rok == 2011)
swierok2012 <- filter(swie, Rok == 2012)
swierok2013 <- filter(swie, Rok == 2013)
swierok2014 <- filter(swie, Rok == 2014)
swierok2015 <- filter(swie, Rok == 2015)
swierok2016 <- filter(swie, Rok == 2016)
swierok2017 <- filter(swie, Rok == 2017)
swierok2018 <- filter(swie, Rok == 2018)
swierok2019 <- filter(swie, Rok == 2019)

swiewartosc2006 <- mean(swierok2006$Wartosc)
swiewartosc2007 <- mean(swierok2007$Wartosc)
swiewartosc2008 <- mean(swierok2008$Wartosc)
swiewartosc2009 <- mean(swierok2009$Wartosc)
swiewartosc2010 <- mean(swierok2010$Wartosc)
swiewartosc2011 <- mean(swierok2011$Wartosc)
swiewartosc2012 <- mean(swierok2012$Wartosc)
swiewartosc2013 <- mean(swierok2013$Wartosc)
swiewartosc2014 <- mean(swierok2014$Wartosc)
swiewartosc2015 <- mean(swierok2015$Wartosc)
swiewartosc2016 <- mean(swierok2016$Wartosc)
swiewartosc2017 <- mean(swierok2017$Wartosc)
swiewartosc2018 <- mean(swierok2018$Wartosc)
swiewartosc2019 <- mean(swierok2019$Wartosc)

ŒWIÊTOKRZYSKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(swiewartosc2006,swiewartosc2007,swiewartosc2008,swiewartosc2009,swiewartosc2010,swiewartosc2011,swiewartosc2012,swiewartosc2013,swiewartosc2014,swiewartosc2015,swiewartosc2016,swiewartosc2017,swiewartosc2018,swiewartosc2019))

plot(ŒWIÊTOKRZYSKIE, aes(x=ŒWIÊTOKRZYSKIE$Rok, y=ŒWIÊTOKRZYSKIE$srednia))+
  geom_line()


#WIELKOPOLSKIEWOJEWÓDZTWO
wiel<-ceny %>%
  filter(Nazwa == "WIELKOPOLSKIE") %>%
  select(Rok, Wartosc)

wielrok2006 <- filter(wiel, Rok == 2006)
wielrok2007 <- filter(wiel, Rok == 2007)
wielrok2008 <- filter(wiel, Rok == 2008)
wielrok2009 <- filter(wiel, Rok == 2009)
wielrok2010 <- filter(wiel, Rok == 2010)
wielrok2011 <- filter(wiel, Rok == 2011)
wielrok2012 <- filter(wiel, Rok == 2012)
wielrok2013 <- filter(wiel, Rok == 2013)
wielrok2014 <- filter(wiel, Rok == 2014)
wielrok2015 <- filter(wiel, Rok == 2015)
wielrok2016 <- filter(wiel, Rok == 2016)
wielrok2017 <- filter(wiel, Rok == 2017)
wielrok2018 <- filter(wiel, Rok == 2018)
wielrok2019 <- filter(wiel, Rok == 2019)

wielwartosc2006 <- mean(wielrok2006$Wartosc)
wielwartosc2007 <- mean(wielrok2007$Wartosc)
wielwartosc2008 <- mean(wielrok2008$Wartosc)
wielwartosc2009 <- mean(wielrok2009$Wartosc)
wielwartosc2010 <- mean(wielrok2010$Wartosc)
wielwartosc2011 <- mean(wielrok2011$Wartosc)
wielwartosc2012 <- mean(wielrok2012$Wartosc)
wielwartosc2013 <- mean(wielrok2013$Wartosc)
wielwartosc2014 <- mean(wielrok2014$Wartosc)
wielwartosc2015 <- mean(wielrok2015$Wartosc)
wielwartosc2016 <- mean(wielrok2016$Wartosc)
wielwartosc2017 <- mean(wielrok2017$Wartosc)
wielwartosc2018 <- mean(wielrok2018$Wartosc)
wielwartosc2019 <- mean(wielrok2019$Wartosc)

WIELKOPOLSKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(wielwartosc2006,wielwartosc2007,wielwartosc2008,wielwartosc2009,wielwartosc2010,wielwartosc2011,wielwartosc2012,wielwartosc2013,wielwartosc2014,wielwartosc2015,wielwartosc2016,wielwartosc2017,wielwartosc2018,wielwartosc2019))

plot(WIELKOPOLSKIE, aes(x=WIELKOPOLSKIE$Rok, y=WIELKOPOLSKIE$srednia))+
  geom_line()


#WARMIÑSKO-MAZURSKIE WOJEWÓDZTWO
warm<-ceny %>%
  filter(Nazwa == "WARMIÑSKO-MAZURSKIE") %>%
  select(Rok, Wartosc)

warmrok2006 <- filter(warm, Rok == 2006)
warmrok2007 <- filter(warm, Rok == 2007)
warmrok2008 <- filter(warm, Rok == 2008)
warmrok2009 <- filter(warm, Rok == 2009)
warmrok2010 <- filter(warm, Rok == 2010)
warmrok2011 <- filter(warm, Rok == 2011)
warmrok2012 <- filter(warm, Rok == 2012)
warmrok2013 <- filter(warm, Rok == 2013)
warmrok2014 <- filter(warm, Rok == 2014)
warmrok2015 <- filter(warm, Rok == 2015)
warmrok2016 <- filter(warm, Rok == 2016)
warmrok2017 <- filter(warm, Rok == 2017)
warmrok2018 <- filter(warm, Rok == 2018)
warmrok2019 <- filter(warm, Rok == 2019)

warmwartosc2006 <- mean(warmrok2006$Wartosc)
warmwartosc2007 <- mean(warmrok2007$Wartosc)
warmwartosc2008 <- mean(warmrok2008$Wartosc)
warmwartosc2009 <- mean(warmrok2009$Wartosc)
warmwartosc2010 <- mean(warmrok2010$Wartosc)
warmwartosc2011 <- mean(warmrok2011$Wartosc)
warmwartosc2012 <- mean(warmrok2012$Wartosc)
warmwartosc2013 <- mean(warmrok2013$Wartosc)
warmwartosc2014 <- mean(warmrok2014$Wartosc)
warmwartosc2015 <- mean(warmrok2015$Wartosc)
warmwartosc2016 <- mean(warmrok2016$Wartosc)
warmwartosc2017 <- mean(warmrok2017$Wartosc)
warmwartosc2018 <- mean(warmrok2018$Wartosc)
warmwartosc2019 <- mean(warmrok2019$Wartosc)

WARMIÑSKOMAZURSKIE<- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(warmwartosc2006,warmwartosc2007,warmwartosc2008,warmwartosc2009,warmwartosc2010,warmwartosc2011,warmwartosc2012,warmwartosc2013,warmwartosc2014,warmwartosc2015,warmwartosc2016,warmwartosc2017,warmwartosc2018,warmwartosc2019))

plot(WARMIÑSKOMAZURSKIE, aes(x=WARMIÑSKOMAZURSKIE$Rok, y=WARMIÑSKOMAZURSKIE$srednia))+
  geom_line()


#PODLASKIE WOJEWÓDZTWO
podl<-ceny %>%
  filter(Nazwa == "PODLASKIE") %>%
  select(Rok, Wartosc)

podlrok2006 <- filter(podl, Rok == 2006)
podlrok2007 <- filter(podl, Rok == 2007)
podlrok2008 <- filter(podl, Rok == 2008)
podlrok2009 <- filter(podl, Rok == 2009)
podlrok2010 <- filter(podl, Rok == 2010)
podlrok2011 <- filter(podl, Rok == 2011)
podlrok2012 <- filter(podl, Rok == 2012)
podlrok2013 <- filter(podl, Rok == 2013)
podlrok2014 <- filter(podl, Rok == 2014)
podlrok2015 <- filter(podl, Rok == 2015)
podlrok2016 <- filter(podl, Rok == 2016)
podlrok2017 <- filter(podl, Rok == 2017)
podlrok2018 <- filter(podl, Rok == 2018)
podlrok2019 <- filter(podl, Rok == 2019)

podlwartosc2006 <- mean(podlrok2006$Wartosc)
podlwartosc2007 <- mean(podlrok2007$Wartosc)
podlwartosc2008 <- mean(podlrok2008$Wartosc)
podlwartosc2009 <- mean(podlrok2009$Wartosc)
podlwartosc2010 <- mean(podlrok2010$Wartosc)
podlwartosc2011 <- mean(podlrok2011$Wartosc)
podlwartosc2012 <- mean(podlrok2012$Wartosc)
podlwartosc2013 <- mean(podlrok2013$Wartosc)
podlwartosc2014 <- mean(podlrok2014$Wartosc)
podlwartosc2015 <- mean(podlrok2015$Wartosc)
podlwartosc2016 <- mean(podlrok2016$Wartosc)
podlwartosc2017 <- mean(podlrok2017$Wartosc)
podlwartosc2018 <- mean(podlrok2018$Wartosc)
podlwartosc2019 <- mean(podlrok2019$Wartosc)

PODLASKIE <- data.frame(Rok = c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019), srednia = c(podlwartosc2006,podlwartosc2007,podlwartosc2008,podlwartosc2009,podlwartosc2010,podlwartosc2011,podlwartosc2012,podlwartosc2013,podlwartosc2014,podlwartosc2015,podlwartosc2016,podlwartosc2017,podlwartosc2018,podlwartosc2019))

plot(PODLASKIE, aes(x=PODLASKIE$Rok, y=PODLASKIE$srednia))+
  geom_line()








