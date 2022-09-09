source("./funkcja.R") 
load("ogloszenia280622")

installPackages(c("ggplot2","dplyr","stringr"))

ogloszenia<-ogloszenia280622%>%filter(!is.na(cena))

ogloszenia <- ogloszenia%>%dplyr::mutate(cena=   as.numeric(cena%>%str_replace_all("[^\\d,]", "")%>%str_replace_all(",",".")) )
ogloszenia<- ogloszenia%>%dplyr::mutate(Przebieg=as.numeric(Przebieg%>%str_replace_all("[^\\d,]","")%>%str_replace_all(",",".")))
ogloszenia<- ogloszenia%>%dplyr::mutate(Rok.produkcji=as.numeric(Rok.produkcji%>%str_replace_all("[^\\d,]","")%>%str_replace_all(",",".")))
ogloszenia<- ogloszenia%>%dplyr::mutate(Moc=as.numeric(Moc%>%str_replace_all("[^\\d,]","")%>%str_replace_all(",",".")))
ogloszenia<- ogloszenia%>%dplyr::mutate(Pojemność.skokowa=(Pojemność.skokowa%>%str_replace_all("cm3","")%>%str_replace_all("[^\\d,]","") ))
ogloszenia<- ogloszenia%>%dplyr::mutate(Pojemność.skokowa=as.numeric(Pojemność.skokowa))

ogloszenia<-ogloszenia%>% filter(waluta=="PLN")

foreot<-ogloszenia%>%filter(Marka.pojazdu=="Ford"|Marka.pojazdu=="Renault"|Marka.pojazdu=="Peugeot")%>%select(Marka.pojazdu,cena,Rok.produkcji)

#install.packages("car")
library(car)
#Dla ubuntu20.04 trzeba bylo nlopt skompilować ze źródeł !!
foreot%>%droplevels()

new_foreot <- foreot%>%filter(cena>10000 & cena<100000 & Rok.produkcji==2013)%>% group_by(Marka.pojazdu) %>% slice_sample(n=250)
shapiro.test(new_foreot$cena)
hist(new_foreot$cena)
leveneTest(cena ~ Marka.pojazdu, data=new_foreot)
kt<-kruskal.test(cena~Marka.pojazdu,new_foreot)
library(FSA)
dunnTest(cena ~ Marka.pojazdu, data = new_foreot, method = "bh")

#anovaforeot<-aov(cena~Marka.pojazdu,new_foreot)
#summary(anovaforeot)
#TukeyHSD(anovaforeot)


 
modele_ceny<-ogloszenia%>%dplyr::group_by(Model.pojazdu)%>%dplyr::summarise(n=n(),m=mean(cena))%>%dplyr::arrange(desc(m))
View(ogloszenia%>%group_by(Marka.pojazdu)%>%summarise(n=n()))

#usuniecie polskich znakow z nazw kolumn:
names(ogloszenia)<-gsub("ść","sc",names(ogloszenia))
names(ogloszenia)<-gsub("ó","o",names(ogloszenia))
names(ogloszenia)<-gsub("ę","e",names(ogloszenia))
ogloszenia<-strings2factors(ogloszenia)
View(ogloszenia)
summary(ogloszenia)
audi<-ogloszenia%>%filter(Marka.pojazdu=="Audi")
summary(audi)

audi<-audi%>%select(cena,Oferta.od,Model.pojazdu,Rok.produkcji,Przebieg,Pojemnosc.skokowa,Rodzaj.paliwa,Moc,Skrzynia.biegow,
                                Naped,Typ.nadwozia,Liczba.drzwi,Liczba.miejsc,Kolor,Stan,Bezwypadkowy,Uszkodzony)


audi$Liczba.drzwi<-as.factor(audi$Liczba.drzwi)
audi$Liczba.miejsc<-as.factor(audi$Liczba.miejsc)

installPackages("vtreat")
treatmentPlan<-design_missingness_treatment(audi,varlist=c("Skrzynia.biegow","Moc","Pojemnosc.skokowa","Naped","Liczba.miejsc","Typ.nadwozia","Bezwypadkowy","Liczba.drzwi","Przebieg","Kolor","Uszkodzony"))
prepared<-prepare(treatmentPlan,audi)
summary(prepared)
#znaki na zmienna kategorialna:
prepared$Bezwypadkowy<-as.factor(prepared$Bezwypadkowy)
prepared$Naped<-as.factor(prepared$Naped)
prepared$Typ_nadwozia<-as.factor(prepared$Typ_nadwozia)
prepared$Naped<-as.factor(prepared$Naped)
prepared$Skrzynia_biegow<-as.factor(prepared$Skrzynia_biegow)
prepared$Liczba_drzwi<-as.factor(prepared$Liczba_drzwi)
prepared$Liczba_miejsc<-as.factor(prepared$Liczba_miejsc)
prepared$Kolor<-as.factor(prepared$Kolor)
prepared$Uszkodzony<-as.factor(prepared$Uszkodzony)

installPackages(c("tidyverse","rcompanion"))

View(mixed_assoc(prepared)%>%filter(y=="cena"))


installPackages(c("regclass","caTools","randomForest","rpart","e1071","kknn","caret","mlr"))
options(scipen = 999)
set.seed(1324124)

prepared<-droplevels(prepared)
sample<-sample.split(Y=prepared, SplitRatio=.75)
trains<-subset(prepared,sample==TRUE)
tests<-subset(prepared,sample==FALSE)
linearR1<- lm(cena~.,data=trains)
predictionsR1<-predict(linearR1,tests)
MAElinearR1<-mean( abs(tests$cena-predictionsR1) ) 
MAElinearR1

 summary(linearR1)
 
 regrKNN<-train.kknn(cena~.,trains)
 predictionsKNN<-predict(regrKNN,tests)
 myMAEKNN<-mean(abs(tests$cena-predictionsKNN))
 
 #MLR:
 
 #tworzymy zadanie:
 zadanieRegresja<- mlr::makeRegrTask(id="regresja",prepared,target="cena")
 n<-getTaskSize(zadanieRegresja)
 set.seed(1324124)
 train.set<-sample(n,size=n*3/4)
 '%ni%'<-Negate('%in%')
 test.set<-seq(1,n,by=1)
 test.set<-test.set[ test.set%ni%train.set ]
 
 learner_rf<- makeLearner("regr.randomForest",id="rf")
 cost_rf<-train(learner_rf,zadanieRegresja,subset = train.set)
 fitted_rf<-predict(cost_rf,zadanieRegresja,subset=test.set)
 maeRF<- mean ( abs(fitted_rf$data$truth-fitted_rf$data$response ) )
 maeRF
















