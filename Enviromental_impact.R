library(tidyverse)
library(dplyr)
library(ggplot2)
require("RColorBrewer")
Food_Production=read.csv("Food_production.csv")
Food_Production
View(Food_Production)
Food_Production$Food.product
Food_Production$Farm
Food_Production$Total_emissions
head(Food_Production)
Food_Production$Food.product
Food_Production=as.data.frame(Food_Production)
rownames(Food_Production)=Food_Production$Food.product ##Stiamo associando il valore della prima colonna come nome di righe.
Food_Production
class(Food_Production$Food.product)
class(Food_Production$Land.use.change)
head(Food_Production) ## Ci permette di poter visualizzare le prime 5 righe del dataset
View(Food_Production) ##Ci permette di poter visualizzare il sataset in una nuova scheda
class(Food_Production)  ## ci permette di verificare la classe del dataframe
dim(Food_Production) ###Ci fornisce il numero delle righe e il numero delle colonne
summary(Food_Production) ##Ci restituisce un summary dei nostri dati (valore minimo, massimo e mediana)

Food_foot_land=read.csv("food-footprints_land_use_.csv", header=TRUE, sep=";", dec=",")
##Analisi descrittiva


####ELIMINIAMO LA VARIABILE CHE NON MI CONVINCE DAL DATASET
##land.use.change
colnames(Food_Production)
Food_Production[,9]
Food_Production$Total_emissions
Food_Production[,9]=Food_Production[,9]-Food_Production[,2]
Food_Production[,9]
Food_Production=Food_Production[,-2]
Food_Production
colnames(Food_Production)




##Indici di posizione e variabilità 
Food_Production$Farm
median(Food_Production$Farm)
xbar=mean(Food_Production$Farm)
xbar<-mean(Food_Production$Farm)

xbar


#Calcolo della moda:
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

moda(Food_Production$Farm)

#Quantile
quantile(Food_Production$Farm) 

sort(Food_Production$Farm)

#Varianza
n=nrow(Food_Production)
n

c=ncol(Food_Production)
c

x=Food_Production$Farm
varianza=sum((x - xbar)^2)/n
varianza

##La formula var in Rstudio calcola la varianza campionaria corretta.
var(Food_Production$Farm)
sum((x - xbar)^2)/(n-1)

##Scarto quadratico medio:
sqrt(varianza)

####ESERCISI 2
var(Food_Production$Transport)
quantile((Food_Production$Farm))
boxplot(Food_Production$Farm, main="Farm")
boxplot(Food_Production$Packging, main="Packaging")
boxplot.stats(Food_Production$Packging)$out
boxplot.stats(Food_Production$Packging)$out

##Visual exploratori analysis
##Boxplot:
boxplot(Food_Production$Farm, main="Terreno")
boxplot(Food_Production$Total_emissions)

##Rinominiamo i nomi delle variabili
##names(Food_Production)##Serve per visualizzare tutti i nomi delle variabili
##colnames(Food_Production)[c(2:8)]=c("Utilizzo_suolo","Mangimi","Agricoltura e allevamento","Elaborazione","Trasporto","Imballaggio","Vendita al dettaglio")

mat=as.matrix(Food_Production[,2:8])
head(mat)
boxplot.stats(Food_Production$Animal.Feed)$out
which(Food_Production$Animal.Feed %in% boxplot.stats(Food_Production$Animal.Feed)$out)
Food_Production$Animal.Feed[34:43]
Food_Production$Food.product[34:43]

##Alternativa
Food_Production[Food_Production$Animal.Feed %in% boxplot.stats(Food_Production$Animal.Feed)$out,]$Food.product

##bar chart
food_footprints_land_use_
food_footprints_land_use_=as.data.frame(Food_foot_land)
Food_foot_land=food_footprints_land_use_
colnames(Food_foot_land)
Food_foot_land$Entity
Food_foot_land$Land.use.per.kilogram..Poore...Nemecek..2018.
Food_foot_land
barplot(Food_foot_land$Land.use.per.kilogram..Poore...Nemecek..2018.)
barplot(Food_foot_land$Land.use.per.kilogram..Poore...Nemecek..2018., names.arg=Food_foot_land$Entity, las=2, horiz=T)
barplot(Food_Production$Land.use.change, names.arg=Food_Production$Food.product)
barplot(Food_Production$Animal.Feed, names.arg=Food_Production$Food.product)

nrow(Food_Production)
nrow(Food_foot_land)
rownames(Food_Production)
Food_foot_land$Entity
barplot(Food_foot_land$Land.use.per.kilogram..Poore...Nemecek..2018., names.arg=Food_foot_land$Entity, col= "purple", horiz = TRUE, las=1.5, cex.names = 0.8)


?barplot
n =30
color=rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
barplot(Food_foot_land$`Land use per kilogram (Poore & Nemecek, 2018)` ,names.arg=Food_foot_land$Entity, horiz = TRUE, las=1.5, cex.axis = 0.8, cex.names = 0.7, main = "Land use change", col=color)

colnames(Food_Production)
mat=as.matrix(Food_Production[,2:7])
mat
?apply
mat1=apply(mat,2, FUN=sum)
mat1
barplot(mat1,horiz=TRUE, col=color, las=1.5, main = "Total emission for process")

barplot(mat1, horiz=FALSE,col="green", main="Emissioni per processo", cex.axis=0.8,cex.names=0.8, las=2)
barplot(mat1, horiz=TRUE,col="green", main="Emissioni per processo", cex.axis=0.8,cex.names=0.8, las=2)
barplot(mat1, horiz=TRUE,col="green", main="Emissioni per processo", cex.axis=0.8,cex.names=0.8,las=2)
##Se volessimo ordinare il grafico:
data_ordinato=mat1[order(mat1,decreasing=FALSE)]

barplot(data_ordinato, horiz=TRUE,col="green", main="Emissioni per processo", cex.axis=0.8,cex.names=0.8, las=2)


data_ordinato_1=mat1[order(mat1,decreasing = TRUE)]
barplot(data_ordinato_1, col = color, main="Emission", horiz = TRUE, las=1.5)
n =30
color=rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
?barplot
barplot(data_ordinato, horiz=TRUE,col=color[1:7], main="Emissioni per processo", cex.axis=0.6,cex.names=0.6, las=1.5, cex.main=0.9, offset=F, width=0.8)

?barplot
data_ordinato_1=mat1[order(mat1,decreasing=FALSE)]
names(data_ordinato_1)
p=barplot(data_ordinato_1, horiz=TRUE,col=color[1:7], main="Emissioni per processo", cex.axis=0.8,cex.names=0.5, las=1.5, xlim = c(0, max(data_ordinato_1) + 1))
text(y = p, x = data_ordinato_1 /2, labels =data_ordinato_1 )

##barplot per singolo processo per tutti i prodotti:
Food_Production$Food.product
barplot(Food_foot_land$`Land use per kilogram (Poore & Nemecek, 2018)`, horiz=TRUE, col=color[1:23], main="Ricambio del suolo per singolo prodotto", cex.axis=0.8,cex.names=0.5,las=1.5, axisnames=TRUE, names.arg=food_footprints_land_use_$Entity)
## Questo grafico non è molto bello...
###lo faremo poi con ggplot2
Food_Production$Animal.Feed
Food_Production$Food.product
Food_Production$Farm

##Grafico a torta base
pie(data_ordinato/sum(data_ordinato), main="C02 per processo",col=brewer.pal(7,'Spectral'))

###Aggiungiamo le percentuali:
data_ordinato
perc=round(data_ordinato/sum(data_ordinato)*100)
label_val=paste(names(data_ordinato), perc)
label_val=paste(label_val,"%",sep="")
pie(data_ordinato,labels=label_val, main="Co2 prodotta per kg di prodotto per processo produttivo",col=rainbow(9))
########################
###Bar chart con ggplot2 un pochino più carino
###bar chart with ggplot 2
library(tidyverse)
ggplot()+
  geom_bar(aes(x=reorder(Food_Production$Food.product,-Food_Production$Total_emissions),y=Food_Production$Total_emissions), stat="identity")+
  ggtitle("Total emission")+
  theme(plot.title=element_text(hjust=0.5))

###Altro modo per poter eseguire la medesima cosa.
Food_Production%>%
  ggplot()+
  geom_bar(aes(x=reorder(Food.product,-Total_emissions),y=Total_emissions), stat="identity")+
  ggtitle("Total emission")+
  theme(plot.title=element_text(hjust=0.5))

  
##Since we have only one value corresponding to each kind of food we have to set stat equal to identity
##hjust=0.5 means that the title will be centered
#in order to enlarge the bars we can add to the ggplot:
#+theme(axis.text=element_text(size=20),axis.title=element_text(size=20),plot.title=element_text(size=20))
##verticalize the food name and change the name of the x label and y label

p=ggplot()+
  geom_bar(aes(x=reorder(Food_Production$Food.product,-Food_Production$Total_emissions),y=Food_Production$Total_emissions), stat="identity")+
  ggtitle("Total emission")+
  theme(plot.title=element_text(hjust=0.5))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Type of food")+
  ylab("Total emission")

##Rotate the barchart
p+coord_flip()
##it rotatates but also the order changes to fix it we need to remove the minus before the variable for which we are ordering

p=ggplot()+
  geom_bar(aes(x=reorder(Food_Production$Food.product,Food_Production$Total_emissions),y=Food_Production$Total_emissions), stat="identity")+
  ggtitle("Total emission")+
  theme(plot.title=element_text(hjust=0.5))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Type of food")+
  ylab("Total emission")+
  coord_flip()
p
#change color
p=ggplot()+
  geom_bar(aes(x=reorder(Food_Production$Food.product,Food_Production$Total_emissions),y=Food_Production$Total_emissions), stat="identity", fill="blue")+
  ggtitle("Total emission")+
  theme(plot.title=element_text(hjust=0.5))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Type of food")+
  ylab("Total emission")+
  coord_flip()
p
##change color with respect to the value of the variable
p=ggplot()+
  geom_bar(aes(x=reorder(Food_Production$Food.product,Food_Production$Total_emissions),y=Food_Production$Total_emissions,fill=Food_Production$Total_emissions), stat="identity")+
  ggtitle("Total emission")+
  theme(plot.title=element_text(hjust=0.5))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Type of food")+
  ylab("Total emission")+
  coord_flip()+
  scale_fill_gradient(low="blue",high="red")
p

##if we want to remove the legend since we already have the x axis
p=ggplot()+
  geom_bar(aes(x=reorder(Food_Production$Food.product,Food_Production$Total_emissions),y=Food_Production$Total_emissions,fill=Food_Production$Total_emissions), stat="identity", show.legend=F)+
  ggtitle("Total emission")+
  theme(plot.title=element_text(hjust=0.5))+
  theme(axis.text.x=element_text(angle=90,hjust=1), axis.text.y=element_text(size=7, margin=margin(0, 0, 0, 0),hjust=1))+
  xlab("Type of food")+
  ylab("Total emission")+
  coord_flip()+
  scale_fill_gradient(low="blue",high="red")
p


##
##Show only the top 10 vaue
top_ten_=c("Beef (beef herd)","Lamb & Mutton","Beef (dairy herd)", "Cheese", "Coffee","Shrimps (farmed)", "Olive Oil", "Pig Meat","Fish (farmed)","Palm Oil")
data_top_10=Food_Production[top_ten_,]
data_top_10
p=ggplot()+
  geom_bar(aes(x=reorder(data_top_10$Food.product,data_top_10$Total_emissions),y=data_top_10$Total_emissions,fill=data_top_10$Total_emissions), stat="identity", show.legend=FALSE)+
  ggtitle("Total emission")+
  theme(plot.title=element_text(hjust=0.5))+
  theme(axis.text.x=element_text(angle=90,hjust=1), axis.text.y=element_text(size=10))+
  xlab("Type of food")+
  ylab("Total emission")+
  coord_flip()+
  scale_fill_gradient(low="blue",high="red")
##non riesco a rimetterlo come decrescente
p



####Come selezionare solo le variabili che sono presenti nel nostro dataset 
##eliminiamo gli na value presenti nel dataset.

Food_Production%>%
  na.omit()%>%
  ggplot()+
  geom_bar(aes(x=reorder(Food.product,Freshwater.withdrawals.per.kilogram..liters.per.kilogram.),y=Freshwater.withdrawals.per.kilogram..liters.per.kilogram.,fill=Freshwater.withdrawals.per.kilogram..liters.per.kilogram.), stat="identity", show.legend=FALSE)+
  ggtitle("Totale acqua utilizzata")+
  theme(plot.title=element_text(hjust=0.5))+
  theme(axis.text.x=element_text(angle=1,hjust=0.5, size=8), axis.text.y=element_text(size=10))+
  xlab("Type of food")+
  ylab("Total emission")+
  coord_flip()




Food_Production%>%
  na.omit()%>%
  ggplot(aes(x=reorder(Food.product,Freshwater.withdrawals.per.kilogram..liters.per.kilogram.),y=Freshwater.withdrawals.per.kilogram..liters.per.kilogram.,fill=Freshwater.withdrawals.per.kilogram..liters.per.kilogram.))+
  geom_bar(stat="identity", show.legend=FALSE)+
  geom_text(aes(label=round(Freshwater.withdrawals.per.kilogram..liters.per.kilogram.,0),hjust=place),hjust=0, vjust=0.5)+
  ggtitle("Litri di acqua utilizzata per prodotto")+
  theme(plot.title=element_text(hjust=1))+
  theme(axis.text.x=element_text(angle=1,hjust=0.5, size=8), axis.text.y=element_text(size=10))+
  xlab("Type of food")+
  ylab("Total emission")+
  coord_flip()+
  theme_bw()

##bar plot with multiple value
#subset of the datafram
head(Food_Production)
Food_Production[,2:7]
sub_data=Food_Production[,1:8]
sub_data
###Qui stavo cercando di capire cosa fosse la variabile Total emission

Food_Production$Total_emissions
Food_Production[1,]
sum(Food_Production[1,2:7])
Food_Production$Total_emissions
Food_Production[2,]
sum(Food_Production[2,2:7])

##Stacked bar chart
##Is a type of graph that represents the proportional contribution of individual data points in comparison to a total

library(ggthemes)
library(scales)
library(dplyr)
library(ggplot2)
ggplot(data=sub_data,aes(x=Food.product,y=Total_emissions))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels=scales::percent)

head(sub_data)
sub_data=sub_data[,-8]
sub_data
##Provo con il mio dataset
nuovo_d=melt(sub_data,id.vars="Food.product")
nuovo_d

###Altro metodo
library(tidyr)
data_d1=Food_Production%>%
  pivot_longer(cols=2:7,
               names_to="variable",
               values_to="values")
data_d1$Total_emissions
data_d2=Food_Production%>%
  pivot_longer(cols=2:7,
               names_to="variable",
               values_to="values")

             
library(scales)

####Dataset ordinato
##Cambiare il nome dell'asse delle y
p=ggplot(data_d1, aes(x=reorder(Food.product,Total_emissions), y=values, fill = variable)) +
  geom_bar( stat = "identity")+coord_flip()
p


###Creiamo dei sottodataset

Food_Production$Food.product
var_animali=c("Beef (beef herd)","Pig Meat","Cheese","Shrimps (farmed)","Beef (dairy herd)","Poultry Meat","Eggs","Lamb & Mutton","Milk","Fish (farmed)")
data_animale=Food_Production[var_animali,]
data_animale

pie(data_animale$Transport,labels=data_animale$Food.product, main="Transport c02", col=brewer.pal(length(data_animale$Transport),'Spectral'))
###Aggiungiamo le percentuali:
data_animale
perc=round(data_animale$Transport/sum(data_animale$Transport)*100)
label_val=paste(data_animale$Food.product, perc)
label_val=paste(label_val,"%",sep="")
pie(data_animale$Transport,labels=label_val, main="C02 per Trasporto",col=rainbow(11))
data_animale$Total_emissions
(p <- data_animale%>%ggplot(aes(x=reorder(Food.product,Total_emissions),y=Total_emissions, fill = Total_emissions)) +
    geom_bar( stat = "identity", show.legend=FALSE)+coord_flip())
p


###Potremmo andare ad aggiungere dettagli.

##Stacked bar plot solo con sottodataset animali

data_d_anim=data_animale%>%
  pivot_longer(cols=2:7,
               names_to="variable",
               values_to="values")
data_d_anim$Food.product
data_d_anim$Total_emissions
data_d_anim$values
data_d_anim$variable

#p <- ggplot(data_d_anim, aes(x=reorder(Food.product,Total_emissions),y=values, fill = variable) +
    #geom_bar( stat = "identity")+coord_flip()
    
p <- ggplot(data_d_anim, aes(x=Food.product,y=values, fill = variable)) +
                  geom_bar( stat = "identity")+coord_flip()
p

p <- ggplot(data_d_anim, aes(x=reorder(Food.product, Total_emissions),y=values, fill = variable)) +
  geom_bar( stat = "identity")+coord_flip()+
  labs(x="Prodotti",y="Emissioni totali")

p




var_vegetali=c("Wheat & Rye (Bread)","Oatmeal","Cassava","Other Pulses","Groundnuts","Soybean Oil","Rapeseed Oil","Onions & Leeks","Other Vegetables","Apples","Other Fruit","Maize (Meal)","Rice","Cane Sugar","Soymilk","Peas","Palm Oil","Root Vegetables","Citrus Fruit","Berries & Grapes","Coffee","Barley (Beer)","Potatoes","Beet Sugar","Nuts","Tofu","Sunflower Oil","Tomatoes","Brassicas","Bananas","Wine","Dark Chocolate", "Olive Oil")
dataset_vegetale=Food_Production[var_vegetali,]

somma_total_emission_animale=sum(data_animale$Total_emissions)
somma_total_emission_vegetale=sum(dataset_vegetale$Total_emissions)

data_insieme=as.matrix(c(somma_total_emission_animale,somma_total_emission_vegetale), byrow=T)
data_insieme=data.frame(first_column=c("tot_emiss_animale","tot_emiss_vegetale"),second_column=c(163.9,86.9))
data_insieme


table(data_insieme)
?barplot
barplot(data_insieme$second_column,horiz=T,col=c(3:4),cex.names=0.9, names.arg=data_insieme$first_column)


#####################################
##Stacked barplot solo con i dati vegetali
###############################

data_d_vegetali=dataset_vegetale%>%
  pivot_longer(cols=2:7,
               names_to="variable",
               values_to="values")

p <- ggplot(data_d_vegetali, aes(x=reorder(Food.product, Total_emissions),y=values, fill = variable)) +
  geom_bar( stat = "identity")+coord_flip()+
  labs(x="Prodotti",y="Emissioni totali")
p


####Eliminare i dati nulli
dato3=Food_Production%>%
  select(Food.product,Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal.)%>%
  na.omit()
dato3

##Terzo metodo metodo
dato2=Food_Production%>%
  select(Food.product,Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal.)%>%
  filter(complete.cases(.))
dato2

##Opposto, prendiamo solo i valori nulli
dato2_=Food_Production%>%
  select(Food.product,Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal.)%>%
  filter(!complete.cases(.))

dato2_

barplot(dato2$Freshwater.withdrawals.per.1000kcal..liters.per.1000kcal.,names.arg=dato2$Food.product,las=2,horiz=TRUE)


####VOLENDO AVREMMO POTUTO RINOMINARE PRIMA LA VARIABILE

##colnames(mydata)[2] <- "Tempo"
##colnames(dataset)[posizione_variabile]="Nuovo nome variabile"

#Esempio

Food_Production$Eutrophying.emissions.per.1000kcal..gPO.eq.per.1000kcal.
colnames(Food_Production)

colnames(Food_Production)[10]="Eutrofizzazione per 1000kcal"

colnames(Food_Production)

rownames(Food_Production)

rownames(Food_Production)[4]="Avena"
rownames(Food_Production)



############################################
### SOTTODATASET CON SOLO GLI OLI.
###########################################


variabili_olio=c("Palm Oil","Soybean Oil","Rapeseed Oil","Sunflower Oil","Olive Oil")
dataset_olio=Food_Production[variabili_olio,]

data_d_olio=dataset_olio%>%
  pivot_longer(cols=2:7,
               names_to="variable",
               values_to="values")

p <- ggplot(data_d_olio, aes(x=reorder(Food.product, Total_emissions),y=values, fill = variable)) +
  geom_bar( stat = "identity")+coord_flip()+
  labs(x="Tipologie oli", y="Emissioni totali")

p


food_footprints_land_use_$Entity

rownames(Food_foot_land)=Food_foot_land$Entity
##Nel dataset con l'utilizzo del suolo
Food_foot_land_oli=c("Palm Oil","Soybean Oil","Rapeseed Oil","Sunflower Oil","Olive Oil")
Land_oli=Food_foot_land[Food_foot_land_oli,]
Land_oli
Land_oli$Entity
p <- ggplot(Land_oli, aes(x=reorder(Land_oli$Entity ,Land_oli$Land.use.per.kilogram..Poore...Nemecek..2018.), y=Land_oli$Land.use.per.kilogram..Poore...Nemecek..2018., fill=Land_oli$Entity)) +
  geom_bar( stat = "identity", show.legend = F)+
  labs(x="Tipologie di olio",y="m^2 di terreno utilizzati per litro di prodotto")+
  coord_flip()

p

###Questo perchè sappiamo che per produrre 10/20 kg di olio si utilizzano circa
## circa 100kg di olive e la produzione dell'olio di oliva avviene una volta l'anno

##


###Dplyr puo' anche essere utilizzato per unire i dataset

colnames(food_footprints_land_use_)[1]="Food.product"
food_footprints_land_use_$Food.product
Food_Production$Food.product

##Sistemiamo le differenze
rownames(Food_Production)[43]="Crumstaceans (farmed)"
rownames(Food_Production)
rownames(food_footprints_land_use_)=food_footprints_land_use_$Food.product
food_footprints_land_use_$Food.product[43]
food_footprints_land_use_$Food.product[34]="Beef (beef herd)"
food_footprints_land_use_$Food.product[35]="Beef (dairy herd)"
food_footprints_land_use_$Food.product[43]="Shrimps (farmed)"
match=left_join(Food_Production,food_footprints_land_use_,by="Food.product")
match
View(match)

##############################
###K-MEANS CLUSTER ANALYSIS
#############################

library(factoextra)
sub_data_labels=sub_data[,1]
sub_data=sub_data[,-1]
sub_data

##Esempio solo a scopo informativo
##Tendenzialmente i dati prima di eseguire il k-means cluster devono essere 
#riportati sulla stessa scala di misura altrimenti il calcolo delle distanze non porta a risultati buono.
mean(sub_data$Processing)
sub_data_scale=scale(sub_data)
sub_data_scale
sub_data
head(sub_data)
####How to select different columns from the data
Food_Production[,c(6,4)]

##Calculate the distance metrics between our observations
data_dis=dist(sub_data_scale)
colnames(sub_data)
?dist
data_dis=dist(sub_data)
data_dis
sub_data
sub_data_new=sub_data[,2:6]
sub_data
##Nel nostro caso i dati rappresentano già la medesima unità di misura
#non c'è bisogno di scalarli

##calcoliamo quanti cluster ci servono
fviz_nbclust(sub_data,kmeans,method="wss")+
  labs(subtitle="Elbow method")
fviz_nbclust(sub_data,kmeans,method="silhouette")+
  labs(subtitle="Silhouette method")

head(sub_data)
km.out=kmeans(sub_data,centers=2, nstart=100)
print(km.out)

##Per vedere le caratteristiche delle osservazioni appartenenti ai due dati
km.out$centers
km.out$cluster
km.cluster=km.out$cluster
fviz_cluster(list(data=sub_data ,cluster=km.cluster))

km.out=kmeans(sub_data,centers=4, nstart=100)
km.cluster=km.out$cluster
fviz_cluster(list(data=sub_data ,cluster=km.cluster))


##calcoliamo quanti cluster ci servono
fviz_nbclust(sub_data_new,kmeans,method="wss")+
  labs(subtitle="Elbow method")
fviz_nbclust(sub_data_new,kmeans,method="silhouette")+
  labs(subtitle="Silhouette method")


km.out=kmeans(sub_data_new,centers=2, nstart=100)
print(km.out)

##Per vedere le caratteristiche delle osservazioni appartenenti ai due dati
km.out$centers
km.cluster=km.out$cluster
fviz_cluster(list(data=sub_data_new ,cluster=km.cluster))

km.out=kmeans(sub_data_new,centers=2, nstart=100)
km.cluster=km.out$cluster
fviz_cluster(list(data=sub_data_new ,cluster=km.cluster))



sub_data
Farm_Animal=sub_data[,1:2]
Farm_Animal

fviz_nbclust(Farm_Animal,kmeans,method="silhouette")+
  labs(subtitle="Silhouette method")
km.out=kmeans(Farm_Animal,centers=2, nstart=100)
##Per vedere le caratteristiche delle osservazioni appartenenti ai due dati
##Sono i due parametri su cui si fa molto leva ultimamente, mangiare a km zero e packaging

km.out$centers
km.cluster=km.out$cluster
fviz_cluster(list(data=sub_data ,cluster=km.cluster))


##with respect to packaging and transport
Transp_pack=sub_data[,4:5]



##Dataset Scaled land use e water withdrawals
colnames(match)
rownames(match)=match$Food.product
water_land=match%>%
  na.omit()
rownames(water_land) 
water_land[,c(14,23)]
dataset_scaled=scale(water_land[,c(14,23)])
dataset_scaled
?fviz_nbclust
fviz_nbclust(dataset_scaled,kmeans,method="wss")+
  labs(subtitle="Elbow method")
fviz_nbclust(dataset_scaled,kmeans,method="silhouette")+
  labs(subtitle="Silhouette method")

km.out=kmeans(dataset_scaled,centers=4, nstart=100)
print(km.out)

km.out$centers
km.cluster=km.out$cluster
fviz_cluster(list(data=dataset_scaled ,cluster=km.cluster))


###Clustering by transport e packaging
sub_data
transp_pack=sub_data[,c(4,5)]
transp_pack

fviz_nbclust(transp_pack,kmeans,method="wss")+
  labs(subtitle="Elbow method")
fviz_nbclust(transp_pack,kmeans,method="silhouette")+
  labs(subtitle="Silhouette method")
km.out=kmeans(transp_pack,centers=2, nstart=100)
print(km.out)
km.cluster=km.out$cluster
fviz_cluster(list(data=transp_pack ,cluster=km.cluster))
km.out$centers



###Mahalanobis distance
dist_mal <- mahalanobis(dataset_scaled, colMeans(dataset_scaled), cov(dataset_scaled))
?kmeans
km.cluster=kmeans(dist_mal,centers=4)$cluster
fviz_cluster(list(data=dataset_scaled ,cluster=km.cluster))


##HIERARCHICAL CLUSTERING
library(cluster)
##Find the linkage method to use
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
#function to compute agglomerative coefficient
ac <- function(x) {
  agnes(dataset_scaled, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac)
##We have to use the one that is the hghest
##ward or complete
##Wards minimum variance
#perform hierarchical clustering using Ward's minimum variance
clust <- agnes(dataset_scaled, method = "ward")

#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 
##Determine the optimal numer of clusters
##WE can use the metrics known as the gap statistics
#which compare the total intracluster variations for different values of k
#with their expected value for a distribution with no clustering

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(dataset_scaled, FUN = hcut, nstart = 25, K.max = 10, B = 50)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

##then we can apply the relative cluster labels onto the dataset
##we can use the cutree method to define the number of cluster

d <- dist(dataset_scaled, method = "euclidean")

#perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )

#cut the dendrogram into 4 clusters
groups <- cutree(final_clust, k=8)

table(groups)

final_data <- cbind(dataset_scaled, cluster = groups)

#display first six rows of final data
head(final_data)

##To find the mean of each group
#find mean values for each cluster
is.data.frame(final_data)
final_data=as.data.frame(final_data)
aggregate(final_data, by=list(cluster=final_data$cluster), mean)

########################
##Hierarchical clustering con tutte le variabili
sub_data_new

ac <- function(x) {
  agnes(sub_data_new, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac)
##We have to use the one that is the hghest
##ward or complete
##Wards minimum variance
#perform hierarchical clustering using Ward's minimum variance
clust <- agnes(sub_data_new, method = "ward")

#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 
##Determine the optimal numer of clusters
##WE can use the metrics known as the gap statistics
#which compare the total intracluster variations for different values of k
#with their expected value for a distribution with no clustering

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(sub_data_new, FUN = hcut, nstart = 25, K.max = 10, B = 50)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

##then we can apply the relative cluster labels onto the dataset
##we can use the cutree method to define the number of cluster

d <- dist(dataset_scaled, method = "euclidean")

#perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )

#cut the dendrogram into 4 clusters
groups <- cutree(final_clust, k=3)

table(groups)

final_data <- cbind(dataset_scaled, cluster = groups)

#display first six rows of final data
head(final_data)

##To find the mean of each group
#find mean values for each cluster
is.data.frame(final_data)
final_data=as.data.frame(final_data)
aggregate(final_data, by=list(cluster=final_data$cluster), mean)


####con anche variabile farm
sub_data

ac <- function(x) {
  agnes(sub_data, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac)
##We have to use the one that is the hghest
##ward or complete
#perform hierarchical clustering using Ward's minimum variance
clust <- agnes(sub_data, method = "ward")

#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 
##Determine the optimal numer of clusters
##WE can use the metrics known as the gap statistics
#which compare the total intracluster variations for different values of k
#with their expected value for a distribution with no clustering

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(sub_data, FUN = hcut, nstart = 25, K.max = 10, B = 50)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

##then we can apply the relative cluster labels onto the dataset
##we can use the cutree method to define the number of cluster

d <- dist(sub_data, method = "euclidean")

#perform hierarchical clustering using Ward's method
final_clust <- hclust(d, method = "ward.D2" )

#cut the dendrogram into 4 clusters
groups <- cutree(final_clust, k=3)

table(groups)

final_data <- cbind(sub_dat, cluster = groups)

#display first six rows of final data
head(final_data)

##To find the mean of each group
#find mean values for each cluster
is.data.frame(final_data)
final_data=as.data.frame(final_data)
aggregate(final_data, by=list(cluster=final_data$cluster), mean)
