
#Importar el dataset
Terrorismo <- read.csv(file="D:/Personal/Master_Data_Science/Tipologia y ciclo de vida/globalterrorismdb_0617dist.csv", header=TRUE, sep=",")

#Explorar los datos 
str(Terrorismo) #Proporciona la estructura del conjunto de datos
names(Terrorismo) #Lista variables en el conjunto de datos
head(Terrorismo) # Muesra las primeras 6 filas de conjunto de datos

#Identificar y seleccionar los datos de interes a analizar
Var_Terrorismo=subset(Terrorismo,select=c(iyear,imonth,iday,country_txt,region_txt,latitude,longitude,attacktype1_txt,targtype1_txt,weaptype1_txt,nkill,nwound,gname,motive,success))
head(Var_Terrorismo)

#Renombrar las columnas
library(plyr)
Var_Terrorismo <- rename(Var_Terrorismo, c(iyear="Año"))
Var_Terrorismo <- rename(Var_Terrorismo, c(imonth="Mes"))
Var_Terrorismo <- rename(Var_Terrorismo, c(iday="Dia"))
Var_Terrorismo <- rename(Var_Terrorismo, c(country_txt="Pais"))
Var_Terrorismo <- rename(Var_Terrorismo, c(region_txt="Region"))
Var_Terrorismo <- rename(Var_Terrorismo, c(latitude="Latitud"))
Var_Terrorismo <- rename(Var_Terrorismo, c(longitude="Longitud"))
Var_Terrorismo <- rename(Var_Terrorismo, c(attacktype1_txt="Tipo_Ataque"))
Var_Terrorismo <- rename(Var_Terrorismo, c(targtype1_txt="Objetivo"))
Var_Terrorismo <- rename(Var_Terrorismo, c(weaptype1_txt="Tipo_Arma"))
Var_Terrorismo <- rename(Var_Terrorismo, c(nkill="Muertos"))
Var_Terrorismo <- rename(Var_Terrorismo, c(nwound="Heridos"))
Var_Terrorismo <- rename(Var_Terrorismo, c(gname="Grupo"))
Var_Terrorismo <- rename(Var_Terrorismo, c(motive="Motivo"))
Var_Terrorismo <- rename(Var_Terrorismo, c(success="Exitosos"))

#identificar si exiten elementos vacios
colSums(is.na(Var_Terrorismo))

#identificar si exiten outliers
library(outliers)
chisq.out.test(Var_Terrorismo$Muertos)

#Remover los outliers
#Var_Terrorismo <- Var_Terrorismo[Var_Terrorismo$Muertos != 1500,] 
chisq.out.test(Var_Terrorismo$Muertos)

#seleccionar el grupo de datos
MxA= Var_Terrorismo[!is.na(Var_Terrorismo$Muertos),]
MxA= subset(MxA,select=c(Muertos, Año)) 

#Comprobacion Varianza
#Test de Bartlett - homogeneidad de la varianza
bartlett.test(MxA$Muertos~MxA$Año, data=MxA)

#Test de Fligner - homogeneidad de la varianza
fligner.test(MxA$Muertos~MxA$Año, data=MxA)

#Test de Levene - Desviacion absoluta de la media
library(lawstat)
levene.test(MxA$Muertos,MxA$Año)

#Test de Shapiro - Desviacion absoluta de la media
shapiro.test(MxA$Muertos)

#Test de Wilcoxon
wilcox.test(MxA$Muertos,MxA$Año, data=MxA)

#Test de kruskal
kruskal.test(MxA$Muertos,MxA$Año, data=MxA)

#Representacion de los datos
#Mapa interactivo de ataques en el mundo
library(leaflet)
Mapa <- 
  leaflet() %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
  attribution='Map tiles by 
    <a href="http://stamen.com">Stamen Design</a>, 
    <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> 
    &mdash; 
    Map data &copy; 
    <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
  setView(15, 40, zoom= 2)

Mapa %>% addCircles (data=Var_Terrorismo, lat= ~Latitud, lng = ~Longitud, 
              popup=paste(
                "<strong>Año: </strong>", Var_Terrorismo$Año,
                "<br><strong>Pais: </strong>", Var_Terrorismo$Pais, 
                "<br><strong>Tipo Ataque: </strong>", Var_Terrorismo$Tipo_Ataque, 
                "<br><strong>Objetivo: </strong>", Var_Terrorismo$Objetivo,
                "<br><strong>Tipo Arma: </strong>", Var_Terrorismo$Tipo_Arma),
              weight = 0.8, color="#8B1A1A", stroke = TRUE, fillOpacity = 0.6)

#Numero de ataques por año
library(highcharter)
library(dplyr)
Por_Año <- Var_Terrorismo[!is.na(Var_Terrorismo$Año),] %>% group_by(Año) %>% summarise(Ataques = n())
hchart(Por_Año, type = "line", hcaes(x = Año, y = Ataques ,  color = Ataques )) %>%
    hc_title(text = "Numero de ataques por año") %>%
        #hc_subtitle(text = "Tree Map of the Number of Attacks")%>%
		hc_add_theme(hc_theme_google())


#distribucion de ataques por pais (Top 20)

Por_Pais <- Var_Terrorismo %>% group_by(Pais) %>% summarise(Total = n()) %>% arrange(desc(Total)) %>% head(20)
hchart(Por_Pais, type = "treemap", hcaes(x = Pais, value = Total, color = Total)) %>%
    hc_title(text = "Distribucion de ataques por pais (Top 20) ") %>%
        #hc_subtitle(text = "Tree Map of the Number of Attacks")%>%
		hc_add_theme(hc_theme_google())

#Numero de muertos por pais (Top 20)
Muertos_pais <- Var_Terrorismo[!is.na(Var_Terrorismo$Muertos),] %>% group_by(Pais) %>% summarise(Victimas =  round(sum(Muertos))) %>% arrange(desc(Victimas)) %>% head(20)
hchart(Muertos_pais,type="bar", hcaes(x = Pais, y = Victimas, value = Victimas, color = Victimas)) %>%
        hc_title(text = "Numero de victimas por pais (Top 20)") %>%
              hc_add_theme(hc_theme_google())

#Regiones mas afectadas por el terrosismo basado en el numero de muertos 
Muertos_region <- Var_Terrorismo[!is.na(Var_Terrorismo$Muertos),] %>% group_by(Region) %>% summarise(Total = sum(Muertos)) %>% arrange(desc(Total))
hchart(Muertos_region,type="column", hcaes( x = Region, y=Total, value = Total, color = Total)) %>%
        hc_title(text = "Numero de victimas por region") %>%
              hc_add_theme(hc_theme_google())

#Distribucion por tipo de ataques
Ataques <- Var_Terrorismo[!is.na(Var_Terrorismo$Tipo_Ataque),] %>% group_by(Tipo_Ataque) %>% summarise(Total = n())
hchart(Ataques ,type="column", hcaes( x = Tipo_Ataque, y=Total, value = Total, color = Total)) %>%
        hc_title(text = "Distribucion por tipo de ataques") %>%
              hc_add_theme(hc_theme_google())

#Numero de ataques por grupo (Top 20)
Grupos <- Var_Terrorismo[!is.na(Var_Terrorismo$Grupo),] %>% filter(Var_Terrorismo$Grupo != "Unknown") %>% group_by(Grupo) %>% summarise(Total = n()) %>% arrange(desc(Total)) %>% head(10)
hchart(Grupos ,type="pie", hcaes( x = Grupo , y=Total, value = Total, color = Total)) %>%
        hc_title(text = "Numero de ataques por grupo terrorista (Top 10)") %>%
              hc_add_theme(hc_theme_google())


#Numero de ataques por objetivos (Top 10)
Objetivos <- Var_Terrorismo[!is.na(Var_Terrorismo$Objetivo),] %>% filter(Var_Terrorismo$Objetivo != "Unknown") %>% group_by(Objetivo) %>% summarise(Ataques = n()) %>% arrange(desc(Ataques))
hchart(Objetivos ,type="bar", hcaes( x = Objetivo , y=Ataques, value = Ataques, color = Ataques)) %>%
        hc_title(text = "Numero de ataques por objetivos") %>%
              hc_add_theme(hc_theme_google())

#Exportar la data final
Var_TerrorismoNA <- na.omit(Var_Terrorismo)
write.csv(Var_TerrorismoNA, file = "D:/Personal/Master_Data_Science/Tipologia y ciclo de vida/globalterrorismdb_0617dist_Clean.csv")





