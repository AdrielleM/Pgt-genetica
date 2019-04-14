#working directory
setwd("Z:/Fe Talarico/Aula R")

#tabela do R
head(airquality)
attach(airquality)

#plotar gr?fico
plot(airquality)
plot(Ozone)
plot(Ozone, axes=F)
#axes=F n?o coloca os eixos X e Y
plot(Ozone, ylim=c(0,150), xlim=c(0,700))
plot(Ozone, ylim=c(50,100), xlim=c(30,100))
#ylim= limita os n?meros do eixo Y; xlim= limita os n?meros do eixo X
plot(Ozone,typ='l')
plot(Ozone,typ='h')
#typ='l' gr?fico em linhas e n?o bolinhas; default = p (points); typ='h' fica igual histograma
plot(Ozone, lwd=10)
plot(Ozone, lwd.ticks=10, lwd=1)
#lwd muda o tamanho da linha/pontos; lwd.ticks muda os tra?os dos eixos
plot(Ozone, ann=F)
#ann=F n?o coloca t?tulo nos eixos
plot(Ozone, xlab="Valor", ylab="Camada de Ozonio")
#xlab e ylab d?o nomes aos eixos 

#para colocar v?rios gr?ficos na mesma janela
par(mfrow=c(2,1))
plot(Ozone, Temp, main="Ozone x Temp")
plot(Solar.R, Temp)

#Mudar os eixos
plot(Ozone, Temp, main="Ozone x Temp", axes=F) #colocar t?tulo no gr?fico e tirar os eixos
axis(1, at=seq(0, 50, by=5)) #colocar eixo de baixo, mostrando os valores de 0 a 50, de 5 em 5
axis(2, at=c(60,80), col="blue") #colocar o eixo da esquerda
axis(3, at=seq(50, 150, by=25), col="red") #colocar o eixo da direita
axis(4, at=seq(70,90))

#Colocar pontos espec?ficos
points(Temp[Temp>70], col="green", pch='C')
points(Ozone[Ozone>75], col="blue", pch='Z')
points(Ozone[Temp=100], col="red", pch='D')

#Colocar linhas
abline(v=Ozone[Ozone>125], col='yellow', lwd=5)
abline(h=Temp[Ozone>125], col='yellow', lwd=5)

#Colocar legenda
legend(110, 75, "rela??o Temp x Ozonio", col="black")
x <- mean(Temp)
legend(45, 62, x)


###3D###
library(rgl)
# 
# INPD_etnia <- read.csv("~/Documents/Banco_INPD_etnia_12082016.csv", header = T)
# pca <-  read.table("~/Downloads/ANCESTRY_INFORMATIVE_DIMENSIONS-2.mds",header=TRUE) 
#INPD_etnia$IID <- INPD_etnia$V1
# library(plyr)
# merged <- join_all(list(pca,INPD_etnia), by = "IID", type = "full", match = "all")
# merged_final <- complete.cases(merged)
# merged_final <- merged[merged_final,]
# write.table(merged_final,"~/Documents/PCSINPD.txt", row.names = F, col.names = T, quote = F)

sampa <- merged_final$site == "SP"
SP <- merged_final[sampa,]

RS <- merged_final[merged_final$site == "RS",]

# colors <- c("green","blue","red","yellow") 

plot3d(RS$C1, RS$C2, RS$C3, xlab="Component 1", ylab="Component 2", 
                zlab="Component 3", 
                col=as.integer(RS$etnia), 
                ylim = c(-0.04,0.005), xlim = c(-0.02,0.04), zlim = c(-0.01,0.01),
                box=F, size=3) 

plot3d(SP$C1, SP$C2, SP$C3, xlab="Component 1", ylab="Component 2", 
                zlab="Component 3", 
                col=as.integer(SP$etnia),
                ylim = c(-0.04,0.005), xlim = c(-0.02,0.04), zlim = c(-0.01,0.01),
                box=T, size=3)

plot3d(merged_final$C1, merged_final$C2, merged_final$C3, xlab="Component 1", ylab="Component 2", 
                   zlab="Component 3", 
                   col=as.integer(merged_final$etnia),
                   ylim = c(-0.04,0.005), xlim = c(-0.02,0.04), zlim = c(-0.01,0.01),
                   box=T, size=3)

plot3d(merged_final$C1, merged_final$C2, merged_final$C3, xlab="Component 1", ylab="Component 2", 
                   zlab="Component 3", 
                   col=as.integer(merged_final$site),
                   ylim = c(-0.04,0.005), xlim = c(-0.02,0.04), zlim = c(-0.01,0.01),
                   box=T, size=3)




# install.packages("RCircos")
library(RCircos)
data(RCircos.Histogram.Data)
head(RCircos.Histogram.Data)
data(RCircos.Heatmap.Data)
data(RCircos.Link.Data)
data(UCSC.HG19.Human.CytoBandIdeogram)
data(UCSC.Mouse.GRCm38.CytoBandIdeogram)
data(UCSC.Baylor.3.4.Rat.cytoBandIdeogram)


chr.exclude <- NULL;
cyto.info <- UCSC.HG19.Human.CytoBandIdeogram;
tracks.inside <- 10;
tracks.outside <- 0;
RCircos.Set.Core.Components(cyto.info, chr.exclude, tracks.inside, tracks.outside)
rcircos.params <- RCircos.Get.Plot.Parameters();
rcircos.cyto <- RCircos.Get.Plot.Ideogram();
rcircos.position <- RCircos.Get.Plot.Positions();
RCircos.List.Plot.Parameters()

out.file <- "RCircosDemoHumanGenome.pdf";
pdf(file=out.file, height=8, width=8, compress=TRUE)
RCircos.Set.Plot.Area()

data(RCircos.Gene.Label.Data);
name.col <- 4;
side <- "in";
track.num <- 1;
RCircos.Gene.Connector.Plot(RCircos.Gene.Label.Data,
                          track.num, side);
track.num <- 2;
RCircos.Gene.Name.Plot(RCircos.Gene.Label.Data,
                         name.col,track.num, side);
