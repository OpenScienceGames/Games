#Ejercicio para el curso de Ciencia Abierta (UGR) por 
#       Marina Torres
#       Marcelino Cabrera

# Paquetes necesarios

install.packages("corrplot")
install.packages("knitr")
install.packages("MASS")


#library(dplyr)



plot(table$Attack~table$HP, xlab="HP", ylab="Attack", pch=16, col = table$Type,
     main="Hp vs Attack")
legend("topright", legend=levels(table$Type), pch=16, col=unique(table$Type),pt.cex = 1 , cex=0.8)

plot(table$Speed~table$Attack, xlab="Attack", ylab="Speed", pch=16, col = table$Type,
     main="Speed vs Attack")
legend("topright", legend=levels(table$Type), pch=16, col=unique(table$Type),pt.cex = 1 , cex=0.8)

plot(table$Special.Attack~table$Attack, xlab="Special Attack", ylab="Attack", pch=16, col = table$Type,
     main="Special Attack vs Attack")
legend("topright", legend=levels(table$Type), pch=16, col=unique(table$Type),pt.cex = 1 , cex=0.8)

plot(table$Attack~table$Defense, xlab="Defense", ylab="Attack", pch=16, col = table$Type,
     main="Defense vs Attack")
legend("topright", legend=levels(table$Type), pch=16, col=unique(table$Type),pt.cex = 1 , cex=0.8)


parcoord(table[,4:10], var.label = TRUE)
water<- subset(table, table$Type=='WATER') [,4:10]
head(water)
parcoord(water,var.label = TRUE, col=water$Total)


tipos=unique(as.vector(table$Type))
tipos
for(i in tipos){
  filtered = subset(table, table$Type==i)
  correlation = cor(filtered[,4:10])
  corrplot(correlation, type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 30)
  
  title(sub=i, line=4)
  
  
}

