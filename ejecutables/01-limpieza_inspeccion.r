#install.packages("naniar")
library(naniar)
library(ggplot2)

getwd() # Imprime directorio de trabajo
data <- read.csv('data.csv') # Carga un fichero data
head(data)  # Muestra los objetos cargados en memoria 

# Tipo de objeto
cat("El fichero es un", class(data),"de", nrow(data), "filas y", ncol(data), "columnas.")
head(data)

# Ausencias
vis_miss(data)

options(repr.plot.width=8, repr.plot.height=2.5)

for (i in 1:19){
    p <- ggplot(data)
    p <- p + geom_histogram(aes(x=data[,i], y=..density.., fill=factor(defecto)), alpha = 0.2)
    p <- p + geom_density(aes(x=data[,i], y =..density.., fill = factor(defecto), colour = factor(defecto)), alpha = 0.35)
    p <- p + scale_x_continuous(name = names(data)[i])
    p <- p + theme_minimal()
    print(p)
}

# Outliers
options(repr.plot.width=8, repr.plot.height=2.5)

for (i in 1:19){
    p <- ggplot(data)
    p <- p + geom_boxplot(aes(x=factor(defecto), y=data[,i], fill=factor(defecto)), alpha = 0.2)
    p <- p + scale_y_continuous(name = names(data)[i])
    p <- p + theme_minimal()
    print(p)
}

# quimico6 parece que es casi siempre 0.0010
table(data$quimico6)

# Eliminamos ausencias 
???? 
cat("El fichero es un", class(data),"de", nrow(data), "filas y", ncol(data), "columnas.")

# Eliminamos variables con baja varianza (quimico9)
???? 

head(data)
write.csv(data, file = "dataPrepared.csv", row.names=FALSE)
