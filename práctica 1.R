#############################################################################
#
# PRACTICA 3
#
# Expresión diferencial de genes de ratón
# Microarray de Affymetrix (Affymetrix Murine Genome U74A version 2 MG_U74Av2
# Origen de los datos: GEO GSE5583 (http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE5583)
# Publicación: Mol Cell Biol 2006 Nov;26(21):7913-28.  16940178 (http://www.ncbi.nlm.nih.gov/pubmed/16940178)
#
# Muestras: 3 Wild Type x 3 Histone deacetylase 1 (HDAC1)
#
# R código original (credits): Ahmed Moustafa
#
## ENTREGA EL 01 OCTUBRE 23:59
## Se requiere la entrega de este script completado con los códigos más las imágenes y las respuestas a las preguntas
## Adjuntar en la entrega el PDF final y el archivo con los genes
#
##############################################################################

# Instalar RCurl

if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("RCurl")

# Si esto falla, que seguro lo hace tratar de instalarlo usando el menú, Paquetes, Servidor Spain A Coruña, RCurl

# Cargamos el paquete y los datos
library(RCurl)
url = getURL ("http://bit.ly/GSE5583_data", followlocation = TRUE)
data = as.matrix(read.table (text = url, row.names = 1, header = T))

# Chequeamos las dimensiones de los datos, y vemos las primeras y las últimas filas


# Hacemos un primer histograma para explorar los datos


# Transformamos los datos con un logaritmo 
# ¿Qué pasa si hacemos una transformación logarítima de los datos? ¿Para qué sirve?
Se usa si los datos son muy grandes y si tiene una distribución sesgada positiva
Si los valores grandes están en el estudio la varianza será más constante y los
datos se normalizará

# Hacemos un boxplot con los datos transformados. ¿Qué significan los parámetros que hemos empleado?
# ¿Qué es un boxplot?
"las" es para el eje vertical, "main" para el título y "col" para los colores
Un boxplot es un diagrama de cajas, método para representar graficamente un 
conjunto de datos a través de cuartiles y así ver mejor la media, los cuartiles
y valores atípicos

boxplot(data_log)
boxplot(data_log, col=c ("blue", "blue", "blue", "orange", "orange", "orange"))

# Hacemos un hierarchical clustering de las muestras basándonos en un coeficiente de correlación
# de los valores de expresión. ¿Es correcta la separación?
Sí
hc = hclust(as.dist(l-cor(data_log)))
plot(hc, main="GSE5583 - Hierarchical Clustering")

#######################################
# Análisis de Expresión Diferencial 
#######################################

# Primero separamos las dos condiciones. ¿Qué tipo de datos has generado?
wt <- data [,1:3]
> ko <-data[,4:6]
> class (wt)

# Calcula las medias de las muestras para cada condición. Usa apply
wt.mean = apply(wt, l,mean)
head(wt.mean)
100001_at  100002_at  100003_at  100004_at  100005_at  100006_at 
  28.73333   48.73333   80.20000  284.10000 1376.30000   39.20000 

ko.mean = apply(ko, 1, mean)
head(ko.mean)
100001_at  100002_at  100003_at  100004_at  100005_at  100006_at 
  31.23333   23.03333   99.10000  389.13333 1288.10000   43.13333 

max(wt.mean)
[1] 35375.53

max(ko.mean)
[1] 37460.5

# ¿Cuál es la media más alta?
max(wt.mean)
[1] 35375.53

max(ko.mean)
[1] 37460.5

limit = max(wt.mean, ko.mean)

# Ahora hacemos un scatter plot (gráfico de dispersión)
plot(ko.mean ~ wt.mean)
plot(ko.mean ~ wt.mean, xlab = "WT", ylab= "KO", main = "GSE5583" - Scatter")


# Añadir una línea diagonal con abline
abline (0, 1, col = "red")
abline(h=2,col="blue")
abline(v=5,col="green")

# ¿Eres capaz de añadirle un grid?
grid()


# Calculamos la diferencia entre las medias de las condiciones
diff.mean = wt.mean - ko.mean


# Hacemos un histograma de las diferencias de medias
> hist(diff.mean, col="pink")

# Calculamos la significancia estadística con un t-test.
# Primero crea una lista vacía para guardar los p-values
# Segundo crea una lista vacía para guardar las estadísticas del test.
# OJO que aquí usamos los datos SIN TRANSFORMAR. ¿Por qué? 
Porque multiplicamos los datos y pueden no ser fiables. 
# ¿Cuántas valores tiene cada condición? Hay 2 condiciones y 3 muestras  

pvalue=NULL
tstat = NULL
for(i in 1 : nrow(data)) { #Para cada gen 
x = wt[i,] # gene wt número i 
y = ko[i,] # gene ko número i

#Hacemos el test
t = t.test(x, y)

#Añadimos el p-value a la lista 
pvalue[i] = t$p.value

#Añadimos las estadísticas a la lista 
tstat[i] = t$p.value

}

head(pvalue)


# Ahora comprobamos que hemos hecho TODOS los cálculos
length(pvalue) [1] 12488
La transformación es sólo para las gráficas


# Hacemos un histograma de los p-values.
# ¿Qué pasa si le ponemos con una transformación de -log10?
hist(-log10(pvalue), col = "green")


# Hacemos un volcano plot. Aquí podemos meter la diferencia de medias y la significancia estadística
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano")
En base a la diferencia, representa una variable con respecto de la otra. 


# Queremos establecer que el mínimo para considerar una diferencia significativa, es con una diferencia de 2 y un p-value de 0.01
# ¿Puedes representarlo en el gráfico?
diff.mean_cutoff = 2
pvalue_cutoff = 0.01
abline(v = diff.mean_cutoff, col = "blue", lwd = 3)
abline(h = -log10(pvalue_cutoff), col = "red", lwd = 3)


# Ahora buscamos los genes que satisfagan estos criterios
# Primero hacemos el filtro para la diferencia de medias (fold)
filter_by_diff.mean = abs(diff.mean) >= diff.mean_cutoff
dim(data[filter_by_diff.mean, ])


# Ahora el filtro de p-value
filter_by_pvalue = pvalue <= pvalue_cutoff
dim(data[filter_by_pvalue, ])


# Ahora las combinamos. ¿Cuántos genes cumplen los dos criterios?
filter_combined = filter_by_diff.mean & filter_by_pvalue
filtered = data[filter_combined,]
dim(filtered)
head(filtered)

# Ahora generamos otro volcano plot con los genes seleccionados marcados en rojo
plot(diff.mean, -log10(pvalue), main = "GSE8883 - Volcano #2")
point (diff.mean[filter_combined], -log10(pvalue[filter_combined]),col = "red) 


# Ahora vamos a marcar los que estarían sobreexpresados (rojo) y reprimidos (azul). ¿Por qué parece que están al revés?
plot(diff.mean, -log10(pvalue), main = "GSE8883 - Volcano #3")
points (diff.mean[filter_combined & diff.mean <0],
 -log10(pvalue[filter_combined & diff.mean < 0]), col = "red")
points (diff.mean[filter_combined & diff.mean >0], 
-log10(pvalue[filter_combined & diff.mean > 0]), col = "blue")
#diff.mean = wt.mean - ko.mean
Los reprimidos en el lado izquierdo


# Ahora vamos a generar un mapa. Para ello primero tenemos que hacer un cluster de las columnas y los genes 
# ¿Qué es cada parámetro que hemos usado dentro de la función heatmap?
# ¿Eres capaz de cambiar los colores del heatmap? Pista: usar el argumento col y hcl.colors
heatmap(filtered)
rowv = as.dendrogram(hclust(as.dist(1-cor(t(filtered)))))
colv = as.dendrogram(hclust(as.dist(1-cor(filtered))))
heatmap(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,labRow=FALSE)
cexCol es el tamaño de la letra del eje x
Colv y Rowv son las dendogramas


# Ahora vamos a crear un heatmap más chulo. Para ello necesitamos dos paquetes: gplots y RcolorBrewer
#if (!requireNamespace("BiocManager"))
#    install.packages("BiocManager")
#BiocManager::install(c("gplots","RColorBrewer"))
install.packages("gplots")		
install.packages("RColorBrewer")	

library(gplots)

# Hacemos nuestro heatmap
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
col = rev(greenyellow(256)), scale = "row",labRow=FALSE)
dev.off()
heatmap.2(filtered, Rowv=rowv, Colv=colv. cexCol=0.7,
+col = greenyellow(75), scale = "row", labRow=FALSE)

# Lo guardamos en un archivo PDF


# Guardamos los genes diferencialmente expresados y filtrados en un fichero
write.table (filtered, "GSE5583_DE.txt", sep = "\t",
quote = FALSE)

