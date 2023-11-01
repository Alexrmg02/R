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
## ENTREGA EL 22 OCTUBRE 23:59
## Se requiere la entrega de un Jupyter Notebook con los códigos más las imágenes y las respuestas a las preguntas
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
dim (data) #Para medir las dimensiones
head(data) #Para ver las primeras filas
tail(data) #Paraver las ultimas filas

# Hacemos un primer histograma para explorar los datos
hist (data)#Para hacer el histograma
hist (data, col= "gray",main=GSE5583-Histograma")#Para cambiar el color de nuestro histograma

# Transformamos los datos con un logaritmo 
# ¿Qué pasa si hacemos una transformación logarítima de los datos? ¿Para qué sirve?
data_log=log2(data) #Para calcular el logaritmo en base 2 de los valores en el vector data
hist (data_log) #Para ver el histograma de data_log

# Hacemos un boxplot con los datos transformados. ¿Qué significan los parámetros que hemos empleado?
# ¿Qué es un boxplot?
# Es un grafico de caja a partir de los datos obtenidos en el vector y se utiliza para proporcionar 
informacion sobre la mediana cuartiles y la presencia de valores atipicos
boxplot(data_log) #Para crear el grafico
boxplot(data_log, col=c("blue","blue","blue", "orange","orange", "orange"),#(Para distribuir en 5 grupos por color los datos del vector data2)
main="GSE5583-boxplots", las=2)   #(Para poner un titulo al grafico y (las=2) para controlar la orientacion)

# Hacemos un hierarchical clustering de las muestras basándonos en un coeficiente de correlación
# de los valores de expresión. ¿Es correcta la separación?
hc = hclust(as.dist (1-cor(data_log)))
plot(hc, main = "GSE5583 - Hierarchical Clustering")


#######################################
# Análisis de Expresión Diferencial 
#######################################

# Primero separamos las dos condiciones. ¿Qué tipo de datos has generado?
# Una matriz
vt <-data[,1:3]
ko <-data[,4:6]
class(wt)
head(wt)
# Calcula las medias de las muestras para cada condición. Usa apply
wt.mean = apply(wt, 1, mean) #llamamos a la variable  wt (...) para guardar las medias de wt
head(wt.mean) #Para ver las primeras filas
ko.mean = apply(ko, 1, mean) #llamamos a la variable ko (...) para guardar las medias de los no caut
head(ko.mean) #Para ver las medias de las primeras filas
# ¿Cuál es la media más alta?
max(wt.mean)
max(ko.mean)
#la media mas alta es de los Ko

# Ahora hacemos un scatter plot (gráfico de dispersión)
plot(ko.mean ~ wt.mean)
plot(ko.mean ~ wt.mean, xlab = "WT", ylab = "ko", main = "GSE5583 - Scatter")

# Añadir una línea diagonal con abline
abline(0, 1, col = "red") # b=0, y= 1*x

#para una horizontal
abline (h=2, col = "blue")
#Para horizontal
abline (v=5, col = "green")
# ¿Eres capaz de añadirle un grid?
grid()
#(cuadricula)
# Calculamos la diferencia entre las medias de las condiciones
diff.mean = wt.mean - ko.mean

# Hacemos un histograma de las diferencias de medias
hist(diff.mean)
diff.mean_log=log2 (diff.mean)
hist(diff.mean_log, col = "red")
# Calculamos la significancia estadística con un t-test.
# Primero crea una lista vacía para guardar los p-values
# Segundo crea una lista vacía para guardar las estadísticas del test.
# OJO que aquí usamos los datos SIN TRANSFORMAR. ¿Por qué?
# ¿Cuántas valores tiene cada muestra?
pvalue = NULL
tstat = NULL
for(i in 1 : nrow(data)) { #Para cada fila de data un gen 
	x = wt[i,] #Para wt número i, son vectores que reunen las columnas 3 de wt y 3 de i lo mismo en la de abajo
	y = ko[i,] #Para ko número i
	#Hacemos el test
	t = t.test (x, y)
	#Para añadir el p-value a la lista
	pvalue[i] = t$p.value
	#Añadimos las estadísticas a la lista
	tstat[i] = t$statistic
}

head(pvalue)

# Ahora comprobamos que hemos hecho TODOS los cálculos
length(pvalue)#Numero total de genes

# Hacemos un histograma de los p-values.
# ¿Qué pasa si le ponemos con una transformación de -log10?
hist(pvalue)
hist(-log10(pvalue), col = "red")

# Hacemos un volcano plot. Aquí podemos meter la diferencia de medias y la significancia estadística
plot(diff.mean, -log10(pvalue), main = "GSE5583 - volcano")

# Queremos establecer que el mínimo para considerar una diferencia significativa, es con una diferencia de 2 y un p-value de 0.01
# ¿Puedes representarlo en el gráfico?
diff.mean_cutoff = 2
pvalue_cutoff = 0.01
abline(v = diff.mean_cutoff, col = "blue", lwd = 3)
#abline(v = -diff.mean_cutoff, col = "red", lwd = 3)(si pusieramos para el -2)
abline(h = -log10(pvalue_cutoff), col = "green", lwd = 3)


# Ahora buscamos los genes que satisfagan estos criterios
# Primero hacemos el filtro para la diferencia de medias (fold)
filter_by_diff.mean = abs(diff.mean) >=diff.mean_cutoff
dim(data[filter_by_diff.mean, ])

# Ahora el filtro de p-value
filter_by_pvalue = pvalue <= pvalue_cutoff
dim(data[filter_by_pvalue, ])

# Ahora las combinamos. ¿Cuántos genes cumplen los dos criterios?
filter_combined = filter_by_diff.mean & filter_by_pvalue
filtered = data [filter_combined,]
dim(filtered)
head(filtered)


# Ahora generamos otro volcano plot con los genes seleccionados marcados en rojo
plot (diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #2")
points (diff.mean[filter_combined], -log10(pvalue[filter_combined]), col = "red")

# Ahora vamos a marcar los que estarían sobreexpresados (rojo) y reprimidos (azul). ¿Por qué parece que están al revés?
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #3")
points (diff.mean[filter_combined & diff.mean < 0],
-log10(pvalue[filter_combined & diff.mean < 0]), col = "red")
points (diff.mean[filter_combined & diff.mean > 0],
-log10(pvalue[filter_combined & diff.mean > 0]), col = "blue")

# Ahora vamos a generar un mapa. Para ello primero tenemos que hacer un cluster de las columnas y los genes 
# ¿Qué es cada parámetro que hemos usado dentro de la función heatmap?
# ¿Eres capaz de cambiar los colores del heatmap? Pista: usar el argumento col y hcl.colors
heatmap(filtered)
rowv = as.dendrogram(hclust(as.dist(1-cor(t(filtered)))))
colv = as.dendrogram(hclust(as.dist(1-cor(filtered))))
heatmap(filtered, Rowv=rowv,Colv=colv, cexCol=0.7,labRow=FALSE)

# Ahora vamos a crear un heatmap más chulo. Para ello necesitamos dos paquetes: gplots y RcolorBrewer
#if (!requireNamespace("BiocManager"))
#    install.packages("BiocManager")
#BiocManager::install(c("gplots","RColorBrewer"))
install.packages("gplots")		
install.packages("RColorBrewer")	

library(gplots)
library(RColorBrewer)

# Hacemos nuestro heatmap
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row")

# Lo guardamos en un archivo PDF
pdf ("GSE5583_DE_heatmap.pdf")
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
col = rev(redblue(256)), scale = "row", labRow=FALSE)
dev.off()
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
col = redgreen(75), scale = "row", labRow=FALSE)

# Guardamos los genes diferencialmente expresados y filtrados en un fichero
write.table(filtered, "GSE5583_DE.txt", sep = "\t",quote = FALSE)
