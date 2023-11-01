##############################################################################

	#TRABAJO R#		#Alejandro Ruiz Marin#		#NP:133725#

##############################################################################
##1. Carga los datos y exáminalos en R. Emplea las funciones head(),
#summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos
#tratamientos?
# Hay 2 variables y hay 5 tratamientos

datos = read.table("datos-trabajoR.txt", header = TRUE)
# Creamos una variable que contiene los datos de nuestro archivo 
# read.table() Para cargar los datos desde el archivo 
# header = TRUE sirve indicar en la primera fila estan los nombres de las columnas
print(datos) 
# Para mostrar los datos  
head(datos)
# Para ver las primeras filas 
summary(datos)
# Hace un resumen de cada columna 
dim(datos)
# Para medir las dimensiones 
str(datos) 
# Para explorar la estrucctura de los datos

##############################################################################
##2. Haz un boxplot para nuestros datos. Uno para cada variable.
#Colorea a Variable 1 y a Variable 2 de forma diferente (guarda esos
#colores para las siguientes gráficas)

# Para crear el boxplot de la Variable 1 de color rojo
boxplot(Variable1~Tratamiento, data = datos, col = "red", main = "Boxplot-Variable 1")

# Para crear el boxplot de la Variable 2 de color azul
boxplot(Variable2~Tratamiento, data = datos, col = "blue", main = "Boxplot-Variable 2")	

##############################################################################
##3. Haz un gráfico de dispersión con las dos variables. Cada
#tratamiento debe de ir de un color distinto. ¡Como en la siguiente imagen! 

# Para crear el grafico de dispersión

plot(x = datos$Variable1, y = datos$Variable2, 
     xlab = "Variable 1", ylab = "Variable 2",
     col = datos$Tratamiento, cex = 1, 
     main = "Gráfico de Dispersión")  

# xlab para el nombre del eje x 
# ylab para el nombre del eje y 
# cex es para el tamaño de los circulos

##############################################################################
##4. Ponle leyenda al gráfico del apartado anterior. En el margen inferior
#derecho. 

legend("bottomright", legend = c("Tratamiento 1", "Tratamiento 2", "Tratamiento 3", "Tratamiento 4", "Tratamiento 5"), fill = c("black", "red", "green", "yellow", "blue"), title = "Tratamientos", border = "black", cex = 1)

# fill es para crear rectanguos con colores de cada tratamiento
# border es para el color del borde de los rectangulos 
# cex es para el tamaño 


##############################################################################
##5. Haz un histograma para cada variable. Recuerda mantener los
#colores.

hist(datos$Variable1, col= "red" ,main=" Histograma Variable 1",
	xlab = "Variable 1")
hist(datos$Variable2, col= "blue" ,main=" Histograma Variable 2",
	xlab = "Variable 2")

##############################################################################
##6. Haz un factor en la columna tratamiento y guárdalo en una
#variable. Pista: factor(factor$Tratamiento)

factor_Tto = factor(datos$Tratamiento)
factor_Tto
	

##############################################################################
##7. Calcula la media y la desviación estándar para cada tratamiento. Recomendación: es
#más fácil si usas aggregate() o tapply().


			##Primero la Media##

Variable_total_medias = aggregate(cbind(Variable1, Variable2)~
Tratamiento, data = datos, mean)
# La media de la Variable1 y Variable2 para cada Tratamiento

Variable_total_medias
# Para verlo
	
#Para modificar los nombres de las columnas de datos
colnames(Variable_total_medias) = c("Tratamiento", "Variable1_media", 
"Variable2_media")

Variable_total_medias 
# Para ver si han cambiado

#Para calcular el resultado final usamos el resultado de las variables entre dos
resultado_final = (Variable_total_medias$Variable1_media 
+ Variable_total_medias$Variable2_media) / 2

resultado_final 
# Para verlo

			##Desviación estandar##

# Para calcular la desviación estándar de Variable1 y Variable2 agrupadas por Tratamiento
Variable_total_Desviacion_estandar = aggregate(cbind(Variable1, Variable2) ~ Tratamiento, data = datos, FUN = sd)

Variable_total_Desviacion_estandar	
# Para verlo

# Para modificar los nombres de las columnas de datos
colnames(Variable_total_Desviacion_estandar) = c("Tratamiento", "Desviacion_estandar1", "Desviacion_estandar2")
	
Variable_total_Desviacion_estandar
# Para ver si han cambiado los nombres

# Para calcular la desviación estándar total
Total_Desviacion_estandar = sqrt(Variable_total_Desviacion_estandar$Desviacion_estandar1^2 + Variable_total_Desviacion_estandar$Desviacion_estandar2^2)

# Para calcular la raiz cuadrada sqrt ()
# ^2 para elevar al cuadrado 

# Para ver el total 
Total_Desviacion_estandar


##############################################################################
##8. Averigua cuántos elementos tiene cada tratamiento. Recomendación: es más fácil si
usas table() con el factor
# Tiene 10 cada una

#Para crear tablas de frecuencia
table(factor_Tto)

##############################################################################
##9. Extrae los datos para el tratamiento 1 y el tratamiento 4 y guárdalos cada uno en una
#variable diferente.

# Crear una variable y guardar los datos extraidos del Tratamiento 1 

Tto1 = datos[datos$Tratamiento ==1,]	
Tto1

# Lo mismo para el tratamiento 4	

Tto4 = datos[datos$Tratamiento ==4,]
Tto4
	

##############################################################################
##10. Nuestra hipótesis nula es que las medias de tratamiento 1 y tratamiento 4 para la
#Variable 1 son iguales. ¿Puedes comprobarlo? Para ello, necesitarás comprobar
#primero si los datos se distribuyen de forma normal. En función del resultado de la
#prueba de normalidad, ¿qué test usarías? ** En general, asumimos que las muestras
#son independientes, pero ¿son sus varianzas iguales? Actúa de acuerdo a tus
#resultados.

##Usamos la función shapiro.test para evaluar si los datos siguen una distribución
#normal. Dado que ambos valores de p (p-value)son mayores que 0.05, podemos 
#concluir que los datos son de una distribución normal, es decir aceptamos 
#la hipótesis nula de que los datos se distribuyen de manera normal.
#Por lo tanto, es apropiado utilizar una prueba t para evaluar nuestra hipótesis.

shapiro.test(Tto1$Variable1)
shapiro.test(Tto4$Variable1)

##Usamos la función `var.test()` para comparar las varianzas. Dado que el valor
#de p (p-value) obtenido es menor que 0.05, podemos concluir que nuestras
#varianzas no son iguales. Hacemos una prueba t de dos muestras asumiendo
#varianzas desiguales. En otras palabras, rechazamos la hipótesis nula de 
#que las varianzas son iguales y aceptamos la hipótesis alternativa de que 
#las varianzas son distintas, por tanto aceptamos la hipotesis alternativa 

var.test(Tto1$Variable1, Tto4$Variable1)
t.test(Tto1$Variable1, Tto4$Variable1, var.equal=FALSE, paried=TRUE)
	

