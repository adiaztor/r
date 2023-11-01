#1. Carga los datos y exáminalos en R. Emplea las funciones head(),
#summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos?
#Introduzco los datos del archivo y se meten las variables
> datos <- read.table("datos-trabajoR.txt",header=TRUE)
> head(datos)
#Hay 2 variables y 5 tratamientos


#2. Haz un boxplot para nuestros datos. Uno para cada variable.
#Colorea a Variable 1 y a Variable 2 de forma diferente (guarda esos
#colores para las siguientes gráficas)
#Para distinguir las variables, la variable 1 será de color azul y la 2 de 
#color rojo
#Se crea el boxplot con las columnas que nos interesan:
> boxplot(Variable1~Tratamiento, data=datos, col=c("blue"), main="boxplot 1-Trabajo.R")
> boxplot(Variable2~Tratamiento, data=datos, col=c("red"), main="boxplot 1-Trabajo.R"


#3. Haz un gráfico de dispersión con las dos variables. Cada tratamiento 
#debe de ir de un color distinto. ¡Como en la siguiente imagen!
> plot(datos$Variable1, datos$Variable2, col = datos$Tratamiento, 
+      pch = 19, xlab = "Variable1", ylab = "Variable2",
+      main = "Gráfico de Dispersión por Tratamiento")


#4. Ponle leyenda al gráfico del apartado anterior. En el margen inferior
#derecho. Pista: investiga sobre legend()
> legend("bottomright", legend = unique(datos$Tratamiento), 
  col = unique(datos$Tratamiento), pch = 19, title= "Tratamiento"


#5. Haz un histograma para cada variable. Recuerda mantener los
#colores.
> hist(datos$Variable1, col = "blue", main = "Histograma Variable1", xlab = "Variable1", ylab = "Frecuencia")
> hist(datos$Variable2, col = "red", main = "Histograma Variable2", xlab = "Variable2", ylab = "Frecuencia")


#6. Haz un factor en la columna tratamiento y guárdalo en una
#variable. Pista: factor(factor$Tratamiento)

#Creas el factor en función de la columna tratamiento:
> FactorTratamiento = factor (datos$Tratamiento)

#Ahora se comprueba
> FactorTratamiento
# [1] 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4
[36] 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5
Levels: 1 2 3 4 5


#7. Calcula la media y la desviación estándar para cada tratamiento. Recomendación: es
#más fácil si usas aggregate() o tapply().
#• aggregate(Variable~factor,datos,función)
#• tapply(datos$Variable,factor,función)

#Para la variable 1
#La media es: 4.00;4,90;8,77;50,80;34,90
> aggregate(Variable1~FactorTratamiento,datos,mean)
FactorTratamiento Variable1
1                 1      4.00
2                 2      4.90
3                 3      8.77
4                 4     50.80
5                 5     34.90

#La desviación estandar es: 1.290133; 3.754997;3.857475;11.113555;8.633912
> aggregate(Variable1~FactorTratamiento,datos,sd)
 FactorTratamiento Variable1
1                 1  1.290133
2                 2  3.754997
3                 3  3.857475
4                 4 11.113555
5                 5  8.633912


#Para la variable 2:
#La media es: 0.510; 1.300; 5.310; 8.730; 9.018
> aggregate(Variable2~FactorTratamiento,datos,mean)
 FactorTratamiento Variable2
1                 1     0.510
2                 2     1.300
3                 3     5.310
4                 4     8.730
5                 5     9.018

#La desviación estandar es:0.2884826;0.4189935;1.3568346;1.3333750;1.2146769
> aggregate(Variable2~FactorTratamiento,datos,sd)
  FactorTratamiento Variable2
1                 1 0.2884826
2                 2 0.4189935
3                 3 1.3568346
4                 4 1.3333750
5                 5 1.2146769


#8. Averigua cuántos elementos tiene cada tratamiento. Recomendación: es más fácil si
#usas table() con el factor

#Para saber cuantos elementos:
> table(FactorTratamiento)

#Se observa que cada tratamiento tiene 10 elementos 


#9. Extrae los datos para el tratamiento 1 y el tratamiento 4 y guárdalos cada uno en una
#variable diferente.

#Tratamiento 1:
> Tratamiento1 = datos [1:10, ]
#Se comprueba con:
> Tratamiento1

#Ahora tratamiento 4:
> Tratamiento4 = datos [31:40, ]
#Se comprueba con:
> Tratamiento4


#10. Nuestra hipótesis nula es que las medias de tratamiento 1 y tratamiento 4 para la
#Variable 1 son iguales. ¿Puedes comprobarlo? Para ello, necesitarás comprobar
#primero si los datos se distribuyen de forma normal. En función del resultado de la
#prueba de normalidad, ¿qué test usarías? ** En general, asumimos que las muestras
#son independientes, pero ¿son sus varianzas iguales? Actúa de acuerdo a tus
#resultados.

#el pi value y el intervalo de confianza son pequeños

#comprabamos si las columnas de tratamiento y la variable se distribuyen 
#de manera normal. Para ello, la columna de tratamiento:
> distribucionnormal <- shapiro.test(datos$Tratamiento)

#Se observa el p value
> p_valor <- distribucionnormal$p.value
  if (p_valor < 0.05) {cat("Los datos no siguen una distribución normal (p-valor =", p_valor, ")\n")
} 
  else {cat("Los datos siguen una distribución normal (p-valor =", p_valor, ")\n")
}

#Se ve que los datos no siguen una distribución normal debido a que 
#el p value es p=0.000204409

#Para la columna de variable 1
> distribucionnormal2 <- shapiro.test(datos$Variable1)

#Compruebo mostrando el p value
> p_valor <- distribucionnormal2$p.value
  if (p_valor < 0.05) {
  cat("Los datos no siguen una distribución normal (p-valor =", p_valor, ")\n")
} else {
  cat("Los datos siguen una distribución normal (p-valor =", p_valor, ")\n")
}

#Se observa de nuevo que los datos no siguen una distribución normal, ya 
#que el p value es p=9.522027e-06

#Se hace la prueba de Mann-Whitney-Wilcoxon, porque los datos no siguen una
#distribución normal; así que por ello, se hace las medias en función de la
#variable1, y se guarda como variable nueva.
> MediaT1 = aggregate(Variable1~Tratamiento,Tratamiento1,mean)

#Se comprueba:
> print(MediaT1)
  Tratamiento Variable1
1           1         4

#La media del tratamiento 1 en función de la variable 1 es 4.

#Para el tratamiento 4 se siigue el mismo procedimiento:
> MediaT4 = aggregate(Variable1~Tratamiento,Tratamiento4,mean)

#Se comprueba:
> print(MediaT4)
  Tratamiento Variable1
1           4      50.8

#La media del tratamiento 4 en función de la variable 1 es 50.8

#Para comparar ambas se hace:
> MT1vsMT4 <- wilcox.test(MediaT1$Variable1, MediaT4$Variable)

# Y ahora se comprueba si son iguales y se hace un muestreo del p-value
> p_valor <- MT1vsMT4$p.value
 if (p_valor < 0.05) {
  cat("Las medias de las dos variables son diferentes (p-valor =", p_valor, ")\n")
} else {
  cat("No hay evidencia para afirmar que las medias de las dos variables son diferentes (p-valor =", p_valor, ")\n")
}

#Como el p.value es 1, no hay evidencia para decir que la media del tto 4 y la
#media del tto 1 para la variable 1 son diferentes.Según este resultado
#serían iguales

#Si las muestras son independientes, hay que calcular la varianza para el 
#tto 1 y el tto 4, sabiendo que no seguimos una distribución normal

#Para la varianza del tratamiento 1
> VarianzaT1 = aggregate(Variable1~Tratamiento,Tratamiento1,var)
> VarianzaT1
  Tratamiento Variable1
1           1  1.664444
#La varianza para el tratamiento 1 en función de la variable 1 es 1.66

#Para la varianza del tratamiento 4
> VarianzaT4 = aggregate(Variable1~Tratamiento,Tratamiento4,var)
> VarianzaT4
  Tratamiento Variable1
1           4  123.5111
#La varianza para el tratamiento 4 es 123.51

#Se comparan ambas variables
> VT1vsVT4 <- wilcox.test(VarianzaT1$Variable1, VarianzaT4$Variable1)

#Muestro el valor de p y compruebo si son iguales
> p_valor <- VT1vsVT4$p.value
if (p_valor < 0.05) {
  cat("Las varianzas de las dos variables son diferentes (p-valor =", p_valor, ")\n")
} else {
  cat("No hay evidencia para afirmar que las varianzas de las dos variables son diferentes (p-valor =", p_valor, ")\n")
}

#Como el p.value es 1, aunque pensase que las varianzas eran diferentes, 
#no hay evidencias para afirmarlo estadísticamente. Por ello, según 
#el resultado serían iguales.


