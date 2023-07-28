library(tidyverse)
library(readxl)
library(readxlsb)

# Directorio
setwd("C:/Users/ASUS/OneDrive/Documentos/myrepo/hogres_enigh")

#Leer el archivo
enigh<-read_xlsx("hogares_enigh.xlsx")

hogares_lider_hombre<-filter(enigh, sexo_jefe==2)
arrange(hogares_lider_hombre, edad_jefe)
hogares_lider_hombre<-arrange(hogares_lider_hombre, edad_jefe)

#Creamos una nueva columnna llamada ingreso_per_capita
#de la siguiente manera

enigh<- mutate(enigh, ingreso_per_capita = ing_cor/tot_integ)

#nos deuelve el siguiente error:
#Error in `mutate()`:
#ℹ In argument: `ingreso_per_capita = ing_cor/tot_integ`.
#Caused by error in `ing_cor / tot_integ`:
#  ! non-numeric argument to binary operator
#enigh<- mutate(enigh, ingreso_per_capita = ing_cor/tot_integ)

# Para sluciuonar este error debemos identificar cuales columnas no son binarias
# con el siguiente codigo:
X<- lapply(enigh, class)

#lo que nos mostró fue que la columna ing_cor es de tipo "character"
#Asi como la tabla de alimentos y salud
#por lo tanto debemos cambiarlas

enigh$ing_cor<-as.numeric(enigh$ing_cor)
enigh$alimentos<-as.numeric(enigh$alimentos)
enigh$salud<-as.numeric(enigh$salud)
enigh$est_socio<-as.numeric(enigh$est_socio)
enigh$sexo_jefe<-as.factor(enigh$sexo_jefe)
enigh$est_socio<-as.factor(enigh$est_socio)

# realizamos un resumen estadistico de los datos que usaremos 
summary(enigh[c('salud', 'alimentos', 'ingreso_per_capita')])

# lo proximo a realizar es, saber por cual sexo se tienen mas ingresos
# para eso lo que haremos es un hacer un group by de la tabla enigh y hacerle un 
# summarise

sexo<-group_by(enigh, sexo_jefe)
summarise(sexo, mean(ing_cor), median(ing_cor))


##Logramos identificar que los hombres tienen mejor salario

##Graficas
##Iniciamos con los histogramas de los datos que usaremos (salud, alimentos, ipc),
##con esta información podemos visualizar el la forma de nuestros datos y saber
##cual es el centro de nuestros datos, como tambíen saber su distribuccion

##Histograma de Gastos en Salud distribuidos por estrato socioeconomico
ggplot(enigh) + 
  geom_histogram(bins = 50, aes(x = ln_salud, fill = est_socio), color = 'black') + 
  facet_grid(est_socio~., scales = 'free') +
  xlab("EStrato Socioeconomico") + 
  ylab("Frecuencia") + 
  ggtitle("Distribución de Gastos en Salud") +
  theme_minimal()

hist(enigh$ln_salud, main = "Histograma de Edad", 
     xlab = "Edad", ylab = "Frecuencia",
     col = "purple")
##Histograma de Gastos en Alimentos distribuidos por sexo
ggplot(enigh) + 
  geom_histogram(bins = 50, aes(x = ln_alimentos, fill = est_socio), color = 'black') + 
  facet_grid(est_socio~., scales = 'free') +
  xlab("Estrato Socioeconomico") + 
  ylab("Frecuencia") + 
  ggtitle("Distribución de Gastos en Alimentos") +
  theme_minimal()

##Histograma de Ingreso Per Capita distribuidos por estrato socioeconomico
ggplot(enigh) + 
  geom_histogram(bins = 50, aes(x = ln_ipc, fill = sexo_jefe), color = 'black') + 
  facet_grid(sexo_jefe~., scales = 'free') +
  xlab("Estrato Socioeconomico") + 
  ylab("Frecuencia") + 
  ggtitle("Distribución de Ingresos Per Capita") +
  theme_minimal()

#Graficos de correlación lineal

ggplot(data=enigh) +
  geom_point(mapping = aes(x=enigh$ingreso_per_capita, y=enigh$alimentos))

ggplot(data=enigh)+
  geom_point(mapping = aes(x=ingreso_per_capita, y=salud))

#Me genero una grafica fuera de sus limites porque los datos son muy grandes
#para poder graficar con datos mas pequeños intentaremos hacer logaritmos los datos
#crearemos nuevas columnas con los datos en logaritmos, con la funcion mutate

enigh<-mutate(enigh, ln_ipc =log(ingreso_per_capita))
enigh<-mutate(enigh, ln_salud =log(salud))
enigh<-mutate(enigh, ln_alimentos =log(alimentos))

#nos presenta un error con los datos de salud porque no es numerica por lo tanto
#procedemos a convertirla en numerica

enigh$salud <- as.numeric(enigh$salud)

## Graficas de Correlaciones Lineales

# Ingreso Per Capita / Gastos en Alimentos, distribuidos por estratos
ggplot(data=enigh) +
  geom_point(mapping = aes(x=ln_ipc, y=ln_alimentos, color = est_socio)) +
  xlab("Ingreso Per Capita") +
  ylab("Gastos en Alimentos") +
  ggtitle("Gráfico de Correlacion") +
  theme_minimal()

# Ingreso Per Capita / Gastos en Salud, distribuidos por estratos
ggplot(data=enigh) +
  geom_point(mapping = aes(x=ln_ipc, y=ln_salud, color = est_socio)) +
  xlab("Estrato Socioeconomico") +
  ylab("Gastos en Alimentos") +
  ggtitle("Gráfico de Correlacion") +
  theme_minimal()

#En la grafica algunos datos nos dieron error, debido a que el logaritmo de 0 es error
#De igual fomra se nota cierta relación en las dos variables

#Debemos eliminar todos los datos error/-inf filtrandolos los 0 y elimnandolos

enigh<-filter(enigh, salud!=0)
enigh<-filter(enigh, ing_cor!=0)
enigh<-filter(enigh, alimentos!=0)

## Al graficar podemos ver a simple vista que existe de alguna manera 
##una correlación lineal, pero esto no se sabe con exactitud hasta tener 
## resumen de los datos.
##Para esto realizaremos un modelo linear o LinearModel

modelos<- lm(enigh$ln_salud ~ enigh$ln_ipc)
summary(modelos)

Call:
  lm(formula = enigh$ln_salud ~ enigh$ln_ipc)

Residuals:
  Min      1Q  Median      3Q     Max 
-5.4801 -1.0223  0.0217  1.0126  6.3854 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)    1.7291     0.1432   12.07   <2e-16 ***
  enigh$ln_ipc   0.4726     0.0150   31.51   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.502 on 15069 degrees of freedom
Multiple R-squared:  0.06181,	Adjusted R-squared:  0.06174 
F-statistic: 992.7 on 1 and 15069 DF,  p-value: < 2.2e-16

##En el summary nos damos cuenta que el Rcuadrado/R-square es = 0,06 
##Lo que nos dice que no hay mucha correlación

##Ahora intentaremos agregarle otra variable paara intentear que la
##que haya mas correlación y para eso, ingresaremos la variable de alimentos

modelos<- lm(enigh$ln_salud ~ enigh$ln_ipc + enigh$ln_alimentos)
summary(modelos)

Call:
  lm(formula = enigh$ln_salud ~ enigh$ln_ipc + enigh$ln_alimentos)

Residuals:
  Min      1Q  Median      3Q     Max 
-5.3475 -0.9997  0.0120  0.9903  6.3707 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)        -0.58706    0.19037  -3.084  0.00205 ** 
  enigh$ln_ipc        0.41236    0.01525  27.045  < 2e-16 ***
  enigh$ln_alimentos  0.31561    0.01723  18.312  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.485 on 14974 degrees of freedom
Multiple R-squared:  0.08214,	Adjusted R-squared:  0.08202 
F-statistic:   670 on 2 and 14974 DF,  p-value: < 2.2e-16

##Solo aumentamos muy poco el Rcuadradro/R-square agregandole la los datos de alimentos








            

