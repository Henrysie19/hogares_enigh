library(tidyverse)

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
#por lo tanto debemos cambiarla
enigh$ing_cor<-as.numeric(enigh$ing_cor)

#Corremos nuevamente la linea del ingreso_per_capita
enigh<- mutate(enigh, ingreso_per_capita = ing_cor/tot_integ)
#correcto

# realizamos un resumen estadistico haciendole un summary
summary(enigh)

# lo proximo a realizar es, saber por cual sexo se tienen mas ingresos
# para eso lo que haremos es un hacer un group by de la tabla enigh y hacerle un 
# summarise

sexo<-group_by(enigh, sexo_jefe)
summarise(sexo, mean(ing_cor), median(ing_cor))


##Logramos identificar que los hombres tienen mejor salario

###Modelos Lineales

ggplot(data=enigh)+
  geom_point(mapping = aes(x=ingreso_per_capita, y=salud))

#Me genero una grafica fuera de sus limites porque los datos son muy grandes
#para poder graficar con datos mas pequeños intentaremos hacer logaritmos los datos
#crearemos nuevas columnas con los datos en logaritmos, con la funcion mutate

enigh<-mutate(enigh, ln_ipc=log(ingreso_per_capita))
enigh<-mutate(enigh, ln_salud=log(salud))

#nos presenta un error con salud porque no es numerica por lo tanto
#procedemos a convertirla en numerica

enigh$salud <- as.numeric(enigh$salud)

#volvemos a correr el codigo anterior
enigh<-mutate(enigh, ln_salud=log(salud))
#correcto

#realizamos la grafica nuevamente con los datos log
ggplot(data=enigh)+
  geom_point(mapping = aes(x=ln_ipc, y=ln_salud))
#En la grafica algunos datos nos dieron error, debido a que el logaritmo de 0 es error
#De igual fomra se nota cierta relación en las dos variables

#Debemos eliminar todos los datos error/-inf filtrandolos los 0 y elimnandolos

enigh<-filter(enigh, salud!=0)
enigh<-filter(enigh, ing_cor!=0)

#volvemos a graficar
ggplot(data=enigh)+
  geom_point(mapping = aes(x=ln_ipc, y=ln_salud))
#precisamente se quitaron los puntos muy alejados

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
##que haya mas correlación y para eso, ingresaremos la variable de gastos personales

modelos<- lm(enigh$ln_salud ~ enigh$ln_ipc + enigh$personales)
summary(modelos)

Call:
  lm(formula = enigh$ln_salud ~ enigh$ln_ipc + enigh$personales)

Residuals:
  Min      1Q  Median      3Q     Max 
-6.1605 -1.0068  0.0268  0.9983  6.4031 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)      2.377e+00  1.447e-01   16.43   <2e-16 ***
  enigh$ln_ipc     3.836e-01  1.541e-02   24.90   <2e-16 ***
  enigh$personales 6.906e-05  3.351e-06   20.61   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.482 on 15068 degrees of freedom
Multiple R-squared:  0.08752,	Adjusted R-squared:  0.0874 
F-statistic: 722.6 on 2 and 15068 DF,  p-value: < 2.2e-16

##Solo aumentamos muy poco el Rcuadradro/R-square








            
