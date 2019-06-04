
#Ejemplo de un modelo numérico generalizado para un N suf. grande de personas en el modelo

#Tenemos 1000 personas dentro de nuestro modelo
#Sea X la variable aleatoria que representa el valor de una casa
set.seed(0834050280)
X<-rnorm(1000,1E6,1E6/2)

#Sea W la variable aleatoria que represeta la edad de fallecimiento de una persona
X<-rpois(1000,75)

#Sea I la edad de ingreso de una persona al programa (nota, a futuro y con suficiente tiempo
#Se puede calcular como la edad prmedio de permanencia en el programa)
I<-rpois(1000,63)

#Se busca la VA de permanencia como la resta de dichas VA's, para efectos prácticos
#Se debe hacer la resta de las VA's  X-I, sin embargo no debemos colocarlo
#explícitamente (Perm<-X-I), ya que en unos pocos casos podría dar valores negativos
#en su lugar se debe desarrollar la teoría para ver la resta de las distribuciones
#Para este caso es sencillo, pues es resta de Poissones
Perm<-rpois(1000,75-63)

#Buscamos también la distribución del costo promedio Anual_P1. Si el ejercicio se pasa a
#mensual, debemos reescribir la variable Perm en función de los meses, análogaente para semanas, etc.
#Se trabajará con una dist. gamma debido a sus bonitas propiedades (también se puede
#usar una Beta ajustada, pero en realidad se debe obtener de muestras reales)
#Simularemos más escenarios para los distintos planes

Anual_P1<-rgamma(1000,30000,.1)

Anual_P2<-rgamma(1000,20000,.1)

Anual_P3<-rgamma(1000,10000,.1)

#Veamos la gráfica de los tres planes

plot(density(c(Anual_P1,Anual_P2,Anual_P3)))

#####################################
#Siendo este el caso, veamos los gastos que implica esta operación:
Costo<-Perm*Anual_P1
#Veamos un pequeño resumen
summary(Costo)
#De ser este el caso, el costo de las viviendas deberían rondar los 3,6M
#####################################

#Suponemos que la tasa de crecimiento del costo de la vivienda es X% Anual_P1
#Suponemos que el costo de los medicamentos/equipo/salario de quien los atiende, etc crece X% Anual_P1
#Y suponemos que nuestra población aumenta su edad de supervivencia con una tasa X% Anual_P1

#Digamos que se distribuye como una Normal, solo para ejemplificar.
#Estos números pueden ser obtenidos con modelos predictivos, no necesariamente con
#variables aleatorias
#Entonces debemos añadir ponderadores a nuestro modelo:
#Pero cuántos años debemos predecir?, pues la de la persona con mayor permanencia +1
Max_perm<-max(Perm)
Pond_Vivienda<-rnorm(Max_perm,1.11,.1)
Pond_Servicio<-rnorm(Max_perm,1.21,.15)
Pond_Superv<-rnorm(Max_perm,1.05,.04)
#Estos resultados serán combinados en un ponderador final
Pond_Fin<-Pond_Vivienda*Pond_Servicio*Pond_Superv

#Siendo así, ya tenemos nuestro modelo listo

#Primero veamos cuánto costará la persona 1.
Costo1<-sum(Anual_P1[1]*Pond_Fin[1:Perm[1]])

#Repliquemos para nuestras 1000 personas
Costo<-purrr::map_dbl(1:1000,~sum(Anual_P1[.x]*Pond_Fin[1:Perm[.x]]))
Costos<-data.frame(Persona=1:1000,Perm=Perm,Costo=Costo)
#Para sacar el valor presente, debemos dividir entre el Pond_Vivienda por cada año
#Para el primer cliente:
Valor_Presente1<-sum(Anual_P1[1]*(Pond_Fin/Pond_Vivienda)[1:Perm[1]])

VP_Costos<-purrr::map_dbl(Perm,~sum(Anual_P1[.x]*(Pond_Fin/Pond_Vivienda)[1:Perm[.x]]))

Costos<-cbind(Costos,data.frame(Valor_Presente_Costos=VP_Costos))
#Sabemos que el costo de una vivienda es de 1M en promedio
#que los costos de tratamiento superan con creces el valor presente
summary(Costos)

#Entonces ya tenemos lo necesario para trabajar en nestro modelo


#Primero trataremos de cambiar los parámetros de Permanencia y de costo de la vivienda

#Después añadiremos una distribución para el valor de las casas

#Finalmente veremos como calcular un caso particular
