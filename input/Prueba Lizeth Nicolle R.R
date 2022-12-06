
#  Prueba final Lizeth Flores ------------------------------


# 1. Instalación de paquetes ----------------------------------------------

install.packages("pacman")

pacman::p_load(tidyverse, #Universo de paquetes
               sjmisc, #Para explorar datos
               dplyr, #Para manipular datos
               haven, #cargar y exportar bases de datos en formatos .sav y .dta
               car) #Para recodificar manteniendo las etiquetas



# 2. Importación de datos  -----------------------------------------------------

## 2.1 De la encuesta Bienestar Social (2021) desde url en formato.dta

EBS_2021 <- read_dta(url("https://github.com/learn-R/examen-LizFlores182/raw/main/input/DATA/EBS_2021.dta"))

# 3. Exploración de los datos -------------------------------------------------------

# Primero, exploramos la base de datos revisando los nombres de las variables que la componen

View(EBS_2021) # se usa para visualizar la base de datos
names(EBS_2021) # entrega los nombres de las variables que componen el dataset
head(EBS_2021) # muestra las primeras filas presentes en el marco de datos

# Luego, revisamos cómo se distribuyen las variables que son de interés para la investigación

frq(EBS_2021$c2_3) 
frq(EBS_2021$indigena) 
frq(EBS_2021$pobreza) 
frq(EBS_2021$sexo) 
frq(EBS_2021$inmigrante) 



# 4. Selección de variables  -------------------------------------------

## Seleccionamos las siguientes variables:
#- c2_3: Tiempo en Trabajo remunerado u ocupación
#- pobreza: Nivel de pobreza de los entrevistados
#- indigena: Etnia de los entrevistados
#- sexo: Sexo de los entrevistados
#- inmigrante:Procedencia del entrevistado


## 4.1 Encuesta de Bienestar Social 2021

EBS_2021 <- select (EBS_2021, tiempo_de_trabajo = c2_3, pobreza,
                   etnia = indigena, sexo, procedencia = inmigrante, zona, fexp) 
              
# 5. Transformación de las variables ------------------------------------------------


## 5.1 Recodificación y codificación como NA

#Primero, recodificamos las variables y codificamos los casos perdidos como NA

EBS_2021 <- EBS_2021 %>% 
  mutate_at(vars(tiempo_de_trabajo, pobreza, etnia, sexo, procedencia), funs(as.numeric(.))) %>% 
  filter(zona == 1) %>%
  mutate(tiempo_de_trabajo = car::recode(.$tiempo_de_trabajo, recodes = c("1='Dedicaría menos tiempo'; c(2)='Dedicaría el mismo tiempo que dedico ahora'; c(3)='Dedicaría más tiempo'"), as.factor = T, 
                                    levels = c('Dedicaría menos tiempo', 'Dedicaría el mismo tiempo que dedico ahora', 'Dedicaría más tiempo')),
          sexo = car::recode(.$sexo, recodes = c("1 = 'Hombre'; 2 = 'Mujer'"), as.factor = T,  levels = c('Hombre', 'Mujer')),
          etnia = car::recode(.$etnia, recodes = c("1='No pertenece a ningún pueblo indígena'; c(2)='Pertenece a pueblo indígena'; c(3)= 'NA'"), as.factor = T, 
                                         levels = c('No pertenece a ningún pueblo indígena', 'Pertenece a pueblo indígena')), 
         pobreza = car::recode(.$pobreza, recodes = c("1='Pobres extremos'; c(2)='Pobres no extremos'; c(3)= 'No pobres'"), as.factor = T, 
                                         levels = c('Pobres extremos', 'Pobres no extremos', 'no pobres')), 
         procedencia = car::recode(.$procedencia, recodes = c("1='No inmigrante'; c(2)='Inmigrante'; c(3)='No sabe'"), as.factor = T, 
                                         levels = c('No inmigrante', 'Inmigrante', 'No sabe'))) %>%  
  mutate_at (vars(tiempo_de_trabajo, pobreza, etnia, sexo, procedencia), funs(forcats::as_factor(.)))#Transformo las variables en un factor para que mi modelo se estime de manera correcta, 




#conservando la etiqueta de la variable y poder así saber la cat. de referencia.

# 6. Visualización el set de datos -------------------------------------------


head(EBS_2021)#visualizamos el único set de datos EBS_2021 para revisar que todo esté en orden antes de guardar y exportar


# 7. Guardar y exportar los datos ----------------------------------------

saveRDS(EBS_2021, file = "output/datos_proc.rds") #Guardamos este único set de datos en datos_proc.rds con los datos procesados



