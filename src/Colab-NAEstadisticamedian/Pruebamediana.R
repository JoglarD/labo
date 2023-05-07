# Experimentos Colaborativos Default
# Workflow  Catastrophe Analysis

#Necesita para correr en Google Cloud
#  32 GB de memoria RAM
#   8 vCPU


#limpio la memoria
rm( list= ls(all.names= TRUE) )  #remove all objects
gc( full= TRUE )                 #garbage collection
library(zoo)
require("data.table")
require("yaml")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "PRUEBA"
PARAM$dataset  <- "C:\\Users\\JOGLARD\\Documents\\AUSTRAL MCD\\LabImp1\\datasets\\dataset_pequeno.csv"

# FIN Parametros del script


#------------------------------------------------------------------------------
CorregirCampoMes  <- function( pcampo, pmeses ){
  
  # Filtrar los datos por cliente y ciertas fechas
  dt <- subset(dataset, foto_mes == pmeses)
  
  # Ordenar los datos por fecha
  dt <- dt[order(foto_mes)]
  
  # Definir la función que se aplicará a cada subconjunto de datos
  fill_na_roll <- function(x) {
    z <- zoo(x$pcampo, order.by = x$foto_mes)
    z_filled <- rollapply(z, width = 7, FUN = function(y) median(y, na.rm = TRUE), align = "center")
    x$pcampo[is.na(x$pcampo)] <- z_filled[is.na(x$pcampo)]
    return(x)
  }
  
  # Utilizar 'by' para aplicar 'fill_na_roll' a cada subconjunto de datos
  dt_filled <- dt[, fill_na_roll(.SD), by = numero_de_cliente]
  
  
  # Convertir los datos de nuevo a un objeto 'data.table'
  dataset <- data.table(numero_de_cliente = dt_filled$numero_de_cliente, foto_mes = dt_filled$foto_mes, pcampo = dt_filled$pcampo)
}
#------------------------------------------------------------------------------

Corregir_MachineLearning  <- function( dataset )
{
  gc()
  #acomodo los errores del dataset

  dataset[ foto_mes==201905,  mrentabilidad     := NA ]
 
  
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
PARAM$stat$time_start  <- format(Sys.time(), "%Y%m%d %H%M%S")


#cargo el dataset
dataset  <- fread( PARAM$dataset )

#tmobile_app se daño a partir de 202010
dataset[  , tmobile_app := NULL ]



#corrijo los  < foto_mes, campo >  que fueron pisados con cero

#Primero aplico NA
Corregir_MachineLearning( dataset )

#Segundo aplico la Mediana
Corregir_EstadisticaClasica( dataset )

