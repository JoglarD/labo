# Definir el número de tiros y de adolescentes
num_tiros <- 100
num_adolescentes <- 100

# Crear una matriz para registrar los resultados de los tiros de cada adolescente
resultados <- matrix(0, nrow = num_adolescentes, ncol = num_tiros)

# Simular los tiros de cada adolescente
for (i in 1:num_adolescentes) {
  for (j in 1:num_tiros) {
    # Lanzar un tiro y registrar si fue acertado o no
    if (runif(1) < 0.5) {
      resultados[i,j] <- 1
    }
  }
}

# Eliminar a los adolescentes que erraron los tiros libres
while (sum(!rowSums(resultados) == num_tiros) > 5) {
  resultados[!rowSums(resultados) == num_tiros,] <- 0
}

# Registra los resultados de los 5 sobrevivientes
resultados_sobrevivientes <- resultados[!rowSums(resultados) == 0,]

# Imprime los resultados de los 5 sobrevivientes
print(resultados_sobrevivientes)
