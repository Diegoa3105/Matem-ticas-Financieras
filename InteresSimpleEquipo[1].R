#Función para interés simple.

interes_simple <- function(a, r, n){
  VF <- a * (1+r*n)
  return(VF)
#Donde a=valor actual, r=tasa del periodo.
  #r=número de periodos de capitalización de interéses 
  
  
}

interes_simple(1000, 0.05, 3)

interes_simple_1 <- function(a, r, n){
  
  # Validaciones
  if(a <= 0 | r < 0 | n <= 0){
    return("Error: Verifica que a>0, r>=0 y n>0")
  }
  
  # Cálculo con for
  VF <- a
  for(i in 1:n){
    VF <- a * (1 + r * i)
  }
  
  return(VF)
}

interes_simple_1(980, 0.03, 6)
