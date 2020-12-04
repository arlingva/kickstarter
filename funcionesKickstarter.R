tabla <- function(vector){
  tabla.temp <- data.frame(table(vector))
  p <- 100 * tabla.temp$Freq/sum(tabla.temp$Freq)
  tabla.temp <- cbind(tabla.temp, Porcentaje = round(p,1))
  
  tabla.temp %>% 
    kbl() %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
}

crosstab <- function(renglon, columna, Freq) {
  tabla <- table(renglon, columna)
  if(!Freq){
    tabla <- tabla %>%
      prop.table(margin = 2) 
    tabla <- tabla * 100
    tabla <- round(tabla, 1) 
  }
  return(tabla)
}

descarta.outliers <- function(x, alpha){
  removeNA <- TRUE
  quantiles <- quantile(x, c(alpha/2, 1-alpha/2), na.rm = removeNA)
  x[x<quantiles[1]] <- NA
  x[x>quantiles[2]] <- NA
  x
}
