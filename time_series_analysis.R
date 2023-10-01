#Importacion librerias
library(dynlm)
library(astsa)

#Importacion de datasets de atsa
data("soi")
data("rec")

#Obtencion estadisticas principales
summary(rec)
summary(rec)

#Grafica de las series temporales utilizadas
par(mfrow = c(2,1))
tsplot(soi, col=4, ylab="", main="Indice de Oscilacion del Sur (SOI)",xlab="Tiempo", lwd=2)
tsplot(rec, col=4, ylab="", main="Reclutamiento de nuevos peces",xlab="Tiempo", lwd=2) 
#Descomposicion de las series temporales
plot(decompose(soi))
title(main = "Descomposición Indice de Oscilacion del Sur (SOI)")

plot(decompose(rec))
title(main = "Descomposición del Reclutamiento")

#Analisis de la funcion de autocorrelacion (ACF)
#y analisis de la funcion de correlacion cruzada (CCF)
par(mfrow=c(4,1)) 
acf(soi, 48)
title(main="Indice de Oscilacion del Sur (SOI)")  

acf(rec, 48)
title(main="Reclutamiento de nuevos peces") 

ccf(soi, rec, 48, ylab="CCF")
title(main="SOI vs Recrutamiento")  

#Analisis de funcion de autocorrelacion parcial (PACF) 
par(mfrow=c(3,1)) 
pacf(soi, 48)
title(main="Indice de Oscilacion del Sur (SOI)")  

pacf(rec, 48)
title(main="Reclutamiento de nuevos peces") 


#Regresion lineal entre reclutamiento 
#y SOI retardado en 6 meses 
soi_vector <- as.vector(soi)
lag_soi <- lag(soi_vector, 6)
summary(fit2 <- dynlm(rec~ L(soi,6)))
#Multiple R-squared:  0.3629 
#p-value: < 2.2e-16

#Grafico de datos Reclutamiento y regresion lineal del modelo
plot(lag_soi, fish$rec, main="Reclutamiento vs el Indice IOS (retardado en 6 meses)", xlab="SOI L6", ylab="Reclutamiento")

abline(fit2, col="red")
legend("bottomleft", legend=c("Datos", "Regresión"), col=c("black", "red"), lty=1, lwd=c(1, 2))


#Suavizado por Kernel (promedio)
plot(soi)
lines(ksmooth(time(soi), soi, "normal", bandwidth=1), lwd=2, col=4)
par(fig = c(.65, 1, .65, 1), new = TRUE) # the insert
gauss = function(x) { 1/sqrt(2*pi) * exp(-(x^2)/2) }
x = seq(from = -3, to = 3, by = 0.001)
plot(x, gauss(x), type ="l", ylim=c(-.02,.45), xaxt='n', yaxt='n', ann=FALSE)


#Suavizado por LOWESS (regresion local)
#similar a regresion KNN
plot(soi)
lines(lowess(soi, f=.05), lwd=2, col=4) # ciclos de El Niño
lines(lowess(soi), lty=2, lwd=2, col=2) # promedio de ciclos


# Grafico de suavizado de datos por LOWESS
plot(soi,ylab="SOI", xlab="Tiempo")
lines(lowess(soi, f = 0.05), lwd = 2, col = "blue", legend.text = "Ciclos de El Niño")
lines(lowess(soi), lty = 2, lwd = 2, col = "red", legend.text = "Suavizado de Datos (ciclos)")
legend("bottomleft", legend = c("Datos del Indice de Oscilacion del Sur (SOI)", "Suavizado de Datos (ciclos de El Niño)", "Promedio del Suavizado (promedio ciclos)"), 
       col = c("black", "blue", "red"), lty = c(1, 1, 2), lwd = c(1, 2, 2))


# Prueba de estacionariedad ACF para serie de Reclutamiento
adf_result <- adf.test(rec)
print(adf_result)
#p-value = 0.01 (estacionaria)

# Prueba del p y q optimo para serie de Reclutamiento
resultados_aic <- matrix(NA, nrow = 5, ncol = 5) # Matriz 5x5
for (p in 1:5) {
  for (q in 1:5) {
    modelo_arima <- arima(rec, order = c(p, 0, q))
    valor_aic <- AIC(modelo_arima)
    resultados_aic[p, q] <- valor_aic
  }
}
min_aic <- min(resultados_aic)
fila_col_min_aic <- which(resultados_aic == min_aic, arr.ind = TRUE)
cat("La combinación con el menor AIC es p =", fila_col_min_aic[1], "y q =", fila_col_min_aic[2], "\n")
cat("El valor del AIC mínimo es:", min_aic, "\n")

# Modelo ARIMA para serie de Reclutamiento
modelo_arima <- arima(rec, order = c(1 ,0, 3))
pronostico <- forecast(modelo_arima, h = 100)

plot(rec, main = "Predicción para serie de Reclutamiento", ylab = "Reclutamiento", xlab = "Tiempo", type = "l", col = "blue",lwd=1.7)

lines(pronostico$mean, col = "red",lwd=1.7) 

legend("topleft", legend = c("Serie original", "Predicción"), col = c("blue", "red"), lty = 1,lwd=1.7)


# Prueba de estacionariedad ACF para serie de Indice SOI
adf_result <- adf.test(soi)
print(adf_result)
#p-value = 0.01 (estacionaria)

# Modelo SARIMA para serie de Indice SOI
modelo_arima <- arima(soi, order = c(1, 0, 1), seasonal = list(order = c(1, 0, 1), period = 12))

pronostico <- forecast(modelo_arima, h = 100)

plot(soi, main = "Predicción para serie del Indice de Oscilacion del Sur (SOI)", ylab = "SOI", xlab = "Tiempo", type = "l", col = "blue",lwd=1.7)

lines(pronostico$mean, col = "red",lwd=1.7) 

legend("topleft", legend = c("Serie original", "Predicción"), col = c("blue", "red"), lty = 1,lwd=1.7)










