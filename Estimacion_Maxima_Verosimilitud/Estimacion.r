parametros_reales = runif(2,0,100)

x.obs <- rbeta(10000, parametros_reales[1], parametros_reales[2])

hist(x.obs, prob = T, col = "orange")

f <- function(theta) -sum(log(dbeta(x.obs,theta[1],theta[2])))

parametros_estimados <- optim(c(1,1),f)$par

parametros_reales
parametros_estimados

hist(x.obs, prob = T, col = "orange")
curve(dbeta(x,parametros_reales[1],parametros_reales[2]), add = T, lwd = 2, col = "red")
curve(dbeta(x,parametros_estimados[1],parametros_estimados[2]), add = T, lwd = 2, col = "blue")


