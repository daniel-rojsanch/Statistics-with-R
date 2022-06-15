## Distribuciones en R

## Normal

## vamos a graficar la función de densidad de una normal

x <- seq(-4, 4, 0.1)
plot(x, dnorm(x), type = "l", lwd = 2, main = "Normal" )
grid()

## otra alternativa es usar curve
curve(dnorm(x), -4, 4 , lwd = 4, col = "#3095D3")
grid()

## beta

## parametros 
a <- c(4,7)
b <- c(10, 3)
curve(dbeta(x,a[1],a[2]), lwd = 3, col = "#F08803", main = "Densidad de una Beta", ylim = c(0, 4), ylab = "")
curve(dbeta(x,b[1],b[2]), lwd = 3, col = "#32F003", add = T)
legend("topright", col = c("#F08803","#32F003"), title = "Parametros", 
       legend = c("(4,7)", "(7, 4)"), pch = 15, pt.cex = 2, bty = "n")
grid()

## Distribuciones Discretas

## Binomial
x <- 1:50
plot(x, dbinom(x, 50, 1/3), type = "h", lwd = 4, main = "Funcionde Probabilidad \n Binomial(50, 1/3)")
grid()

plot(x, pbinom(x, 50, 1/3), type = "s", lwd = 4, main = "Funcion de Distribución \n Binomial(50, 1/3)")
grid()


