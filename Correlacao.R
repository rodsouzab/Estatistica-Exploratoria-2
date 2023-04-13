tempo =      c(5, 9, 13, 7.5, 16, 21)
distancia = c(14, 30, 37, 22, 50, 63)

cor(tempo, distancia)
plot(tempo, distancia)

n = length(tempo)

x = tempo
y = distancia
num = n*sum(x*y) - sum(x)*sum(y)
den1 = sqrt(n*sum(x^2) - sum(x)^2)
den2 = sqrt(n*sum(y^2) - sum(y)^2)

num/(den1*den2)

cor(x,y)





x = cars$speed
y = cars$dist

plot(x,y)
reta = lm(y~x)

a = round(reta$coefficients[1], 2)
b = round(reta$coefficients[2], 2)

abline(a = a, b = b)
text(10,100, paste0('y= ', a, ' + ', b, '*x'))
