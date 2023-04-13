####### Teste bilateral
mu0 = 11
sig = 0.8
n = 35
sigxb = sig/sqrt(n)


xb = 11.4

alfa = 0.001

prob = 1 - alfa + alfa/2

ztab = qnorm(prob) #entra probabilidade, sai z

zcalc = (xb - mu0)/sigxb

if(zcalc <= -ztab | zcalc >= ztab){
  print(paste0('Rejeitamos H0 ao nível de ', alfa*100, '%'))
}else{
  print(paste0('Aceitamos H0 ao nível de ', alfa*100, '%'))
}






####### Teste unilateral a esquerda
mu0 = 26
sig = 2.32
n = 10
sigxb = sig/sqrt(n)


xb = 25.3

alfa = 0.05

prob = alfa

ztab = qnorm(prob) #entra probabilidade, sai z

zcalc = (xb - mu0)/sigxb

if(zcalc <= ztab){
  print(paste0('Rejeitamos H0 ao nível de ', alfa*100, '%'))
}else{
  print(paste0('Aceitamos H0 ao nível de ', alfa*100, '%'))
}



####### Teste unilateral a direita
mu0 = 206
sig = 12
n = 30
sigxb = sig/sqrt(n)


xb = 210

alfa = 0.01

prob = 1 - alfa

ztab = qnorm(prob) #entra probabilidade, sai z

zcalc = (xb - mu0)/sigxb


if(zcalc >= ztab){
  print(paste0('Rejeitamos H0 ao nível de ', alfa*100, '%'))
}else{
  print(paste0('Aceitamos H0 ao nível de ', alfa*100, '%'))
}

