#ais <- read.table("ais.txt", header = T)
num = ais[,-c(1,2)]
attach(ais)
apply(num, 2, mean)
apply(num, 2, var)
n = dim(num)[2]
korelacje = data.frame()
for (i in 1:n) {
  for (j in 1:n) {
    korelacje[i, j] = cor(num[,i], num[,j], method = "pearson")
  }
}
library(GGally)
library(ggplot2)
print(ggplot(ais, aes(x=Ht, y = Wt)) + geom_point())
beta1 = cor(Ht, Wt, method = "pearson") * sd(Wt) / sd(Ht)
beta0 = mean(Wt) - beta1*mean(Ht)
print(ggplot(ais, aes(x=Ht, y = Wt)) + geom_point() + geom_abline(slope = beta1, intercept = beta0, color = 'red'))
predykcja_Wt = beta0 + beta1 * Ht
r <- Wt - predykcja_Wt
print(qqnorm(r))
n = length(Ht)
sigma_squared = sum(r*r)/(n - 2)
sigma = sqrt(sigma_squared)
X = cbind(1, Ht)
XTX = t(X) %*% X
e_1 = c(0,1)
w = solve(XTX, e_1)
v1 = w[2]
statystyka_t = beta1/(sigma * sqrt(v1))
pv = 2*pt(-statystyka_t, n - 2)
alfa = 0.05
z = qnorm(1-alfa/2)
c = z * sqrt(v1)*sigma
przedzial_ufnosci = c(beta1 - c, beta1 + c)
