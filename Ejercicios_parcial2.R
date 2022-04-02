
# Ejercicios clase 6 ------------------------------------------------------

# 3.2 (Casella & Berger)
numbers <- seq(1,100)

# a)
min(which(dhyper(0, 6, 94, numbers)<0.1))
# b)
min(which(phyper(1, 6, 94, numbers)<0.1))

# 3.5

pbinom(84, 100, 0.8, lower.tail = F)

1-pbinom(84, 100, 0.8)

# 3.6
# a)
pbinom(99,2000,0.01)
sum(dbinom(seq(1,99),2000,0.01))
# b)
ppois(99,20)
sum(dpois(c(1:99),20))

# 3 ----
# a)
pbinom(6,15,0.25) - pbinom(2,15,0.25)
sum(dbinom(c(3:6),15,0.25))
# b)
pbinom(3,15,0.25) 
sum(dbinom(c(0:3),15,0.25))
# c)
pbinom(5,15,0.25,lower.tail = F)
sum(dbinom(c(6:15),15,0.25))

# 4 ----
phyper(2,10,40,5)
sum(dhyper(c(0:2),10,40,5))


# 5 -----
7*(5^6)/(6^8)
dnbinom(6,2,1/6) # Recordar que cuentalos fallos




# Ejercicios clase 7 ------------------------------------------------------
# Casella and berger 3.7

obj_func <- function(lambda){abs(0.01 - exp(-lambda)*(1+lambda))}

optim(1,obj_func)

# 2 --
ppois(5, 5, lower.tail = FALSE)
1 - ppois(5,5)
1- sum(dpois(c(0:5),5))

exp(-5)
dpois(0,5)

# 3 ---
ppois(3,6)
sum(dpois(c(0:3),6))

ppois(8,6) - ppois(5,6)
sum(dpois(c(6:8),6))

# 4 ----
dpois(0,7.5)

# 5 -----
ppois(10,5,lower.tail = FALSE)
1 - sum(dpois(c(0:10),5))

ppois(20,15, lower.tail = FALSE)

# 6 ------
ppois(16,20)

pi*0.1^2*640

# 7 -------
dpois(10,10)*dbinom(0,10,0.5)
(exp(-10)*10^10)/factorial(10) *0.5^10


# Codigo clase 8 ----------------------------------------------------------

library(extraDistr)
# Lo traemos por la uniforme discreta
?ddunif()
ddunif(5,1,15)

# con p(x) = 1/100 1 < x < 100
# p(2), p(10), p(100)
ddunif(2,1,100)
ddunif(10,1,100)
ddunif(100,1,100)

# p(x<=80), p(x>90), p(50<x<=70)
pdunif(80,1,100)
pdunif(90,1,100,lower.tail = FALSE) # Mayor estricto
pdunif(70,1,100) - pdunif(50,1,100)
sum(ddunif(c(51:70),1,100))


# P(X<=c) = 0.94, P(X<=d) = 0.5 P(X > e) = 10
qdunif(0.94,1,100,lower.tail=TRUE)
qdunif(0.5,1,100,lower.tail=TRUE)
qdunif(0.1,1,100,lower.tail=FALSE)

# Simular datos y crear histograma
valores <- rdunif(10000,1,100)
hist(valores, col='gray',breaks=200)

?distributions


# Actividad propuesta -----------------------------------------------------

# BINOMIAL
# n = 50, p = 0.3

#a. 
dbinom(3, 50, 0.3)
dbinom(10, 50, 0.3)
dbinom(30, 50, 0.3)

# b.
pbinom(18,50,0.3)
pbinom(15.3,50,0.3,lower.tail=FALSE)
pbinom(7.8,50,0.3)-pbinom(5,50,0.3)

# c. 
qbinom(0.14,50,0.3)
qbinom(0.55, 50, 0.3)
qbinom(0.5, 50, 0.3, lower.tail = FALSE)

# d.
binom_vals <- rbinom(10000, 50, 0.3)
hist(binom_vals,col='blue', breaks=100)


# HIPERGEOMETRICA
# N = 50, k = 10, n = 7
# m = 10, n = 40, k = 7 # en R

#a. 
dhyper(3, 10, 40, 7)
dhyper(10, 10, 40, 7)
dhyper(30, 10, 40, 7)

# b.
phyper(18,10, 40, 7)
phyper(15.3,10, 40, 7,lower.tail=FALSE)
phyper(7.8,10,40,7)-phyper(5,10,40,7)

# c. 
qhyper(0.14,10, 40, 7)
qhyper(0.55, 10, 40, 7)
qhyper(0.5, 10, 40, 7, lower.tail = FALSE)

# d.
hyper_vals <- rhyper(10000, 10, 40, 7)
hist(hyper_vals,col='blue', breaks=100)



# BINOMIAL NEGATIVA
# r = 2, p = 0.3
# size = 2, prob = 0.3

# a
dnbinom(1,2,0.3)
dnbinom(8,2,0.3)
dnbinom(28,2,0.3)

# b
pnbinom(16,2,0.3)
pnbinom(13.3,2,0.3,lower.tail = FALSE)
pnbinom(5.8,2,0.3)-pnbinom(3,2,0.3)

# c
qnbinom(0.14,2,0.3)
qnbinom(0.55,2,0.3)
qnbinom(0.5,2,0.3,lower.tail = FALSE)


# d.
nbinom_vals = rnbinom(10000,2,0.3)
hist(nbinom_vals,col='blue',breaks=100)

# POISSON
# lambda = 3

#a. 
dpois(3,3)
dpois(10,3)
dpois(30,3)

# b.
ppois(18,3)
ppois(15.3,3,lower.tail=FALSE)
ppois(7.8,3)-ppois(5,3)

# c. 
qpois(0.14,3)
qpois(0.55,3)
qpois(0.5,3, lower.tail = FALSE)

# d.
pois_vals <- rpois(10000,3)
hist(pois_vals,col='blue', breaks=100)

# GEOMETRICA
# prob = 0.4 

#a. 
dgeom(3,0.4)
dgeom(10,0.4)
dgeom(30,0.4)

# b.
pgeom(18,0.4)
pgeom(15.3,0.4,lower.tail=FALSE)
pgeom(7.8,0.4)-pgeom(5,0.4)

# c. 
qgeom(0.14,0.4)
qgeom(0.55,0.4)
qgeom(0.5,0.4,lower.tail = FALSE)

# d.
geom_vals <- rgeom(10000,0.4)
hist(geom_vals,col='blue', breaks=100)








