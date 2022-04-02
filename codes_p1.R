f_2.6 <- function(x){ 
  val <- 1-x^2 
  return(val)
}

curve(f_2.6, -1, 1)

6^4/72

f_2.6.1 <- function(x){abs(x)^3}
curve(f_2.6.1, -100, 100)

f3 <- function(x){1-x^2}
curve(f3, -1, 1)
grid()

72/9 + 4 -6^5/45

sum(c(seq(1:5))^2)/5

f_bin = Vectorize(function(x){choose(4,x)*(1/2)^x*(1/2)^(4-x)})
curve(f_bin, 0, 4)
grid()

f_bincum = Vectorize(function(x){
  vec <- c(0:x)
  probs <- f_bin(vec)
  return(sum(probs))
})
curve(f_bincum, 0, 4)
grid()




# punto 1 -----------------------------------------------------------------

factorial(10)
10*9*8*7*6*5*4*3*2*1

factorial(5)



# Punto 2 -----------------------------------------------------------------

f_p2 <- function(x){x^2}
curve(f_p2, -1, 1)
grid()

# Paso 1: definir la función
fy_p2 <- function(y){(1/2)*y^(-1/2)}
# Paso 2: evaluar en la función integrate
integrate(fy_p2, 0, 1)

yfy_p2 <- function(y){(1/2)*y^(1/2)}
integrate(yfy_p2, 0, 1)

y2fy_p2 <- function(y){(1/2)*y^(3/2)}
integrate(y2fy_p2, 0, 1)

1/5-1/9
4/45


# Punto 3 -----------------------------------------------------------------

(15/37)*0.7*0.8
(15/37)*0.7 - 0.227



# Punto 4 -----------------------------------------------------------------

integrate(function(x){(1/4)*exp((x-3)/2)}, -10000, 3)


0.26*0.03+0.38*0.04+0.36*0.01

(0.26*0.03)/0.0266
(0.38*0.04)/0.0266
(0.36*0.01)/0.0266


numbers <- seq(1,100)
dhyper(0, 6, 94, numbers)[dhyper(0, 6, 94, numbers)>0.098&dhyper(0, 6, 94, numbers)<0.12]

min(which(dhyper(0, 6, 94, numbers)<0.1))

min(which(phyper(1, 6, 94, numbers)<0.1))





