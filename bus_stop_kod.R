remove(list = ls())
####################### Macierz kosztow #############################
setwd('C:\\Users\\lenovo\\Desktop\\projekt\ nmo')
koszty <- read.csv('koszty.csv', sep = ';', dec = ",")
koszty <- koszty[,1:11]
rownames(koszty) <- koszty[,1]
koszty <- koszty[,2:11]
koszty <- as.data.frame(koszty)

Tk <- koszty
colnames(Tk) <- c(1:10)
rownames(Tk) <- c(1:10)
Tk <- as.matrix(Tk)



####################### Zmienne #########################
B <- 3
K = 1:B
# Tk ij - odleglosc podrozy z przystanku i do przystanka j  
N = nrow(Tk)
S = 1:N
C = c(50,75,60)

Y = sample.int(15, 9)


b1 = c(1,4,10)
b2 = c(2,3,5,8, 10)
b3 = c(6,7,9,10)


f = function(b1, b2, b3, D)
{
  n1 = length(b1)
  d1 = 0
  for (i in 1 : (n1-1))
  {
    d1 = d1 + D[b1[i], b1[i+1]] * Y[b1[i]] 
  }
  
  n2 = length(b2)
  d2 = 0
  for (i in 1 : (n2-1))
  {
    d2 = d2 + D[b2[i], b2[i+1]] * Y[b2[i]] 
  }
  
  n3 = length(b3)
  d3 = 0
  for (i in 1 : (n3-1))
  {
    d3 = d3 + D[b3[i], b3[i+1]] * Y[b3[i]] 
  }
  
  d = d1 + d2 + d3
  
  
  return(d)
}

f(b1,b2,b3, Tk)
print('hello')


