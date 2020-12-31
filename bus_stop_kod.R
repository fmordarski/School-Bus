remove(list = ls())
####################### Macierz kosztow #############################
# setwd('C:\\Users\\lenovo\\Desktop\\projekt\ nmo')
setwd('C:/Users/Uzytkownik/Documents/Studia/nieklasyczne metody/School-Bus')
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

b = list(b1, b2, b3)
f = function(b, D)
{
  number_buses = length(b)
  n_list = lapply(b, length)
  d_list = rep(0, number_buses)
  y_list = rep(0, number_buses)
  for (bus in 1:number_buses){
    for (n in 1 : (n_list[[bus]]-1))
    { 
      y_list[bus] = y_list[bus] + Y[b[[bus]][n]]
      d_list[bus] = d_list[bus] + D[b[[bus]][n], b[[bus]][n+1]] * y_list[bus]
      if (bus == 1){
        print(y_list[bus])
        print(d_list[bus])
      }
      }
  }
  
  return(sum(d_list))
}

f(b, Tk)


