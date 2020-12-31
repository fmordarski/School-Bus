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

#Wylosowana trasa poczatkowa b
b = split(1:9, sample(3, 9 , repl = TRUE))

for (i in 1:length(b))
{
  b[[i]] = c(b[[i]], 10)
  
}



#Ustalona trasa poczatkowa b
#b1 = c(1,4,10)
#b2 = c(2,3,5,8, 10)
#b3 = c(6,7,9,10)

#b = list(b1, b2, b3)

#Funkcja kosztow
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
      }
  }
  
  return(sum(d_list))
}

f(b, Tk)

t = 100
alpha = 0.99
maxIt = 100

SA = function(stops, buses, x, D, f, delta, t, alpha, maxIt)
{
 
  out = list()
  out$x.hist = vector(mode = "list", length = maxIt)
  out$x.hist[[1]] = x
  out$f.hist = vector(mode = "list", length = maxIt)
  out$f.hist[[1]] = f(x, D)
  out$t.hist = vector(mode = "list", length = maxIt)
  out$t.hist[[1]] = t
  
  for (i in 1 : maxIt)
  {
    # Losowanie kandydata na rozwiazanie.
    stop = sample.int(length(stops)-1, 1)
    bus = sample.int(buses, 1)
    x_c = x
    for (b in 1:buses){
      if (is.na(match(stop, x[[b]])) == FALSE){
        if (length(x[[b]]) > 2){
        sol = b
        x_c[[b]] = x_c[[b]][!x_c[[b]] %in% stop]
        x_c[[bus]] = sort(append(x_c[[bus]], stop))
        }
      }
    }
    
    # Symulacja przejscia do kandydata na rozwiazanie.
    A = min(1, exp(-(f(x_c, D) - f(x, D)) / t))
    u = runif(1)
    if (u < A)
    {
      x = x_c
    }
    
    # Aktualizacja wartosci temperatury.
    t = alpha * t
    
    out$x.hist[[i+1]] = x
    out$f.hist[[i+1]] = f(x, D)
    out$t.hist[[i+1]] = t
  }
  
  out$x.opt = x
  
  return(out)
}

SA(S, B, b, Tk, f, delta, t, alpha, maxIt)

