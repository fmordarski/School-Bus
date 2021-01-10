remove(list = ls())
####################### Macierz kosztow #############################
#setwd('C:\\Users\\lenovo\\Desktop\\project nmo')
#setwd('C:/Users/Uzytkownik/Documents/Studia/nieklasyczne metody/School-Bus')
setwd("C:\\Users\\pc\\Desktop\\Nieklasyczne metody optymalizacji\\projekt")
koszty <- read.csv('koszty.csv', sep = ';', dec = ",")
koszty <- koszty[,1:11]
rownames(koszty) <- koszty[,1]
koszty <- koszty[,2:11]
koszty <- as.data.frame(koszty)

Tk <- koszty
colnames(Tk) <- c(1:10)
rownames(Tk) <- c(1:10)
Tk <- as.matrix(Tk)


set.seed(1234)
####################### Zmienne #########################
B <- 3
K = 1:B
# Tk ij - odleglosc podrozy z przystanku i do przystanka j  
N = nrow(Tk)
S = 1:N


Y = round(rnorm(9,mean=12,sd=3),0)
Y
sum(Y)
#Wylosowana trasa poczatkowa b
#b = split(1:9, sample(3, 9 , repl = TRUE))
#
#for (i in 1:length(b))
#{
#  b[[i]] = c(b[[i]], 10)
#  
#}



#Ustalona trasa poczatkowa b
b1 = c(1,4, 9, 10)
b2 = c(2,3,5,8, 10)
b3 = c(6,7,10)

b = list(b1, b2, b3)


constraint <- function(pass){
  check <- c()
  for(i in 1:3){
    check[i] <- ifelse(pass[i]<45,1,0)
  }
  return(sum(check))}




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


f2 = function(b, D)
{
  number_buses = length(b)
  n_list = lapply(b, length)
  d_list = rep(0, number_buses)
  y_list = rep(0, number_buses)
  for (bus in 1:number_buses){
    for (n in 1 : (n_list[[bus]]-1))
    { 
      y_list[bus] = y_list[bus] + Y[b[[bus]][n]]
      
    }
  }
  
  return(y_list)
}




wyn =f2(b, Tk)

constraint(wyn)



t = 100
alpha = 0.99
maxIt = 1000

SA = function(stops, buses, x, D, f, f2, constraint, delta, t, alpha, maxIt)
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
    index = sample.int(length(x[[bus]])-1, 1)
    x_c = x
    for (b in 1:buses){
      if (is.na(match(stop, x[[b]])) == FALSE){
        if (length(x[[b]]) > 2){
          sol = b
          x_c[[b]] = x_c[[b]][!x_c[[b]] %in% stop]
          x_c[[bus]] = append(x_c[[bus]], stop, after = index-1)
          
          wyn = f2(x_c, D)
          cons = constraint(wyn)
          
        
          }
          
          
          
      }
      
      
    }
    if (cons < 3) next
    
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
  
  opt_f.hist = min(unlist(out$f.hist)) #optymalna wartosc funkcji celu (minimalna)
  index.min = which.min(unlist(out$f.hist)) #indeks z optymalna wartoscia funkcji celu
  x.opt = out$x.hist[[index.min]] #x optymalny, ktory wyznacza optymalna wartosc f. celu
  f.hist = unlist(out$f.hist)
  
  ret = list(x.opt, opt_f.hist,f.hist)
  
  return(ret)
  
}

Z <- SA(S, B, b, Tk, f, f2, constraint, delta, t, alpha, maxIt)
plot(1:length(Z[[3]]),Z[[3]])
