remove(list = ls())
####################### Macierz kosztow #############################
#setwd('C:\\Users\\lenovo\\Desktop\\project nmo')
setwd('C:/Users/Uzytkownik/Documents/Studia/nieklasyczne metody/School-Bus')
# setwd("C:\\Users\\pc\\Desktop\\Nieklasyczne metody optymalizacji\\projekt")
koszty <- read.csv('koszty2.csv', sep = ';', dec = ",")
koszty <- koszty[,1:13]
rownames(koszty) <- koszty[,1]
koszty <- koszty[,2:13]
koszty <- as.data.frame(koszty)

Tk <- koszty
colnames(Tk) <- c(1:12)
rownames(Tk) <- c(1:12)
Tk <- as.matrix(Tk)


set.seed(1234)
####################### Zmienne #########################
B <- 3
K = 1:B
# Tk ij - odleglosc podrozy z przystanku i do przystanka j  
N = nrow(Tk)
S = 1:N


Y = round(rnorm(11,mean=12,sd=3),0)
#Wylosowana trasa poczatkowa b




#funkcja do sprawdzenia liczby przystankow 
czy_n_przystankow <- function(pass){
  n_przystankow <- c()
  for(i in 1:length(b)){
    n_przystankow[i] <- ifelse(length(b[[i]])>2, 1, 0)
  }
  return(sum(n_przystankow))}




constraint_1 <- function(pass){
  check <- c()
  for(i in 1:3){
    check[i] <- ifelse(pass[i]<50,1,0)
  }
  return(sum(check))}

constraint <- function(pass, buses){
  check <- c()
  for(i in 1:buses){
    check[i] <- ifelse(pass[i]<50,1,0)
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

repeat 
{ 
  #losujemy przystnaki dla kazdego busa
  b = split(1:11, sample(3, 11 , repl = TRUE))
  
  
  #dodajemy sgh na koncu "10"
  for (i in 1:length(b))
  {
    b[[i]] = c(b[[i]], 12)
    
  }
  
  if(czy_n_przystankow(b) == 3 &
     constraint_1(f2(b, Tk)) == 3) 
  {
    break
  }
  
}

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
          cons = constraint(wyn, buses)
          
        
          }
          
          
          
      }
      
      
    }
    if (exists("cons") == FALSE) next
    if (cons < buses) next
    
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

max_buses = 10
cost_bus = 20
outputs = c()
koszt_studenta_minuta = 1.6

for(bus in 3:max_buses){
  # od tego rozwi¹zania wyznaczamy dla kolejnych busów
  initial_solution = b
  if(bus > 3){
    # ile musimy dodaæ przystanków do pojedynczego nowego busa
    buses_to_change = round((length(S)-1)/bus, 0)
    for(i in 1:(bus-3)){
      # przystanki w nowym busie
      new_bus = c()
      for(new_stop in 1:buses_to_change){
        # jak¹ d³ugoœæ przystanków maj¹ obecne busy
        length_buses <- sapply(initial_solution, length)
        # wyznaczamy przystanki tego busa, który ma ich najwiêcej
        old_bus <- initial_solution[[which.max(length_buses)]]
        # losujemy przystanek, który zabieramy
        stop_to_change <- sample(old_bus[-length(old_bus)], 1)
        # dodajemy ten przystanek do nowego busa
        new_bus <- append(new_bus, stop_to_change)
        # usuwamy ten przystanek z poprzedniego busa
        initial_solution[[which.max(length_buses)]] = old_bus[!old_bus %in% stop_to_change]
      }
    # dodajemy SGH - 10 do busa
    new_bus <- append(new_bus, length(S))
    # dodajemy tego busa do naszego pocz¹tkowego rozwi¹zania
    initial_solution <- c(initial_solution, list(new_bus))
    }
    
  }
  new_value <- SA(S, bus, initial_solution, Tk, f, f2, constraint, delta, t, alpha, maxIt)[[2]]
  new_value <- koszt_studenta_minuta*new_value+(bus*cost_bus)
  outputs <- append(outputs, new_value)
}
