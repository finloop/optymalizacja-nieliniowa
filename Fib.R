
################################# Fibonacci ############################
# Obliczenie pierwszych 101 wyrazów ciągu fib i zapisanie go w wektorze
phil <- c(rep(0, 100))
phil[1:3] <- c(1,1,1)
for(i in c(3:length(phil))) {phil[i] = phil[i-1] + phil[i-2]}

phi <- function(i) {
  if(i == 0) {return(0)}; # F(0) = 0
  if (i > length(phil)) { # Obliczenie nowych elementów jeżeli wyjdziemy poza zakres
    len <- length(phil)
    phil <- c(phil, rep(0, i - len))
    for(j in c(len:length(phil))) {phil[j] = phil[j-1] + phil[j-2]}
  }
  return(phil[i])
}

# Funkcja zwracająca dowolny element ciągu Fibonacciego
phi <- function(i) {
  return(phil[i])
}


fib_k <-function(lower, upper, tol) {
  i <- 1
  l <- upper - lower
  while(phi(i) < l / tol) {
    i <- i + 1
  }
  return(i)
}

phi <- function(i) {
  if (i <= 1) {
    return(i)
  } else {
    return(phi(i-1) + phi(i-2))
  }
}

fibonacci <- function(f,lower, upper, tol){
  k <- fib_k(lower, upper, tol) + 1
  c <- upper - phi(k-1-i)/phi(k-i)*(upper - lower)
  d <- lower + upper -c
  i <- 0
  cat("iteracja=", i, "a=", lower, "c=",c,"d=",d,"b=", upper , "beta=", phi(k-1-i)/phi(k-i),"\n",sep=" ")
  while (i <= k-4) {
    if (f(c) < f(d)) {
      upper <- d
      i <- i + 1
    } else {
      lower <- c
      i <- i + 1
    }
    c <- upper - phi(k-1-i)/phi(k-i)*(upper - lower)
    d <- lower + upper - c
    cat("iteracja=", i, "a=", lower, "c=",c,"d=",d,"b=", upper , "beta=", phi(k-1-i)/phi(k-i),"\n",sep=" ")
  }
  return((lower + upper)/2)
}

plot(f1, xlim = c(-5, 3.5), ylim = c(-5, 20))
fibonacci(f1, -5, -0.1, 1e-3)


fibonacci_max <- function(f,lower, upper, tol){
  fm <- function(x) -f(x)
  fibonacci(fm, lower, upper, tol)
}

plot(f2, xlim=c(6,10))
plot(f2, xlim=c(-2,2))

fibonacci_max(f2, 0, 1, 1e-3)
fibonacci_max(f2, 6, 10, 1e-3)

################################# Lagrange ############################

lagrange <- function(f,lower, upper, tol, tol_d){
  d <- lower
  old.d <- upper
  c <- (lower + upper)/2
  f.c <- f(c)
  d <- (1/2) * (f(lower) * (c^2 - upper^2) + f.c * (upper^2 - lower^2)
                + f(upper) * (lower^2 - c^2)) / (f(lower) * (c - upper)
                                                 + f.c * (upper - lower) + f(upper) * (lower - c))
  f.d <- f(d)
  i <- 0
  cat("iteracja=", i, "a=", lower, "c=",c,"d=",d,"b=", upper, "\n",sep=" ")
  while((abs(upper - lower) > tol) && (abs(old.d - d) > tol_d)){
    if ((lower < d) && (d < c)) {
      if (f.d < f.c) {
        upper <- c 
        c <- d
        f.c <- f.d
      } else {
        lower <- d
      } 
    } else if ((c < d) && (d < upper)) {
      if (f.d < f.c) {
        lower <- c
        c <- d
        f.c <- f.d
      } else {
        upper <- d
      } 
    } else {stop("Algorytm nie jest zbieżny") 
    }
    old.d <- d
    d <- (1/2) * (f(lower) * (c^2 - upper^2) + f.c * (upper^2 - lower^2)
                  + f(upper) * (lower^2 - c^2)) / (f(lower) * (c - upper)
                                                   + f.c * (upper - lower) + f(upper) * (lower - c))
    f.d <- f(d)
    i <- i + 1
    cat("iteracja=", i, "a=", lower, "c=",c,"d=",d,"b=", upper, "\n",sep=" ")
  } 
  return(d)
}