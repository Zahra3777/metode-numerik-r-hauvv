# 2. METODE REGULA FALSI
regula_falsi <- function(f, a, b, tol = 1e-4, max_iter = 20) {
  df_res <- data.frame(Iterasi = integer(), a = numeric(), b = numeric(), 
                       c = numeric(), fc = numeric(), Error = numeric())
  c_old <- a
  
  for (i in 1:max_iter) {
    fa <- f(a)
    fb <- f(b)
    c <- b - (fb * (a - b)) / (fa - fb)
    fc <- f(c)
    err <- abs(c - c_old)
    
    df_res[i, ] <- c(i, round(a, 5), round(b, 5), round(c, 5), round(fc, 5), round(err, 5))
    
    if (abs(fc) < tol || (i > 1 && err < tol)) break
    
    if (fa * fc < 0) { b <- c } else { a <- c }
    c_old <- c
  }
  return(df_res)
}

# Soal 2: f(x) = x^3 - 2x - 5
f2 <- function(x) { x^3 - 2*x - 5 }

cat("=== 2. HASIL METODE REGULA FALSI (f(x) = x^3 - 2x - 5) ===\n")
print(regula_falsi(f2, a = 2, b = 3))