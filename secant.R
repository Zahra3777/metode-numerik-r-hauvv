# 5. METODE SECANT
secant <- function(f, x0, x1, tol = 1e-4, max_iter = 20) {
  df_res <- data.frame(Iterasi = integer(), x0 = numeric(), x1 = numeric(), 
                       x2 = numeric(), fx2 = numeric(), Error = numeric())
  
  for (i in 1:max_iter) {
    fx0 <- f(x0)
    fx1 <- f(x1)
    
    x2 <- x1 - fx1 * (x1 - x0) / (fx1 - fx0)
    fx2 <- f(x2)
    err <- abs(x2 - x1)
    
    df_res[i, ] <- c(i, round(x0, 5), round(x1, 5), round(x2, 5), round(fx2, 5), round(err, 5))
    
    if (err < tol) break
    x0 <- x1
    x1 <- x2
  }
  return(df_res)
}

# Soal 5: f(x) = exp(x) - 3*x
f5 <- function(x) { exp(x) - 3*x }

cat("=== 5. HASIL METODE SECANT (f(x) = e^x - 3x) ===\n")
print(secant(f5, x0 = 0, x1 = 1))