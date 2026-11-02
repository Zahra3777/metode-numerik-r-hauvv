# 4. METODE NEWTON-RAPHSON
newton_raphson <- function(f, df, x0, tol = 1e-4, max_iter = 20) {
  df_res <- data.frame(Iterasi = integer(), x_old = numeric(), fx = numeric(), 
                       dfx = numeric(), x_new = numeric(), Error = numeric())
  x <- x0
  
  for (i in 1:max_iter) {
    fx <- f(x)
    dfx <- df(x)
    x_new <- x - (fx / dfx)
    err <- abs(x_new - x)
    
    df_res[i, ] <- c(i, round(x, 5), round(fx, 5), round(dfx, 5), round(x_new, 5), round(err, 5))
    
    if (err < tol) break
    x <- x_new
  }
  return(df_res)
}

# Soal 4: f(x) = x^3 - x - 1
f4  <- function(x) { x^3 - x - 1 }
df4 <- function(x) { 3*x^2 - 1 }

cat("=== 4. HASIL METODE NEWTON-RAPHSON (f(x) = x^3 - x - 1) ===\n")
print(newton_raphson(f4, df4, x0 = 1.5))