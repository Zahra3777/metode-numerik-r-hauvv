# 3. METODE FIXED-POINT
fixed_point <- function(g, f, x0, tol = 1e-4, max_iter = 20) {
  df_res <- data.frame(Iterasi = integer(), x_old = numeric(), 
                       x_new = numeric(), fx = numeric(), Error = numeric())
  x <- x0
  
  for (i in 1:max_iter) {
    x_new <- g(x)
    err <- abs(x_new - x)
    fx <- f(x_new)
    
    df_res[i, ] <- c(i, round(x, 5), round(x_new, 5), round(fx, 5), round(err, 5))
    
    if (err < tol) break
    x <- x_new
  }
  return(df_res)
}

# Soal 3: f(x) = x^2 - x - 2, dengan g(x) = sqrt(x + 2)
f3 <- function(x) { x^2 - x - 2 }
g3 <- function(x) { sqrt(x + 2) }

cat("=== 3. HASIL METODE FIXED-POINT (g(x) = sqrt(x + 2)) ===\n")
print(fixed_point(g3, f3, x0 = 1))