

set.seed(123456)
bootstrap_diff_one_sided <- function(x, y, R = 10000, alternative = "greater") {
  diff_vals <- replicate(R, {
    x_sample <- sample(x, length(x), replace = TRUE)
    y_sample <- sample(y, length(y), replace = TRUE)
    mean(x_sample) - mean(y_sample)
  })
  
  if (alternative == "greater") {
    p_value <- (sum(diff_vals <= 0) + 1) / (R + 1)
  } else if (alternative == "less") {
    p_value <- (sum(diff_vals >= 0) + 1) / (R + 1)
  } else {
    stop("alternative must be 'greater' or 'less'")
  }
  
  ci <- quantile(diff_vals, c(0.025, 0.975))
  
  list(
    mean_diff = mean(x) - mean(y),
    boot_ci = ci,
    p_value = p_value,
    alternative = alternative
  )
}


# CD8
bootstrap_diff_one_sided(a1, a2, alternative = "greater")
bootstrap_diff_one_sided(a2, a3, alternative = "greater") 
bootstrap_diff_one_sided(a1, a3, alternative = "greater") 

# CD4
bootstrap_diff_one_sided(b1, b2, alternative = "greater") 
bootstrap_diff_one_sided(b2, b3, alternative = "greater") 
bootstrap_diff_one_sided(b1, b3, alternative = "greater") 

# CPS
bootstrap_diff_one_sided(c1, c2, alternative = "greater") 
bootstrap_diff_one_sided(c2, c3, alternative = "greater") 
bootstrap_diff_one_sided(c1, c3, alternative = "greater") 




set.seed(123456)
bootstrap_diff_two_sided <- function(x, y, R = 10000) {
  diff_vals <- replicate(R, {
    x_sample <- sample(x, length(x), replace = TRUE)
    y_sample <- sample(y, length(y), replace = TRUE)
    mean(x_sample) - mean(y_sample)
  })
  
  obs_diff <- mean(x) - mean(y)
  
  p_left <- (sum(diff_vals <= 0) + 1) / (R + 1)
  p_right <- (sum(diff_vals >= 0) + 1) / (R + 1)
  p_value <- min(1, 2 * min(p_left, p_right))
  
  ci <- quantile(diff_vals, c(0.025, 0.975))
  
  list(
    mean_diff = obs_diff,
    boot_ci = ci,
    p_left = p_left,
    p_right = p_right,
    p_value = p_value,
    alternative = "two.sided"
  )
}

# CD8
bootstrap_diff_two_sided(a1, a2) 
bootstrap_diff_two_sided(a2, a3) 
bootstrap_diff_two_sided(a1, a3) 
# CD4
bootstrap_diff_two_sided(b1, b2) 
bootstrap_diff_two_sided(b2, b3) 
bootstrap_diff_two_sided(b1, b3) 
# CPS
bootstrap_diff_two_sided(c1, c2) 
bootstrap_diff_two_sided(c2, c3) 
bootstrap_diff_two_sided(c1, c3) 




























