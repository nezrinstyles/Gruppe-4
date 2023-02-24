# (a)
A <- function(data, var) {
      mean_val <- mean(data[[var]])
      median_val <- median(data[[var]])
      sd_val <- sd(data[[var]])
      min_val <- min(data[[var]])
      max_val <- max(data[[var]])
      stats <- c(mean_val, median_val, sd_val, min_val, max_val)
      names(stats) <- c("mean","median","sd","min","max")
      return(stats)
}

# (b)
B <- function(data, var) {
  freq <- table(data[[var]])
  freq_in_percent <- prop.table(freq)
  stats <- cbind(freq, freq_in_percent)
  return(stats)
}
  
# (c)
C <- function(data, var1, var2) {
  table_val <- table(data[[var1]], data[[var2]])
  stats <- chisq.test(table_val)
  return(stats)
}

# (d)
D <- function(data, var1, var2) {
  ttest <- t.test(data[[var1]] ~ data[[var2]])
  stats <- c(ttest$estimate, ttest$p.value)
  names(stats) <- c("mean in group FALSE","mean in group TRUE ","p-value")
  return(stats)
}

# (e)
E <- function(data, var) {
  category <- cut(data[[var]], 3, labels = c("Low", "Medium", "High"))
  return(category)
}

# (f)
F <- function(data, var1, var2, var3 = NULL, var4 = NULL) {
  if (!is.null(var4)) {
    plot <- ggplot(data, aes(x = var1, fill = var2)) + 
      geom_bar(position = "dodge") +
      facet_wrap(vars(var3, var4))
  } else if (!is.null(var3)) {
    plot <- ggplot(data, aes(x = var1, fill = var2)) + 
      geom_bar(position = "dodge") +
      facet_wrap(var3)
  } else {
    plot <- ggplot(data, aes(x = var1, fill = var2)) + 
      geom_bar(position = "dodge")
  }
  return(plot)
}





