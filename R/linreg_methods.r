#' Print linreg class object
#'
#' Print the coefficients and the call function of the linear regression
#'
#' @param x linreg object
#' @param ... other options
#' @export
print.linreg <- function(x, ...){
    cat("Call:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    print_list <- unlist(x["coeffs"])
    names(print_list) <- rownames(x[["coeffs"]])

    print(print_list)
}

#' Residuals of linreg object
#'
#' Returns all the residuals of the linreg object
#'
#' @param object linreg object
#' @param ... other options
#' @return float list
#' @export
residuals.linreg <- function(object, ...){
    return(object[["residual"]])
}

# As we are defining the method only for the class, we need a new dummy function
pred <- function(x) {
    UseMethod("pred")
}


#' Predictions of linreg object
#'
#' Returns all the predictions of the linreg object
#'
#' @param x linreg object
#' @return float list
#' @export
pred.linreg <- function(x){
    return(x[["predicted"]])
}

#' Coefficients of linreg object
#'
#' Returns all the coefficients of the linreg object
#'
#' @param object linreg object
#' @param ... other options
#' @return float list
#' @export
coef.linreg <- function(object, ...){  
    coef_vector <- unlist(object["coeffs"])
    names(coef_vector) <- rownames(object[["coeffs"]])

    return(coef_vector)
}

#' Summary of linreg object
#'
#' Prints on screen a summary of the different parameters of the linreg object.
#' This includes the estimate, the standard error, t value and p value for each
#' variable, as well as the estimated variance and the degrees of freedom.
#'
#' @param object linreg object
#' @param ... other options
#' @export
summary.linreg <- function(object, ...){
    p_values <- 2 * (1-pt(abs(object$t_value), object$dof))

    #asterisks <- seq_along(p_values)*0
    asterisks <- p_values

    for(i in seq_along(p_values)){
        if(p_values[i]<0.001)
          asterisks[i] <- "***"
        else if(p_values[i]<0.01)
          asterisks[i] <- "**"
        else if(p_values[i]<0.05)
          asterisks[i] <- "*"
        else if(p_values[i]<0.1)
          asterisks[i] <- "."
        else 
          asterisks[i] <- ""
    }

    df <- data.frame(cbind(object$coeffs, sqrt(diag(object$var_beta_hat)), object$t_value, p_values, asterisks))
    names(df) <- c("Estimate", "Std. Error", "t value", "p value", "")

    print(df)
    cat(
      "Residual standard error:",
      sqrt(object$var_hat),
      "on",
      object$dof,
      "degrees of freedom\n"
    )
}


#' Plot of linreg object
#'
#' Saves two plot figures
#' One is the Residuals for each predicted value
#' The second is the standardized resiuduals for each prediction.
#'
#' @param x linreg object
#' @param ... other options
#' @return two saved figures
#' @export
plot.linreg <- function(x, ...) {
  
  df <- data.frame(
    fitted = x$predicted,
    residuals = x$residual
  )
  
  mean_df <- aggregate(residuals ~ fitted, data = df, FUN = mean)
  g1 <- ggplot2::ggplot(df, aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_line(data = mean_df,aes(x = fitted, y = residuals), color = "#00b9e7",linewidth = 1) +
    labs(
      title = "Residuals vs Predicted",
      x = "Predicted",
      y = "Residual"
    ) +
    theme_minimal()

  df <- data.frame(
    fitted = x$predicted,
    stresiduals = sqrt(abs(x$residual/sqrt(var(x$residual)[1])))
  )
  
  mean_df1 <- aggregate(stresiduals ~ fitted, data = df, FUN = mean)
  g2 <- ggplot2::ggplot(df, aes(x = fitted, y = stresiduals)) +
    geom_point() +
    geom_line(data = mean_df1,aes(x = fitted, y = stresiduals), color = "#00b9e7",linewidth = 1) +
    labs(
      title = "Standardised residuals vs Predicted",
      x = "Predicted",
      y = "Standardised residual"
    ) +
    theme_minimal()
  #g1 + g2

  # We save the plots
  ggplot2::ggsave("ResVsPredicted.png", plot = g1, width = 6, height = 4)
  ggplot2::ggsave("Std_resVsPredicted.png", plot = g2, width = 6, height = 4)
}



# formula <- Sepal.Length ~ Sepal.Width + Petal.Length
formula <- Petal.Length ~ Species
r <- linreg(formula, iris)
