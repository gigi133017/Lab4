#' Print linreg class object
#'
#' Print the coefficients and the call function of the linear regression
#'
#' @param x linreg object
#' @export
print.linreg <- function(x){
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
#' @param x linreg object
#' @return float list
#' @export
resid.linreg <- function(x){
    return(x["residual"])
}

#' Predictions of linreg object
#'
#' Returns all the predictions of the linreg object
#'
#' @param x linreg object
#' @return float list
#' @export
pred.linreg <- function(x){
    return(x["predicted"])
}

#' Coefficients of linreg object
#'
#' Returns all the coefficients of the linreg object
#'
#' @param x linreg object
#' @return float list
#' @export
coef.linreg <- function(x){  
    coef_vector <- unlist(x["coeffs"])
    names(coef_vector) <- all.vars(formula)
    names(coef_vector)[1] <- "(Intercept)"

    return(coef_vector)
}

#' Summary of linreg object
#'
#' Prints on screen a summary of the different parameters of the linreg object.
#' This includes the estimate, the standard error, t value and p value for each
#' variable, as well as the estimated variance and the degrees of freedom.
#'
#' @param x linreg object
#' @export
summary.linreg <- function(x){
    p_values <- 2 * (1-pt(abs(x$t_value), x$dof))
    df <- data.frame(cbind(x$coeffs, sqrt(diag(x$var_beta_hat)), x$t_value, p_values))
    names(df) <- c("Estimate", "Std. Error", "t value", "p value")

    print(df)
    cat("\nEstimated sigma**2\n")
    print(x$var_hat)
    cat("\nDegrees of Freedom\n")
    print(x$dof)
}

library(ggplot2)
library(patchwork)

#' Plot of linreg object
#'
#' Returns two plot figures
#'
#' @param x linreg object
#' @return float list
#' @export
plot.linreg <- function(x) {
  
  df <- data.frame(
    fitted = x$predicted,
    residuals = x$residual
  )
  
  mean_df <- aggregate(residuals ~ fitted, data = df, FUN = mean)
  g1 <- ggplot(df, aes(x = fitted, y = residuals)) +
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
  g2 <- ggplot(df, aes(x = fitted, y = stresiduals)) +
    geom_point() +
    geom_line(data = mean_df1,aes(x = fitted, y = stresiduals), color = "#00b9e7",linewidth = 1) +
    labs(
      title = "Standardised residuals vs Predicted",
      x = "Predicted",
      y = "Standardised residual"
    ) +
    theme_minimal()
  g1 + g2
}



# formula <- Sepal.Length ~ Sepal.Width + Petal.Length
formula <- Petal.Length ~ Species
r <- linreg(formula, iris)
