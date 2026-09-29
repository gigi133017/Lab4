linreg <- function(formula, data){
    #stopifnot

    X <- model.matrix(formula, data)
    y <- data[[all.vars(formula)[1]]]

    # ---------------------
    # QR - factorization 
    # ---------------------
    stopifnot(is.matrix(X), nrow(X)>ncol(X))

    n <- nrow(X)
    m <- ncol(X)

    Q <- diag(n)
    R <- X

    for(i in 1:m){
        # Initialize our vector with zeros and then take the rest from our matrix
        v <- seq_len(n)*0
        v[i:n] <- R[i:n,i]

        # Calculate the Householder matrix
        v[i] <- v[i] + sign(v[i])*sqrt(sum(v**2))
        H <- diag(nrow(R)) - 2* (v %*% t(v)) / (t(v) %*% v)[1]

        # Update our matrixes
        R <- H %*% R
        Q <- Q %*% H
    }

    R <- R[1:m,]
    Q <- Q[,1:m]

    # ---------------------
    #  /QR - factorization 
    # ---------------------

    beta_hat <- solve(R) %*% t(Q) %*% y
    predicted <- X %*% beta_hat
    residual <- y - predicted
    dof <- n - m
    var_hat <- (t(residual) %*% residual)[1] / dof
    var_beta_hat <- var_hat * solve(t(R)%*%R)
    t_value <- beta_hat / sqrt(diag(var_beta_hat))

    # Create a variable with class linreg 
    result <- list( coeffs=beta_hat,
                    predicted= predicted,
                    residual=residual,
                    dof=dof,
                    var_hat=var_hat,
                    var_beta_hat=var_beta_hat,
                    t_value=t_value,
                    call=match.call(),
                    formula=formula)

    class(result) <- "linreg_class"

    return(result)
}

# # linreg S3 class constructor
# new_linreg <- function(x, beta_hat, predicted, residual, dof, var_hat, var_beta_hat, t_value){
#     structure(x, class="linreg",beta_hat=beta_hat, predicted=predicted, residual=residual, dof=dof, var_hat=var_hat, var_beta_hat=var_beta_hat, t_value=t_value)
# }

print.linreg_class <- function(x){
    cat("Call:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    print_list <- unlist(x["coeffs"])
    names(print_list) <- all.vars(formula)
    names(print_list)[1] <- "(Intercept)"

    print(print_list)
}

resid.linreg_class <- function(x){
    return(x["residual"])
}

pred.linreg_class <- function(x){
    return(x["predicted"])
}

coef.linreg_class <- function(x){  
    coef_vector <- unlist(x["coeffs"])
    names(coef_vector) <- all.vars(formula)
    names(coef_vector)[1] <- "(Intercept)"

    return(coef_vector)
}

summary.linreg_class <- function(x){
    p_values <- 2 * (1-pt(abs(r$t_value), r$dof))
    df <- data.frame(cbind(r$coeffs, sqrt(diag(r$var_beta_hat)), r$t_value, p_values))
    names(df) <- c("Estimate", "Std. Error", "t value", "p value")

    print(df)
    cat("\nEstimated sigma**2\n")
    print(r$var_hat)
    cat("\nDegrees of Freedom\n")
    print(r$dof)
}

formula <- Sepal.Length ~ Sepal.Width + Petal.Length
r <- linreg(formula, iris)

l <- lm(formula, iris)