#' Linear Regression using QR factorization
#'
#' This funcction calculates the parameters for a linear regression for a given
#'set of data and formula. QR factorization is used to get the final result.
#'The output is an object of class "linreg"
#'
#' @param formula formula
#' @param data data.frame
#' @return linreg class object
#' @export

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