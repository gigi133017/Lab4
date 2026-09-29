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

    # ---------------------
    #  /QR - factorization 
    # ---------------------
    R <- R[1:m,]
    Q <- Q[,1:m]

    beta_hat <- solve(R) %*% t(Q) %*% y
    predicted <- X %*% beta_hat
    residual <- y - predicted
    dof <- n - m
    var_hat <- (t(residual) %*% residual)[1] / dof
    var_beta_hat <- var_hat * solve(t(R)%*%R)
    t_value <- beta_hat / sqrt(diag(var_beta_hat))

    # Create a variable with class linreg 
    result <- list( coefs=beta_hat,
                    predicted= predicted,
                    residual=residual,
                    dof=dof,
                    var_hat=var_hat,
                    var_beta_hat=var_beta_hat,
                    t_value=t_value)

    class(result) <- "linreg"

    return(result)
}

# # linreg S3 class constructor
# new_linreg <- function(x, beta_hat, predicted, residual, dof, var_hat, var_beta_hat, t_value){
#     structure(x, class="linreg",beta_hat=beta_hat, predicted=predicted, residual=residual, dof=dof, var_hat=var_hat, var_beta_hat=var_beta_hat, t_value=t_value)
# }

# resid.linreg <- function(x){
#     return(x[residual])
# }