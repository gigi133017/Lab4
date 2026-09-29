
qr_desc <- function(A){
    stopifnot(is.matrix(A), nrow(A)>ncol(A))


    n <- nrow(A)
    m <- ncol(A)

    Q <- diag(n)
    R <- A

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

    return(c(Q, R))
}



# test <- qr_desc(A)
# Q <- matrix(test[1:m**2], nrow=m)
# R <- matrix(test[m**2+1:], nrow=m)


# Check if the output of our functions is the same as the inbuilt functions
# qr.R(qr(A)) == R
# qr.Q(qr(A)) == Q