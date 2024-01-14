library("hetGP")

######################### conventional, homoskedastic GP
# negative log-likelihood
# nLL <- function(par, X, Y) {
#     theta <- par[1:ncol(X)]
#     tau2 <- par[ncol(X) + 1]
#     n <- length(Y)
#     K <- cov_gen(X1 = X, theta = theta) + diag(tau2, n)
#     Ki <- solve(K)
#     ldetK <- determinant(K, logarithm = TRUE)$modulus

#     return((n / 2) * log(t(Y) %*% Ki %*% Y) + (1 / 2) * ldetK)
# }

# # gradient of negative log-likelihood
# gnLL <- function(par, X, Y) {
#     n <- length(Y)
#     theta <- par[1:ncol(X)]
#     tau2 <- par[ncol(X) + 1]
#     K <- cov_gen(X1 = X, theta = theta) + diag(tau2, n)
#     Ki <- solve(K)
#     KiY <- Ki %*% Y
#     dlltheta <- rep(0, length(theta))
#     for (k in 1:length(dlltheta)) {
#         dotK <- K * as.matrix(dist(X[, k]))^2 / (theta[k]^2)
#         dlltheta[k] <- n * t(KiY) %*% dotK %*% KiY / (t(Y) %*% KiY) - sum(diag(Ki %*% dotK))
#     }
#     dlltau2 <- n * t(KiY) %*% KiY / (t(Y) %*% KiY) - sum(diag(Ki))

#     return(-c(dlltheta / 2, dlltau2 / 2))
# }

# library("lhs")
# X <- 6 * randomLHS(40, 2) - 2
# X <- rbind(X, X)
# y <- X[, 1] * exp(-X[, 1]^2 - X[, 2]^2) + rnorm(nrow(X), sd = 0.01)

# Lwr <- sqrt(.Machine$double.eps)
# Upr <- 10

# out <- optim(c(rep(0.1, 2), 0.1 * var(y)), nLL, gnLL,
#     method = "L-BFGS-B",
#     lower = Lwr, upper = c(rep(Upr, 2), var(y)), X = X, Y = y
# )
# print(out$par)

# # Ki <- solve(cov_gen(X, theta = out$par[1:2]) + diag(out$par[3], nrow(X)))
# # nuhat <- drop(t(y) %*% Ki %*% y / nrow(X))
# # xx <- seq(-2, 4, length = 40)
# # XX <- as.matrix(expand.grid(xx, xx))

# # KXX <- cov_gen(XX, theta = out$par[1:2]) + diag(out$par[3], nrow(XX))
# # KX <- cov_gen(XX, X, theta = out$par[1:2])
# # mup <- KX %*% Ki %*% y
# # Sigmap <- nuhat * (KXX - KX %*% Ki %*% t(KX))

# fit <- mleHomGP(X, y, rep(Lwr, 2), rep(Upr, 2),
#     known = list(beta0 = 0),
#     init = c(list(theta = rep(0.1, 2), g = 0.1 * var(y)))
# )

# print(c(fit$theta, fit$g))



######################### conventional, heteroskedastic GP
library("MASS")
# hom <- mleHomGP(mcycle$times, mcycle$accel, covtype = "Matern5_2")
# het <- mleHetGP(mcycle$times, mcycle$accel, covtype = "Matern5_2")

# Xgrid <- matrix(seq(0, 60, length = 301), ncol = 1)
# p <- predict(x = Xgrid, object = hom)
# p2 <- predict(x = Xgrid, object = het)

# print(length(Xgrid))
# print(c(length(p$mean), length(p2$mean)))
# plot(Xgrid, p$mean)
# plot(Xgrid, p2$mean)

Xbar <- randomLHS(200, 2)
a <- sample(1:100, nrow(Xbar), replace = TRUE)
X <- matrix(0, ncol = 2, nrow = sum(a))
nf <- 0
for (i in 1:nrow(Xbar)) {
    X[(nf + 1):(nf + a[i]), ] <- matrix(rep(Xbar[i, ], a[i]),
        ncol = 2,
        byrow = TRUE
    )
    nf <- nf + a[i]
}
Y <- apply(X, 1, sirEval)
fit <- mleHetGP(X, Y,
    covtype = "Matern5_2", lower = rep(0.05, 2),
    upper = rep(10, 2), settings = list(linkThetas = "none"), maxit = 1e4
)
print(fit)
