##
# simulate 100 frequencies (lets use frequency instead of dominance for now)
library(zipfR)
library(rethinking)
# plural freq
N <- 100
ZM <- lnre("zm", alpha = 0.55, B = 0.2)
WF <- as.numeric(as.character(rlnre(ZM, n=N)))
hist(log(WF), breaks = 100, xaxt="n")
axis(side = 1, at = log(WF), labels = WF)
# singular freq
ZM2 <- lnre("zm", alpha = 0.55, B = 0.2)
WF2 <- as.numeric(as.character(rlnre(ZM2, n=N)))
hist(log(WF2), breaks = 100, xaxt="n")
axis(side = 1, at = log(WF2), labels = WF2)
D <- WF / (WF+WF2)

# simulate distances
# imagine a 2-dimensional space, with random x and y coordinates
curve(dunif(x, 0, 10), from=0, to=10)

X <- runif(n = N, min = 0, max = 10)
Y <- runif(n = N, min = 0, max = 10)
plot(X, Y)
# calculate Euclidan distance matrix
Dvec <- c()
for (i in 1:N){
  for (j in 1:N){
    a <- X[i] - X[j]
    b <- Y[i] - Y[j]
    c <- sqrt(a^2 + b^2)
    Dvec <- c(Dvec, c)
  }
}
Dmat <- matrix(data = Dvec, nrow = N, ncol = N)

# simulate correlations

etasq = 2
rhosq = 0.3
curve(etasq*exp(-rhosq*(x^2)), from = 0, to = max(Dmat))

# first covariance matrix
K <- matrix(0,nrow=N,ncol=N)
for ( i in 1:N ){
  for ( j in i:N ){
    if ( i == j ){
      K[i,j] <- etasq + 0.01
    } else {
      K[i,j] <- etasq * exp( -rhosq * Dmat[i,j]^2 )
    }
    K[j,i] <- K[i,j]
  }
}

# convert to correlation matrix
Rho <- round( cov2cor(K) , 2 )

# visualize true correlations:
psize <- D
psize <- exp(psize*1.5)-2

# plot raw data
plot( X , Y ,col=rangi2 , cex=psize , pch=16 , xlim=c(0,10), ylim=c(0,10) )

# overlay lines shaded by Rho
for( i in 1:N ){
  for ( j in 1:N ){
    if ( i < j ){
      lines( c( X[i], X[j] ) , c( Y[i], Y[j] ) , lwd=2 , 
             col=col.alpha("black",Rho[i,j]^2) )
    }
  }
}

# now generate the binomial data

means <- rep(0, N)
# means <- rnorm(n = N, mean = 0, sd = 2)

k <- as.vector(rmvnorm(1, mean = means , K ))

S <- rbinom(n = N, size = WF, prob = logistic(k))

word <- 1:100

# now let's see if we can recover the correlations
mS <- ulam(
  alist(
    S ~ dbinom(WF, prob),
    logit(prob) <- k[word],
    vector[100]:k ~ multi_normal(0, SIGMA),
    matrix[100,100]:SIGMA <- cov_GPL2( Dmat , etasq , rhosq , 0.01 ),
    etasq ~ dexp( 2 ),
    rhosq ~ dexp( 0.5 )
  ), data = list(S = S, WF = WF, word = word, Dmat = Dmat), chains = 4, cores = 4
)

post <- extract.samples(mS)

# check if k predictions are ok
plot(1:N, logistic(k), ylim=c(0,1), xlab="Nouns", ylab="Proportion")
means <- logistic(apply(post$k, 2, mean))
points(1:N, means, pch = 19)
PIs <- logistic(apply(post$k, 2, PI, .95))
for (l in 1:N) lines(c(l, l), PIs[,l])

# plot the posterior median covariance function
plot( NULL , xlab="distance" , ylab="covariance" ,
      xlim=c(0,15), ylim=c(0,3) )
# compute posterior median covariance
x_seq <- seq( from=0 , to=max(Dmat) , length.out=100 )

pmcov <- sapply( x_seq , function(x) post$etasq*exp(-post$rhosq*x^2) ) 
pmcov_med <- apply( pmcov , 2 , median )
lines( x_seq , pmcov_med , lwd=2 )
# plot 60 functions sampled from posterior
for ( i in 1:50 )
  curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add=TRUE , 
         col=col.alpha("black",0.3) )

# now check correlations
# compute posterior median covariance among societies 
Kpost <- matrix(0,nrow=N,ncol=N)
for ( i in 1:N ){
  for ( j in 1:N ){
    Kpost[i,j] <- median(post$etasq) * exp( -median(post$rhosq) * Dmat[i,j]^2 )
  }
}
diag(Kpost) <- median(post$etasq) + 0.01

# convert to correlation matrix
Rho_post <- round( cov2cor(Kpost) , 2 )

# plot raw data
plot( X , Y ,col=rangi2 , cex=psize , pch=16 , xlim=c(0,10), ylim=c(0,10) )

# overlay lines shaded by Rho
for( i in 1:N ){
  for ( j in 1:N ){
    if ( i < j ){
      lines( c( X[i], X[j] ) , c( Y[i], Y[j] ) , lwd=2 , 
             col=col.alpha("black",Rho_post[i,j]^2) )
    }
  }
}

# why is it perfect?

# now let's try with dominance

# create Dommat
Domvec <- c()
for (i in 1:N){
  for (j in 1:N){
    Domvec <- c(Domvec, max(D[i],D[j]))
  }
}
Dommat <- matrix(data = Domvec, nrow = N, ncol = N)


# simulate correlations

etasq = 2
a = log(0.3)
b = 1
rhosq_mat = exp(a + b*Dommat)

# first covariance matrix
K2 <- matrix(0,nrow=N,ncol=N)
for ( i in 1:N ){
  for ( j in i:N ){
    if ( i == j ){
      K2[i,j] <- etasq + 0.01
    } else {
      K2[i,j] <- etasq * exp( -rhosq_mat[i,j] * Dmat[i,j]^2 )
    }
    K2[j,i] <- K2[i,j]
  }
}

# convert to correlation matrix
Rho2 <- round( cov2cor(K2) , 2 )

# visualize true correlations:
psize <- D
psize <- exp(psize*1.5)-2

# plot raw data
plot( X , Y ,col=rangi2 , cex=psize , pch=16 , xlim=c(0,10), ylim=c(0,10) )

# overlay lines shaded by Rho
for( i in 1:N ){
  for ( j in 1:N ){
    if ( i < j ){
      lines( c( X[i], X[j] ) , c( Y[i], Y[j] ) , lwd=2 , 
             col=col.alpha("black",Rho2[i,j]^2) )
    }
  }
}

# now generate the binomial data

means <- rep(0, N)
# means <- rnorm(n = N, mean = 0, sd = 2)

k2 <- as.vector(rmvnorm(1, mean = means , K2, method = "svd" ))
# note that "eigen" and "chol" methods fail here: sigma is numerically not positive semidefinite
isSymmetric(K2)
eigen(K2)$values


S2 <- rbinom(n = N, size = WF, prob = logistic(k2))

word <- 1:100

code_mS2 <- "
functions{
    matrix cov_GPL2(matrix x, real sq_alpha, matrix sq_rho, real delta) {
        int N = dims(x)[1];
        matrix[N, N] K;
        for (i in 1:(N-1)) {
          K[i, i] = sq_alpha + delta;
          for (j in (i + 1):N) {
            K[i, j] = sq_alpha * exp(-sq_rho[i,j] * square(x[i,j]) );
            K[j, i] = K[i, j];
          }
        }
        K[N, N] = sq_alpha + delta;
        return K;
    }
}
data{
    int WF[100];
    int S2[100];
    int word[100];
    matrix[100,100] Dmat;
    matrix[100,100] Dommat;
}
parameters{
    vector[100] k;
    real<lower=0> etasq;
    real a;
    real b;
}
model{
    vector[100] prob;
    matrix[100,100] SIGMA;
    matrix[100,100] rhosq;
    etasq ~ exponential( 2 );
    a ~ normal( 0 , 2 );
    b ~ normal( 0 , 1 );
    for ( i in 1:100 ) {
        for ( j in 1:100 ) {
            rhosq[i,j] = exp(a + b * Dommat[i,j]);
        }
    }
    SIGMA = cov_GPL2(Dmat, etasq, rhosq, 0.01);
    k ~ multi_normal( rep_vector(0,100) , SIGMA );
    for ( i in 1:100 ) {
        prob[i] = k[word[i]];
        prob[i] = inv_logit(prob[i]);
    }
    S2 ~ binomial( WF , prob );
}
"
mS2 <- stan(model_code = code_mS2, data = list(S2 = S2, WF = WF, word = word, Dmat = Dmat, Dommat = Dommat), chains=4 , cores=4)


# this doesn't work need to find another way
# what if logit(prob[i]) = m[i]*D + k[i]
# where m[i] are just regular random intercepts and k[i] are the GP intercepts
# if Dominance negatively affects influence of phon. similar words
# we would expect that for higher D k[i] should be around 0 or would it just be very uncertain?
# for low D we let k[i] do most of the work

# plural freq
N <- 100
ZM <- lnre("zm", alpha = 0.55, B = 0.2)
WF <- as.numeric(as.character(rlnre(ZM, n=N)))
hist(log(WF), breaks = 100, xaxt="n")
axis(side = 1, at = log(WF), labels = WF)
# singular freq
ZM2 <- lnre("zm", alpha = 0.55, B = 0.2)
WF2 <- as.numeric(as.character(rlnre(ZM2, n=N)))
hist(log(WF2), breaks = 100, xaxt="n")
axis(side = 1, at = log(WF2), labels = WF2)
D <- WF / (WF+WF2)

# simulate distances
# imagine a 2-dimensional space, with random x and y coordinates
curve(dunif(x, 0, 10), from=0, to=10)

X <- runif(n = N, min = 0, max = 10)
Y <- runif(n = N, min = 0, max = 10)
plot(X, Y)
# calculate Euclidan distance matrix
Dvec <- c()
for (i in 1:N){
  for (j in 1:N){
    a <- X[i] - X[j]
    b <- Y[i] - Y[j]
    c <- sqrt(a^2 + b^2)
    Dvec <- c(Dvec, c)
  }
}
Dmat <- matrix(data = Dvec, nrow = N, ncol = N)

# simulate correlations

etasq = 2
rhosq = 0.3
curve(etasq*exp(-rhosq*(x^2)), from = 0, to = max(Dmat))

# first covariance matrix
K <- matrix(0,nrow=N,ncol=N)
for ( i in 1:N ){
  for ( j in i:N ){
    if ( i == j ){
      K[i,j] <- etasq + 0.01
    } else {
      K[i,j] <- etasq * exp( -rhosq * Dmat[i,j]^2 )
    }
    K[j,i] <- K[i,j]
  }
}

# convert to correlation matrix
Rho <- round( cov2cor(K) , 2 )

# visualize true correlations:
psize <- D
psize <- exp(psize*1.5)-2

# plot raw data
plot( X , Y ,col=rangi2 , cex=psize , pch=16 , xlim=c(0,10), ylim=c(0,10) )

# overlay lines shaded by Rho
for( i in 1:N ){
  for ( j in 1:N ){
    if ( i < j ){
      lines( c( X[i], X[j] ) , c( Y[i], Y[j] ) , lwd=2 , 
             col=col.alpha("black",Rho[i,j]^2) )
    }
  }
}

# now generate the binomial data
a_bar <- 0
sigma_a <- 1
b <- 0.9

m <- rnorm(n = N, mean = a_bar, sd = sigma_a)

k <- as.vector(rmvnorm(1, mean = rep(0, N) , K ))

prob <- logistic(m*b*D + k*(1-b*D))

S <- rbinom(n = N, size = WF, prob = prob)

word <- 1:100

# now let's see if we can recover the correlations
mS3 <- ulam(
  alist(
    S ~ dbinom(WF, prob),
    logit(prob) <- m[word]*b*D + k[word]*(1-b*D),
    m[word] ~ dnorm(0, 2),
#    a_bar ~ dnorm(0,2),
#    sigma_a ~ dnorm(0,2),
    b ~ dunif(0,1),
    vector[100]:k ~ multi_normal(0, SIGMA),
    matrix[100,100]:SIGMA <- cov_GPL2( Dmat , etasq , rhosq , 0.01 ),
    etasq ~ dexp( 2 ),
    rhosq ~ dexp( 0.5 )
  ), data = list(S = S, WF = WF, word = word, D = D, Dmat = Dmat), chains = 4, cores = 4
)

# dual route
library(sads)
library(rethinking)
N <- 100
WF <- rzipf(N, 10000, 1.5)
hist(log(WF), breaks = 100, xaxt="n")
axis(side = 1, at = log(WF), labels = WF)
# singular freq
WF2 <- rzipf(N, 10000, 1.5)
hist(log(WF2), breaks = 100, xaxt="n")
axis(side = 1, at = log(WF2), labels = WF2)
D <- WF / (WF+WF2)

# simulate distances
# imagine a 2-dimensional space, with random x and y coordinates
curve(dunif(x, 0, 10), from=0, to=10)

X <- runif(n = N, min = 0, max = 10)
Y <- runif(n = N, min = 0, max = 10)
plot(X, Y)
# calculate Euclidan distance matrix
Dvec <- c()
for (i in 1:N){
  for (j in 1:N){
    a <- X[i] - X[j]
    b <- Y[i] - Y[j]
    c <- sqrt(a^2 + b^2)
    Dvec <- c(Dvec, c)
  }
}
Dmat <- matrix(data = Dvec, nrow = N, ncol = N)

# simulate correlations

etasq = 2
rhosq = 0.3
curve(etasq*exp(-rhosq*(x^2)), from = 0, to = max(Dmat))

# first covariance matrix
K <- matrix(0,nrow=N,ncol=N)
for ( i in 1:N ){
  for ( j in i:N ){
    if ( i == j ){
      K[i,j] <- etasq + 0.01
    } else {
      K[i,j] <- etasq * exp( -rhosq * Dmat[i,j]^2 )
    }
    K[j,i] <- K[i,j]
  }
}

# convert to correlation matrix
Rho <- round( cov2cor(K) , 2 )

# visualize true correlations:
psize <- D
psize <- exp(psize*1.5)-2

# plot raw data
plot( X , Y ,col=rangi2 , cex=psize , pch=16 , xlim=c(0,10), ylim=c(0,10) )

# overlay lines shaded by Rho
for( i in 1:N ){
  for ( j in 1:N ){
    if ( i < j ){
      lines( c( X[i], X[j] ) , c( Y[i], Y[j] ) , lwd=2 , 
             col=col.alpha("black",Rho[i,j]^2) )
    }
  }
}


# now generate the binomial data
a <- 0
b <- -2
p_gp <- logistic(a + b*D)

a_bar <- 0
sigma_a <- 1

m <- rnorm(n = N, mean = a_bar, sd = sigma_a)

k <- as.vector(rmvnorm(1, mean = rep(0, N) , K ))

prob <- c()
routes <- c("GP", "RI")
chosen_routes <- c()
for (i in 1:N){
  route <- sample(routes, size = 1, replace = T, prob = c(p_gp[i], 1-p_gp[i]))
  chosen_routes <- c(chosen_routes, route)
  if (route == "GP"){
    prob <- c(prob, logistic(k[i]))
  } else {
    prob <- c(prob, logistic(m[i]))
  }
}

S <- rbinom(n = N, size = WF, prob = prob)

WF <- as.integer(WF)
S <- as.integer(S)
word <- 1:N

# mS4 <- ulam(
#   alist(
#     S ~ custom(log_mix(p_gp, binomial_lpmf(S|WF, theta_gp), binomial_lpmf(S|WF, theta))),
#     logit(theta_gp) <- k[word],
#     logit(theta) <- m[word],
#     logit(p_gp) <- a + b*D,
#     m[word] ~ dnorm(m_bar, sigma_m),
#     m_bar ~ dnorm(0,2),
#     sigma_m ~ dnorm(0,2),
#     a ~ dnorm(0,2),
#     b ~ dnorm(0,2),
#     vector[100]:k ~ multi_normal(0, SIGMA),
#     matrix[100,100]:SIGMA <- cov_GPL2( Dmat , etasq , rhosq , 0.01 ),
#     etasq ~ dexp( 2 ),
#     rhosq ~ dexp( 0.5 )
#   ), data = list(S=S, WF=WF, word=word, D=D, Dmat=Dmat), chains = 1, cores = 1
# )

mS4code <- "
functions{
    matrix cov_GPL2(matrix x, real sq_alpha, real sq_rho, real delta) {
        int M = dims(x)[1];
        matrix[M, M] K;
        for (i in 1:(M-1)) {
          K[i, i] = sq_alpha + delta;
          for (j in (i + 1):M) {
            K[i, j] = sq_alpha * exp(-sq_rho * square(x[i,j]) );
            K[j, i] = K[i, j];
          }
        }
        K[M, M] = sq_alpha + delta;
        return K;
    }
}
data{
    int<lower=0> N;
    int WF[N];
    int S[N];
    int word[N];
    matrix[N,N] Dmat;
    vector[N] D;
}
parameters{
    vector[N] k;
    real<lower=0> etasq;
    real<lower=0> rhosq;
    real a;
    real b;
    vector[N] m;
    real m_bar;
    real<lower=0> m_sigma;
}
model{
    vector[N] theta_gp;
    vector[N] theta;
    matrix[N,N] SIGMA;
    vector[N] p_gp;
    etasq ~ exponential( 2 );
    rhosq ~ exponential( 2 );
    a ~ normal( 0 , 2 );
    b ~ normal( 0 , 1 );
    SIGMA = cov_GPL2(Dmat, etasq, rhosq, 0.01);
    k ~ multi_normal( rep_vector(0,N) , SIGMA );
    m_sigma ~ exponential( 1 );
    m_bar ~ normal( 0 , 2 );
    m ~ normal( m_bar , m_sigma );
    for ( i in 1:N ) {
        theta_gp[i] = k[word[i]];
        theta_gp[i] = inv_logit(theta_gp[i]);
        theta[i] = m[word[i]];
        theta[i] = inv_logit(theta[i]);
        p_gp[i] = a + b*D[i];
        target += log_mix(p_gp[i], binomial_lpmf(S[i] | WF[i], theta_gp[i]), binomial_lpmf(S[i] | WF[i], theta[i]));
    }
}
"

mS4 <- stan(model_code = mS4code, data = list(N=N, S=S, WF=WF, word=word, D=D, Dmat=Dmat), chains = 1, cores = 1)

precis(mS4)

# betabin?

mS5code <- "
functions{
    matrix cov_GPL2(matrix x, real sq_alpha, real sq_rho, real delta) {
        int M = dims(x)[1];
        matrix[M, M] K;
        for (i in 1:(M-1)) {
          K[i, i] = sq_alpha + delta;
          for (j in (i + 1):M) {
            K[i, j] = sq_alpha * exp(-sq_rho * square(x[i,j]) );
            K[j, i] = K[i, j];
          }
        }
        K[M, M] = sq_alpha + delta;
        return K;
    }
}
data{
    int<lower=0> N;
    int WF[N];
    int S[N];
    int word[N];
    matrix[N,N] Dmat;
    vector[N] D;
}
parameters{
    vector[N] k;
    real<lower=0> etasq;
    real<lower=0> rhosq;
    real a;
    real b;
    real m_bar;
    real<lower=0> theta;
}
model{
    vector[N] theta_gp;
    real theta_bb;
    matrix[N,N] SIGMA;
    vector[N] p_gp;
    etasq ~ exponential( 2 );
    rhosq ~ exponential( 2 );
    a ~ normal( 0 , 2 );
    b ~ normal( 0 , 1 );
    SIGMA = cov_GPL2(Dmat, etasq, rhosq, 0.01);
    k ~ multi_normal( rep_vector(0,N) , SIGMA );
    m_bar ~ normal( 0 , 2 );
    theta ~ exponential( 1 );
    theta_bb = m_bar;
    theta_bb = inv_logit(theta_bb);
    for ( i in 1:N ) {
        theta_gp[i] = k[word[i]];
        theta_gp[i] = inv_logit(theta_gp[i]);
        p_gp[i] = a + b*D[i];
        target += log_mix(p_gp[i], binomial_lpmf(S[i] | WF[i], theta_gp[i]), beta_binomial_lpmf(S[i] | WF[i], theta_bb*theta, (1-theta_bb)*theta));
    }
}
"

mS5 <- stan(model_code = mS5code, data = list(N=N, S=S, WF=WF, word=word, D=D, Dmat=Dmat), chains = 1, cores = 1)


B <- rbinom(10, size = 100, prob = 0.5)
A <- rbinom(10, size = B, prob = 0.8)
M <- 10

mtest_code = "
data{
    int<lower=0> M;
    int B[M];
    int A[M];
}
parameters{
    real pl;
    real t;
    real bla;
}
model{
    real theta_bla;
    real theta;
    real p_bla;
    bla ~ normal( 0 , 2 );
    t ~ normal( 0 , 2 );
    pl ~ normal( 0 , 2 );
    p_bla = pl;
    p_bla = inv_logit(p_bla);
    theta = t;
    theta = inv_logit(theta);
    theta_bla = bla;
    theta_bla = inv_logit(theta_bla);
    for ( i in 1:M ) target += log_mix(p_bla, binomial_lpmf(A[i] | B[i], theta_bla), binomial_lpmf(A[i] | B[i], theta));
}
"

mtest <- stan(model_code = mtest_code, data = list(M=M, A=A, B=B), chains = 1, cores = 1)

mtest <- ulam(
  alist(
    A ~ dbetabinom(B, pbar, theta),
    logit(pbar) <- bla[item],
    bla[item] ~ dnorm(0, 2),
    theta ~ dexp(1)
  ), data = list(item=M, A=A, B=B), chains = 1, cores = 1
)


