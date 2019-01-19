

x <- rnorm(50)
y <- runif(30)
# Do x and y come from the same distribution?
ks.test(x, y)

set.seed(100)

ks.test.v <- vector(mode = "numeric", length = 1000)
t.test.v  <- vector(mode = "numeric", length = 1000)

for ( i in 1:1000) {
  x <- rnorm(100)
  y <- rnorm(100) + .5
  ks.test.v[i] <- ks.test(x, y)$p.value
  t.test.v[i] <- t.test(x, y)$p.value
}

summary(ks.test.v)
summary(t.test.v)

summary(ks.test.v < 0.01)
summary(t.test.v < 0.01)


set.seed(100)

for ( i in 1:1000) {
  x <- rnorm(100)
  y <- rnorm(100) + .5
  x <- c(x, rnorm(2) + 0)
  y <- c(y, rnorm(2) + 0)
  ks.test.v[i] <- ks.test(x, y)$p.value
  t.test.v[i] <- t.test(x, y)$p.value
}

summary(ks.test.v)
summary(t.test.v)

summary(ks.test.v < 0.01)
summary(t.test.v < 0.01)



set.seed(100)

for ( i in 1:1000) {
  x <- rnorm(100)
  y <- rlnorm(100) - 0.9
  #x <- c(x, rnorm(2) + 0)
  #y <- c(y, rnorm(2) + 0)
  ks.test.v[i] <- ks.test(x, y)$p.value
  t.test.v[i] <- t.test(x, y)$p.value
}

summary(ks.test.v)
summary(t.test.v)

summary(ks.test.v < 0.01)
summary(t.test.v < 0.01)




