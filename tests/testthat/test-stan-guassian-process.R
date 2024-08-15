skip_on_cran()
skip_on_os("windows")

# Helper functions
linspaced_vector <- function(n, start, end) {
  seq(start, end, length.out = n)
}

to_vector <- function(x) {
  as.vector(x)
}

test_that("diagSPD_EQ returns correct dimensions and values", {
  alpha <- 1.0
  rho <- 2.0
  L <- 1.0
  M <- 5
  result <- diagSPD_EQ(alpha, rho, L, M)
  expect_equal(length(result), M)
  expect_true(all(result > 0))  # Expect spectral density to be positive
  # Check specific values for known inputs
  indices <- linspaced_vector(M, 1, M)
  expected_result <- alpha * sqrt(sqrt(2 * pi) * rho) * exp(-0.25 * (rho * pi / (2 * L))^2 * indices^2)
  expect_equal(result, expected_result, tolerance = 1e-8)
})

test_that("diagSPD_Matern returns correct dimensions and values", {
  nu <- 1.5
  alpha <- 1.0
  rho <- 2.0
  L <- 1.0
  M <- 5
  result <- diagSPD_Matern(nu, alpha, rho, L, M)
  expect_equal(length(result), M)
  expect_true(all(result > 0))  # Expect spectral density to be positive
  # Check specific values for known inputs
  indices <- linspaced_vector(M, 1, M)
  factor <- 2 * alpha * (sqrt(2 * nu) / rho)^nu
  denom <- (sqrt(2 * nu) / rho)^2 + (pi / (2 * L) * indices)^(nu + 0.5)
  expected_result <- factor / denom
  expect_equal(result, expected_result, tolerance = 1e-8)
})

test_that("diagSPD_Periodic returns correct dimensions and values", {
  alpha <- 1.0
  rho <- 2.0
  M <- 5
  result <- diagSPD_Periodic(alpha, rho, M)
  expect_equal(length(result), 2 * M)  # Expect double the dimensions due to append_row
  expect_true(all(result > 0))  # Expect spectral density to be positive
})

test_that("diagSPD_Linear returns correct dimensions and values", {
  alpha <- 1.0
  L <- 1.0
  M <- 5
  result <- diagSPD_Linear(alpha, L, M)
  expect_equal(length(result), M)
  expect_true(all(result > 0))  # Expect spectral density to be positive
  # Check specific values for known inputs
  indices <- linspaced_vector(M, 1, M)
  expected_result <- alpha * L^2 / (pi^2 * indices^2)
  expect_equal(result, expected_result, tolerance = 1e-8)
})

test_that("PHI returns correct dimensions and values", {
  N <- 5
  M <- 3
  L <- 1.0
  x <- seq(0, 1, length.out = N)
  result <- PHI(N, M, L, x)
  expect_equal(dim(result), c(N, M))
  # Check specific values for known inputs
  expected_result <- sin(outer(x + L, 1:M, function(x, m) pi / (2 * L) * x * m)) / sqrt(L)
  expect_equal(result, expected_result, tolerance = 1e-8)
})

test_that("PHI_periodic returns correct dimensions and values", {
  N <- 5
  M <- 3
  w0 <- 1.0
  x <- seq(0, 1, length.out = N)
  result <- PHI_periodic(N, M, w0, x)
  expect_equal(dim(result), c(N, 2 * M))  # Cosine and sine terms
  # Check specific values for known inputs
  mw0x <- outer(w0 * x, 1:M, function(x, m) x * m)
  expected_result <- cbind(cos(mw0x), sin(mw0x))
  expect_equal(result, expected_result, tolerance = 1e-8)
})

test_that("setup_noise returns correct count of noise terms", {
  ot_h <- 10
  t <- 10
  horizon <- 0
  estimate_r <- 1
  stationary <- 1
  future_fixed <- 0
  fixed_from <- 0
  result <- setup_noise(ot_h, t, horizon, estimate_r, stationary, future_fixed, fixed_from)
  expect_equal(result, ot_h)
  # Test with different parameters
  result <- setup_noise(ot_h, t, horizon, estimate_r, 0, future_fixed, fixed_from)
  expect_equal(result, ot_h - 1)
  result <- setup_noise(ot_h, t, horizon, 0, stationary, future_fixed, fixed_from)
  expect_equal(result, t)
  result <- setup_noise(ot_h, t, 2, estimate_r, stationary, 1, 3)
  expect_equal(result, ot_h - 2 + 3)
})

test_that("setup_gp returns correct dimensions and values", {
  M <- 3
  L <- 1.0
  dimension <- 5
  is_periodic <- 0
  w0 <- 1.0
  result <- setup_gp(M, L, dimension, is_periodic, w0)
  expect_equal(dim(result), c(dimension, M))
  # Compare with direct PHI call
  x <- linspaced_vector(dimension, 1, dimension)
  x <- (x - mean(x)) / sd(x)
  expected_result <- PHI(dimension, M, L, x)
  expect_equal(result, expected_result, tolerance = 1e-8)
})

test_that("setup_gp with periodic basis functions returns correct dimensions and values", {
  M <- 3
  L <- 1.0
  dimension <- 5
  is_periodic <- 1
  w0 <- 1.0
  result <- setup_gp(M, L, dimension, is_periodic, w0)
  expect_equal(dim(result), c(dimension, 2 * M))  # Cosine and sine terms
  # Compare with direct PHI_periodic call
  x <- linspaced_vector(dimension, 1, dimension)
  x <- (x - mean(x)) / sd(x)
  expected_result <- PHI_periodic(dimension, M, w0, x)
  expect_equal(result, expected_result, tolerance = 1e-8)
})

test_that("update_gp returns correct dimensions and values", {
  M <- 3
  L <- 1.0
  alpha <- 1.0
  rho <- 2.0
  eta <- rep(1, M)
  PHI <- matrix(runif(15), nrow = 5)  # 5 observations, 3 basis functions
  type <- 0
  nu <- 1.5 # Not used in SE case
  result <- update_gp(PHI, M, L, alpha, rho, eta, type, nu)
  expect_equal(length(result), nrow(PHI))  # Should match number of observations
  # Check specific values for known inputs
  diagSPD <- diagSPD_EQ(alpha, rho, L, M)
  expected_result <- PHI %*% (diagSPD * eta)
  expect_equal(matrix(result, ncol = 1), expected_result, tolerance = 1e-8)
})

test_that("update_gp with linear kernel returns correct dimensions and values", {
  M <- 3
  L <- 1.0
  alpha <- 1.0
  rho <- c(1.0)  # Not used for linear kernel
  eta <- rep(1, M)
  PHI <- matrix(runif(15), nrow = 5)  # 5 observations, 3 basis functions
  type <- 3  # Linear kernel
  nu <- 1.5  # Not used for linear kernel
  result <- update_gp(PHI, M, L, alpha, rho, eta, type, nu)
  expect_equal(length(result), nrow(PHI))  # Should match number of observations
  # Check specific values for known inputs
  diagSPD <- diagSPD_Linear(alpha, L, M)
  expected_result <- PHI %*% (diagSPD * eta)
  expect_equal(matrix(result, ncol = 1), expected_result, tolerance = 1e-8)
})

test_that("Linear kernel produces a linear GP", {
  N <- 100
  M <- 50
  L <- 10.0
  alpha <- 2.0
  x <- seq(-L, L, length.out = N)
  
  # Setup GP
  PHI <- PHI(N, M, L, x)
  
  # Generate random eta
  set.seed(123)
  eta <- rnorm(M)
  
  # Compute GP
  diagSPD <- diagSPD_Linear(alpha, L, M)
  gp <- PHI %*% (diagSPD * eta)
  
  # Fit a linear model
  lm_fit <- lm(gp ~ x)
  
  # Check if the GP is approximately linear
  expect_gt(summary(lm_fit)$r.squared, 0.99)  # R-squared should be very close to 1
  expect_lt(summary(lm_fit)$sigma, 0.1)  # Residual standard error should be small
})
