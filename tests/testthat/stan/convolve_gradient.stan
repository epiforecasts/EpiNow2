// Test model comparing the C++ convolve_with_rev_pmf() with the pure Stan
// reference. x and the reversed pmf y are each either parameters or data,
// so every var/double combination is reached.
functions {
#include functions/convolve.stan
#include convolve_reference.stan

  vector conv(vector x, vector y, int len, int use_cpp) {
    if (use_cpp) {
      return convolve_with_rev_pmf(x, y, len);
    }
    return convolve_with_rev_pmf_stan(x, y, len);
  }
}

data {
  int n;
  int D;
  int len;
  vector[n] x_data;
  vector[D] y_data;
  vector[len] r;
  int<lower = 0, upper = 1> x_param;
  int<lower = 0, upper = 1> y_param;
  int<lower = 0, upper = 1> use_cpp;
}

parameters {
  vector[x_param ? n : 0] x_par;
  vector[y_param ? D : 0] y_par;
}

transformed parameters {
  vector[len] z;
  if (x_param && y_param) {
    z = conv(x_par, y_par, len, use_cpp);
  } else if (x_param) {
    z = conv(x_par, y_data, len, use_cpp);
  } else if (y_param) {
    z = conv(x_data, y_par, len, use_cpp);
  } else {
    z = conv(x_data, y_data, len, use_cpp);
  }
}

model {
  target += dot_product(r, z) - 0.5 * dot_self(z);
}

generated quantities {
  vector[len] z_data = conv(x_data, y_data, len, use_cpp);
}
