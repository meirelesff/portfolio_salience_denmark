// Dynamic ordinal IRT, two-latent quantity model for portfolio salience

data {
  int<lower=1> J;                             // Number of portfolios
  int<lower=1> K;                             // Number of indicators
  int<lower=1> P;                             // Number of parties
  int<lower=1> N;                             // Number of observations
  int<lower=1> T;                             // Number of time periods
  int<lower=3> C;                             // Number of categories in response y
  array[N] int<lower=1, upper=J> jj;          // Portfolio for observation n
  array[N] int<lower=1, upper=K> kk;          // Indicator for observation n
  array[N] int<lower=1, upper=P> pp;          // Party for observation n
  array[N] int<lower=1, upper=T> tt;          // Timestamp for observation n
  array[N] int<lower=1, upper=C> y;           // Ordinal response for observation n
  array[N] int<lower=0, upper=1> occ;         // Portfolio occupation indicator
}

parameters {
  vector<lower=0>[K] beta;                  // Discrimination parameter for indicators
  array[K] ordered[C - 1] alpha;            // Difficulty parameters (one per indicator)
  matrix[T, J] theta_raw;                   // Raw latent portfolio salience
  matrix[P, J] lambda;                      // Latent party effect
  real gamma;                               // Occupation effect shift
  real<lower=0> sigma;                      // Innovation parameter
}

transformed parameters {
  matrix[T,J] theta;                   // Latent portfolio salience

  // Random-walk transformation
  for (t in 1:T) {
    if (t == 1) {
      theta[t, 1:J] = theta_raw[t, 1:J];
    } else {
      theta[t, 1:J] = theta[t - 1, 1:J] + theta_raw[t, 1:J] * sigma;
    }
  }
}

model {
  // Priors
  to_vector(theta_raw[1,]) ~ normal(0, 3);
  to_vector(theta_raw[2:T,]) ~ student_t(4, 0, 1);
  to_vector(lambda) ~ normal(0, 3);
  beta ~ normal(0, 3);
  gamma ~ normal(0, 2);
  sigma ~ normal(0, 1);

  // Measurement model with occupation adjustment
  for (n in 1:N) {
    real latent = theta[tt[n], jj[n]] + lambda[pp[n], jj[n]] + gamma * occ[n];
    y[n] ~ ordered_logistic(latent * beta[kk[n]], alpha[kk[n]]);
  }
}

generated quantities {
  array[P, T, J] real est;
  array[N] int<lower=1, upper=C> y_pred;

  // Latent portfolio salience by party (unconditional on occupation)
  for (p in 1:P) {
    for (t in 1:T) {
      for (j in 1:J) {
        est[p, t, j] = theta[t, j] + lambda[p, j];
      }
    }
  }

  // Predicted values including occupation effect
  for (n in 1:N) {
    real latent = theta[tt[n], jj[n]] + lambda[pp[n], jj[n]] + gamma * occ[n];
    y_pred[n] = ordered_logistic_rng(latent * beta[kk[n]], alpha[kk[n]]);
  }
}
