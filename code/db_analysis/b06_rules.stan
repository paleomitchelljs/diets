// b06: the b04 skeleton-tree model with switches for the pooling rule.
// Same likelihood (exact sufficient statistics per cell) and same priors as b04.
// For each clade effect e in {intercept a, slope b, spread g}:
//     e = tau_e * chol(lambda_e * Ctree + (1 - lambda_e) * I) * z_e
// Rules (set by data):
//   free      est_tau = 1, est_lambda = 1, est_global = 1   (= b04)
//   lambda=0  est_tau = 1, est_lambda = 0, lambda_fixed = 0 (clades exchangeable; = b02)
//   lambda=1  est_tau = 1, est_lambda = 0, lambda_fixed = 1 (Brownian motion on the skeleton)
//   unpooled  est_tau = 0, tau_fixed = (2, 0.5, 1), est_global = 0, lambda_fixed = 0:
//             each clade's line gets the global prior on its own, nothing shared
//   pooled    est_tau = 0, tau_fixed = (0, 0, 0): one line for all tetrapods
// Species, source and observation-type terms are the same in every rule.
data {
  int<lower=1> C;
  int<lower=1> K;
  int<lower=1> J;
  int<lower=1> R;
  int<lower=2> O;
  vector<lower=1>[C] n;
  vector[C] ybar;
  vector<lower=0>[C] SS;
  vector[C] m;
  array[C] int<lower=1, upper=K> kk;
  array[C] int<lower=1, upper=J> jj;
  array[C] int<lower=1, upper=R> rr;
  array[C] int<lower=1, upper=O> oo;
  matrix[K, K] Ctree;
  int<lower=0, upper=1> est_tau;
  vector<lower=0>[3] tau_fixed;
  int<lower=0, upper=1> est_lambda;
  vector<lower=0, upper=1>[3] lambda_fixed;
  int<lower=0, upper=1> est_global;
  vector[3] global_fixed;               // alpha0, beta0, gamma0 when est_global = 0
  int<lower=0, upper=1> prior_only;
}
transformed data {
  matrix[K, K] I_K = identity_matrix(K);
  array[3] matrix[K, K] L_fixed;
  for (e in 1:3) L_fixed[e] = cholesky_decompose(lambda_fixed[e] * Ctree + (1 - lambda_fixed[e]) * I_K);
}
parameters {
  array[est_global] real alpha0_p;
  array[est_global] real beta0_p;
  array[est_global] real gamma0_p;
  real gamma1;
  vector[O - 1] delta_raw;
  vector[O - 1] zeta_raw;
  vector[K] z_a;
  vector[K] z_b;
  vector[K] z_g;
  vector[J] z_u;
  vector[R] z_w;
  array[est_tau] real<lower=0> tau_a_p;
  array[est_tau] real<lower=0> tau_b_p;
  array[est_tau] real<lower=0> tau_g_p;
  real<lower=0> tau_u;
  real<lower=0> tau_w;
  array[est_lambda] real<lower=0, upper=1> lambda_a_p;
  array[est_lambda] real<lower=0, upper=1> lambda_b_p;
  array[est_lambda] real<lower=0, upper=1> lambda_g_p;
}
transformed parameters {
  real alpha0 = est_global ? alpha0_p[1] : global_fixed[1];
  real beta0  = est_global ? beta0_p[1]  : global_fixed[2];
  real gamma0 = est_global ? gamma0_p[1] : global_fixed[3];
  real tau_a = est_tau ? tau_a_p[1] : tau_fixed[1];
  real tau_b = est_tau ? tau_b_p[1] : tau_fixed[2];
  real tau_g = est_tau ? tau_g_p[1] : tau_fixed[3];
  real lambda_a = est_lambda ? lambda_a_p[1] : lambda_fixed[1];
  real lambda_b = est_lambda ? lambda_b_p[1] : lambda_fixed[2];
  real lambda_g = est_lambda ? lambda_g_p[1] : lambda_fixed[3];
  vector[K] a;
  vector[K] b;
  vector[K] g;
  if (est_lambda) {
    a = tau_a * (cholesky_decompose(lambda_a * Ctree + (1 - lambda_a) * I_K) * z_a);
    b = tau_b * (cholesky_decompose(lambda_b * Ctree + (1 - lambda_b) * I_K) * z_b);
    g = tau_g * (cholesky_decompose(lambda_g * Ctree + (1 - lambda_g) * I_K) * z_g);
  } else {
    a = tau_a * (L_fixed[1] * z_a);
    b = tau_b * (L_fixed[2] * z_b);
    g = tau_g * (L_fixed[3] * z_g);
  }
  vector[O] delta = append_row(0, delta_raw);
  vector[O] zeta = append_row(0, zeta_raw);
}
model {
  if (est_global) {
    alpha0_p ~ normal(log(50), 2);
    beta0_p ~ normal(1, 0.5);
    gamma0_p ~ normal(0, 1);
  }
  gamma1 ~ normal(0, 0.25);
  delta_raw ~ normal(0, 1);
  zeta_raw ~ normal(0, 0.5);
  z_a ~ std_normal();
  z_b ~ std_normal();
  z_g ~ std_normal();
  z_u ~ std_normal();
  z_w ~ std_normal();
  if (est_tau) {
    tau_a_p ~ normal(0, 1);
    tau_b_p ~ normal(0, 0.3);
    tau_g_p ~ normal(0, 0.5);
  }
  tau_u ~ normal(0, 1);
  tau_w ~ normal(0, 1);
  if (prior_only == 0) {
    vector[C] mu = alpha0 + a[kk] + tau_u * z_u[jj] + tau_w * z_w[rr]
                   + (beta0 + b[kk]) .* m + delta[oo];
    vector[C] ls = gamma0 + g[kk] + gamma1 * m + zeta[oo];
    target += -dot_product(n, ls)
              - 0.5 * dot_product(SS + n .* square(ybar - mu), exp(-2 * ls));
  }
}
generated quantities {
  vector[K] clade_intercept = alpha0 + a;
  vector[K] clade_slope = beta0 + b;
  vector[K] clade_sigma = exp(gamma0 + g);
}
