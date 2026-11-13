// Phase 6 first pass: minor clades partially pooled toward one global relationship.
// ln prey ~ Normal(mu, s), evaluated on exact sufficient statistics per cell
// (species x source x observation type x predator mass).
//   mu   = alpha0 + a[k] + u[j] + w[r] + (beta0 + b[k]) * m + delta[o]
//   ln s = gamma0 + g[k] + gamma1 * m + zeta[o]
// Observation type 1 (stomach_content) is the baseline.
data {
  int<lower=1> C;                       // cells
  int<lower=1> K;                       // minor clades
  int<lower=1> J;                       // species
  int<lower=1> R;                       // sources
  int<lower=2> O;                       // observation types
  vector<lower=1>[C] n;
  vector[C] ybar;
  vector<lower=0>[C] SS;
  vector[C] m;                          // ln predator mass - ln 1000 g
  array[C] int<lower=1, upper=K> kk;
  array[C] int<lower=1, upper=J> jj;
  array[C] int<lower=1, upper=R> rr;
  array[C] int<lower=1, upper=O> oo;
  int<lower=0, upper=1> prior_only;
}
parameters {
  real alpha0;
  real beta0;
  real gamma0;
  real gamma1;
  vector[O - 1] delta_raw;
  vector[O - 1] zeta_raw;
  vector[K] z_a;
  vector[K] z_b;
  vector[K] z_g;
  vector[J] z_u;
  vector[R] z_w;
  real<lower=0> tau_a;
  real<lower=0> tau_b;
  real<lower=0> tau_g;
  real<lower=0> tau_u;
  real<lower=0> tau_w;
}
transformed parameters {
  vector[K] a = tau_a * z_a;
  vector[K] b = tau_b * z_b;
  vector[K] g = tau_g * z_g;
  vector[O] delta = append_row(0, delta_raw);
  vector[O] zeta = append_row(0, zeta_raw);
}
model {
  alpha0 ~ normal(log(50), 2);
  beta0 ~ normal(1, 0.5);
  gamma0 ~ normal(0, 1);
  gamma1 ~ normal(0, 0.25);
  delta_raw ~ normal(0, 1);
  zeta_raw ~ normal(0, 0.5);
  z_a ~ std_normal();
  z_b ~ std_normal();
  z_g ~ std_normal();
  z_u ~ std_normal();
  z_w ~ std_normal();
  tau_a ~ normal(0, 1);
  tau_u ~ normal(0, 1);
  tau_w ~ normal(0, 1);
  tau_b ~ normal(0, 0.3);
  tau_g ~ normal(0, 0.5);
  if (prior_only == 0) {
    vector[C] mu = alpha0 + a[kk] + tau_u * z_u[jj] + tau_w * z_w[rr]
                   + (beta0 + b[kk]) .* m + delta[oo];
    vector[C] ls = gamma0 + g[kk] + gamma1 * m + zeta[oo];
    target += -dot_product(n, ls)
              - 0.5 * dot_product(SS + n .* square(ybar - mu), exp(-2 * ls));
  }
}
generated quantities {
  vector[K] clade_intercept = alpha0 + a;        // ln prey at 1 kg predator, stomach contents
  vector[K] clade_slope = beta0 + b;
  vector[K] clade_sigma = exp(gamma0 + g);       // residual sd at 1 kg, stomach contents
}
