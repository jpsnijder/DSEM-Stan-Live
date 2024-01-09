data {
  int<lower=1> N_subj; // subjects
  int<lower=1> N_obs; //  observations
  array[N_subj] vector[N_obs] X, Y; //! changed from _obs
  // missing data: indicate number and indices, each vector contains
  // a position as (subject, observation, x_pos)
  int<lower=0,upper=N_subj*N_obs> N_mis_X; // changed
  int<lower=0,upper=N_subj*N_obs> N_mis_Y; // changed
  array[N_mis_X,2] int ii_mis_X; // changed
  array[N_mis_Y,2] int ii_mis_Y; // changed
}
parameters {
  // within-level
  real phi_X;
  real phi_Y;
  real phi_XY;
  real beta_YX;
  real<lower=0> psi_X;
  real<lower=0> psi_Y;
  
  // between-level
  vector[2] gamma;
  array[N_subj] vector[2] alpha_u;  // random intercepts
  // for covariance matrices
  cholesky_factor_corr[2] L_Omega; // cholesky factor
  vector<lower=0>[2] tau_Omega;
  // parameters for modeling missing data
  array[N_mis_X] real X_mis;
  array[N_mis_Y] real Y_mis;
}
transformed parameters {
  // construct covariance matrices from cholesky factors and standard deviations
  corr_matrix[2] R_Omega; // correlation matrix
  R_Omega = multiply_lower_tri_self_transpose(L_Omega); // R = L* L'

  array[N_subj] vector[2] u;
  for (i in 1:N_subj) {
    u[i] = tau_Omega .* (L_Omega * alpha_u[i]);
  }
}
model {
  
  // impute missing values into the matrix
  
  array[N_subj] vector[N_obs] X_imp = X; 
  array[N_subj] vector[N_obs] Y_imp = Y; 
  
  // array[N_subj,N_obs] real X_imp = X; 
  // array[N_subj,N_obs] real Y_imp = Y; 
  if (N_mis_X > 0) {
    for (k in 1:N_mis_X) {
      X_imp[ii_mis_X[k][1], ii_mis_X[k][2]] = X_mis[k]; 
    }
  }
  
    if (N_mis_Y > 0) {
    for (k in 1:N_mis_Y) {
      Y_imp[ii_mis_Y[k][1], ii_mis_Y[k][2]] = Y_mis[k]; 
    }
  }
  
  // priors (within-level)
  phi_X ~ normal(0, 1e6);
  phi_Y ~ normal(0, 1e6);
  phi_XY ~ normal(0, 1e6);
  beta_YX ~ normal(0, 1e6);
  
  psi_X ~ normal(0, 1e6);
  psi_Y ~ normal(0, 1e6);

  // priors (between-level)
  gamma ~ normal(0, 1e6);
  L_Omega ~ lkj_corr_cholesky(1.0);  // R_Omega ~ lkj_corr(1.0)
  tau_Omega ~ cauchy(0, 2.5);

  // priors (imputed data points)
  X_mis ~ normal(0, 10);
  Y_mis ~ normal(0, 10);

  // likelihood
  for (i in 1:N_subj) {
    // between-level model
    real mu_X = gamma[1] + u[i,1];
    real mu_Y = gamma[2] + u[i,2];
    alpha_u[i] ~ std_normal();
    // calculate within-level values by subtracting the means
    vector[N_obs] X_w = X_imp[i] - mu_X;
    vector[N_obs] Y_w = Y_imp[i] - mu_Y;

    for (t in 2:N_obs) {
      // within-level model
      X_w[t] ~ normal(phi_X * X_w[t-1] + beta_YX * Y_w[t], psi_X);
      Y_w[t] ~ normal(phi_Y * Y_w[t-1] + phi_XY * X_w[t-1], psi_Y);
    }
  }
}
generated quantities {
  // quad_form_diag: diag_matrix(tau) * R * diag_matrix(tau)
  cov_matrix[2] Omega = quad_form_diag(R_Omega, tau_Omega);
}
