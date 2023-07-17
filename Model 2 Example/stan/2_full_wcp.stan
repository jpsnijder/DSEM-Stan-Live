functions {
  real partial_sum_ll(array[] int slice, int start, int end,
                   int N_obs, vector gamma, array[] vector u, array[] vector alpha_u,
                   array[,] real X_imp, array[,] real Y_imp)
  {
    real ll = 0;
    for (i in slice) {
      // obtain individual means and regression parameters
      real mu_X = gamma[1] + u[i,1];
      real mu_Y = gamma[2] + u[i,2];
      
      real phi_X = gamma[3] + u[i,3];
      real phi_Y = gamma[4] + u[i,4];
      real phi_XY = gamma[5] + u[i,5];
      real beta_YX = gamma[6] + u[i,6];
      
      real psi_X = sqrt(exp(gamma[7] + u[i,7]));
      real psi_Y = sqrt(exp(gamma[8] + u[i,8]));
  
      for (t in 2:N_obs) {
        // calculate innovations
        real w_Xt = X_imp[i,t] - mu_X; 
        real w_Yt = Y_imp[i,t] - mu_Y; 
        real w_Xt_1 = X_imp[i,t-1] - mu_X; 
        real w_Yt_1 = Y_imp[i,t-1] - mu_Y; 
        ll += normal_lpdf(w_Xt| phi_X * w_Xt_1 + beta_YX * w_Yt, psi_X);
        ll += normal_lpdf(w_Yt| phi_Y * w_Yt_1 + phi_XY * w_Xt_1, psi_Y);
      }
      ll += std_normal_lpdf(alpha_u[i]);
    }
    return ll;
  }
}

data {
  int<lower=1> N_subj; // //subjects
  int<lower=1> N_obs; // //observations
  array[N_subj,N_obs] real X; 
  array[N_subj,N_obs] real Y; 
  // missing data: indicate number and indices, each vector contains
  // a position as (subject, observation)
  int<lower=0,upper=N_subj*N_obs> N_mis;
  array[N_mis,2] int ii_mis;
}
transformed data {
  // create subect vector for use in reduce_sum
  array[N_subj] int subjects;
  for (i in 1:N_subj) {
    subjects[i] = i;
  }
}
parameters {
  // between-level
  vector[8] gamma;
  array[N_subj] vector[8] alpha_u;  // random intercepts
  // for covariance matrices
  cholesky_factor_corr[8] L_Omega; // cholesky factor
  vector<lower=0>[8] tau_Omega;
  // parameters for modeling missing data
  array[N_mis] real X_mis;
  array[N_mis] real Y_mis;
}
transformed parameters {
  // construct covariance matrices from cholesky factors and standard deviations
  corr_matrix[8] R_Omega; // correlation matrix
  R_Omega = multiply_lower_tri_self_transpose(L_Omega); // R = L* L'

  array[N_subj] vector[8] u;
  for (i in 1:N_subj) {
    u[i] = tau_Omega .* (L_Omega * alpha_u[i]);
  }
}
model {
int grainsize = N_subj / 2;  // set grainsize as half the number of subjects
  
  // impute missing values into the matrix
  array[N_subj,N_obs] real X_imp = X; 
  array[N_subj,N_obs] real Y_imp = Y; 
  if (N_mis > 0) {
    for (k in 1:N_mis) {
      X_imp[ii_mis[k][1], ii_mis[k][2]] = X_mis[k]; 
      Y_imp[ii_mis[k][1], ii_mis[k][2]] = Y_mis[k]; 
    }
  }

  // priors (between-level)
  gamma ~ normal(0, 1e6);
  L_Omega ~ lkj_corr_cholesky(1.0);  // R_Omega ~ lkj_corr(1.0)
  tau_Omega ~ cauchy(0, 2.5);

  // priors (imputed data points)
  X_mis ~ normal(0, 10);
  Y_mis ~ normal(0, 10);

  // likelihood
  target += reduce_sum(partial_sum_ll, subjects, grainsize,
             N_obs, gamma, u, alpha_u, X_imp, Y_imp);
}
generated quantities {
  // quad_form_diag: diag_matrix(tau) * R * diag_matrix(tau)
  cov_matrix[8] Omega = quad_form_diag(R_Omega, tau_Omega);
}
