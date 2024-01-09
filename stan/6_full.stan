data {
  int<lower=1> N_subj; // //subjects
  int<lower=1> N_obs; // //observations
  array[N_subj,N_obs] real X; 
  array[N_subj,N_obs] real Y; 
  array[N_subj] real P;
  array[N_subj] real O;
  // missing data: indicate number and indices, each vector contains
  // a position as (subject, observation, x_pos)
  int<lower=0,upper=N_subj*N_obs> N_mis_X; //changed
  int<lower=0,upper=N_subj*N_obs> N_mis_Y; //changed
  array[N_mis_X,2] int ii_mis_X; //changed
  array[N_mis_Y,2] int ii_mis_Y; //changed
}

parameters {
  // between-level
  vector[8] gamma;
  array[N_subj] vector[8] alpha_u;  // residuals
  vector<lower=0>[8] tau_Omega;
  vector[N_subj] C_;
  // real<lower=0> sigma_C;
  real<lower=0> sigma_O;
  vector[4] tau_;
  vector[8] a_;
  // parameters for modeling missing data
  array[N_mis_X] real X_mis;
  array[N_mis_Y] real Y_mis;
}

transformed parameters {
  array[N_subj] vector[8] u;
  for (i in 1:N_subj) {
    u[i] = tau_Omega .* alpha_u[i];
  }
}

model {
  
  // impute missing values into the matrix
  array[N_subj,N_obs] real X_imp = X; 
  array[N_subj,N_obs] real Y_imp = Y; 
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

  // priors (between-level)
  gamma ~ normal(0, 100000);
  tau_Omega ~ normal(0, 100000);
  // a_ ~ normal(0, 100000);
  // sigma_C ~ normal(0, 100000);

  // priors (imputed data points)
  X_mis ~ normal(0, 10);
  Y_mis ~ normal(0, 10);

  // likelihood
  for (i in 1:N_subj) {
    // obtain individual means and regression parameters
    real mu_X = gamma[1] + a_[1] * C_[i] + u[i,1];
    real mu_Y = gamma[2] + a_[2] * C_[i] + u[i,2];
    
    real phi_X = gamma[3] + a_[3] * C_[i] + u[i,3];
    real phi_Y = gamma[4] + a_[4] * C_[i] + u[i,4];
    real phi_XY = gamma[5] + a_[5] * C_[i] + u[i,5];
    real beta_YX = gamma[6] + a_[6] * C_[i] + u[i,6];
    
    real psi_X = sqrt(exp(gamma[7] + a_[7] * C_[i] + u[i,7]));
    real psi_Y = sqrt(exp(gamma[8] + a_[8] * C_[i] + u[i,8]));

    for (t in 2:N_obs) {
      // calculate innovations
      real w_Xt = X_imp[i,t] - mu_X; 
      real w_Yt = Y_imp[i,t] - mu_Y; 
      real w_Xt_1 = X_imp[i,t-1] - mu_X; 
      real w_Yt_1 = Y_imp[i,t-1] - mu_Y; 
      // within-person model
      w_Xt ~ normal(phi_X * w_Xt_1 + beta_YX * w_Yt, psi_X);
      w_Yt ~ normal(phi_Y * w_Yt_1 + phi_XY * w_Xt_1, psi_Y);
    }
    alpha_u[i] ~ std_normal();
    O[i] ~ normal(
      tau_[1] + tau_[2] * P[i] + tau_[3] * C_[i],
      sigma_O
    );
    C_[i] ~ normal(tau_[4] * P[i], 1);
  }
}

generated quantities {
  vector[8] a = a_;
  vector[N_subj] C = C_;
  vector[4] tau = tau_;
  
  if (a_[1] < 0) {
    a = -a;
    C = -C;
    tau[3] = -tau_[3];
    tau[4] = -tau_[4];
  }
}
