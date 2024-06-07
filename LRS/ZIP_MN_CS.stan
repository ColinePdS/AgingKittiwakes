data{
    int<lower=0> N;
    vector[N] x;
    int y[N];
    vector[N] Sex;
    

}

parameters{
    real alpha;
    real beta;
    real alpha_2;
    real beta_2;

    real <lower = 0, upper = 1> theta;
    


}

transformed parameters{

    vector[N] lambda;
    
    for (i in 1:N){

        lambda[i] = exp(alpha + alpha_2*Sex[i] + beta*x[i] + (beta_2 * Sex[i] * x[i] ));
 
    }
}

model{
    alpha ~normal(0,10);
    beta~normal(0,10);
    alpha_2 ~normal(0,10);
    beta_2~normal(0,10);

    theta~normal(0,10);

    
   
    for (n in 1:N) {

        
        if (y[n] == 0)
        target += log_sum_exp(bernoulli_lpmf(1 | theta),
                                bernoulli_lpmf(0 | theta)
                                + poisson_lpmf(y[n] | lambda[n]));
        else
        target += bernoulli_lpmf(0 | theta)
                    + poisson_lpmf(y[n] | lambda[n]);
  }
}


generated quantities{

 real log_lik[N];
  
  for(n in 1:N){
	     if(y[n] == 0)
	      log_lik[n] = log_sum_exp(bernoulli_lpmf(1 | theta),  // IS THIS RIGHT?
                            bernoulli_lpmf(0 | theta)
                              + poisson_lpmf(y[n] | lambda[n])) ;
       else
        log_lik[n] = bernoulli_lpmf(0 | theta)
                      + poisson_lpmf(y[n] | lambda[n]);
  }
}

