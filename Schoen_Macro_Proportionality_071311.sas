%include "G:\Lamb_KL_PERS\Consulting\Fine\Fine_Beta_031413\Schoen_Macro_Proportionality_Mayo_071311.sas";


%schoen  (time= t,
  event=e,
  xvars=  bb age pg female,
   data=beta,
   cen_vl=0,
  strata= ,
  outsch=schr,
  outbt=schbt,
  plot=t,   
  vref=yes,
  points=yes,
  pvars= ,
   method=spline,
  df=4,
  smooth= ,
  alpha=.05,
  rug=no,
  ties=efron);


/*

  Ken,

I found this easier to explain by writing out.  

One degree of freedom test-  
This test is used to assess whether a covariate’s relationship with the outcome variable is sufficiently modeled by a linear term, or whether a quadratic term significantly improves the fit.  However, the test is inconclusive if you had a nonsignificant linear term to begin with.  Here is an example from my recent data request.

I found that age was linearly related to the risk of death-censored pancreas graft failure, in that the older a person was, the lower their risk of pancreas graft failure (when death was not considered a cause of graft failure).  

Model 1:
proc tphreg data= test; 
      model patime_days*pafaildc(0)= age_at_Tx; 
run;

                                  Model Fit Statistics

                                              Without           With
                             Criterion     Covariates     Covariates

                             -2 LOG L       11243.995      11213.959


                          Parameter      Standard                                 Hazard
       Parameter    DF      Estimate         Error    Chi-Square    Pr > ChiSq       Ratio

       age_at_tx     1      -0.02551       0.00469       29.5811        <.0001       0.975


But I wondered whether a quadratic term would provide an even better fit.  To evaluate this, fit a model with both the linear and quadratic terms in it (Model 2).  Then, the first model is “nested” in the second (meaning all terms in the first are found in the second), and they can be compared using the one degree of freedom test.  

Model 2: 
proc tphreg data= test; 
      model patime_days*pafaildc(0)= age_at_Tx agesq; 
run;

                                          Model Fit Statistics

                                              Without           With
                             Criterion     Covariates     Covariates

                             -2 LOG L       11243.995      11212.977



                           Parameter      Standard                                  Hazard
       Parameter    DF      Estimate         Error    Chi-Square    Pr > ChiSq       Ratio

       age_at_tx     1      -0.06416       0.03882        2.7320        0.0984       0.938
       agesq         1     0.0004652     0.0004637        1.0065        0.3158       1.000


The test is constructed by comparing the difference in -2loglikelihood terms between models.  Adding terms to a model never increases the -2 log likelihood, and if they are predictive terms, the -2LL will actually decrease by a significant amount, showing an improvement in fit.  To see whether the decrease in -2LL is significant, you can compare to the Chi-square distribution, since the difference in -2LL between models follows a Chi-square distribution (based some theorem you don’t need to know… I don’t know it myself.)  

Test statistic= (-2logL from Model 1) – (-2logL from Model 2) , with degrees of freedom = difference in number of terms.  In this case, I had 2-1 = 1 df.    ?² = 11213.959 – 11212.977 = 0.98, df=1.  Compare this to a Chi-square distribution, and you should get a p-value of 0.32.  What that tells me is that a quadratic term does not significantly improve the model fit, and that based on Model 1, a linear term is sufficient to explain the relationship between age and the hazard of the outcome.  

If the test statistic had a p-value of less than 0.05, I would have eliminated the linear term from the model and kept the quadratic term in, with the explanation that the relationship between age and the outcome was better modeled with a quadratic than a linear term.  

However, if the relationship between linear age and the outcome had not been significant to start with, it is much harder to draw conclusions.  If a quadratic term doesn’t significantly improve a bad fit, then you only know that neither of those terms works as a parameterization.  If a quadratic term DOES significantly improve a bad linear fit, I still wouldn’t take that as evidence that the relationship is quadratic.  Fit the quadratic covariate alone to see if it is a significant predictor, and investigate further by fitting a categorical parameterization to get confirmation of this.   

Actually, you can also compare a linear versus cubic fit, or a quadratic versus cubic.  As long as the two models you compare are nested, it is fine.  There would still be one degree of freedom difference between the two models.  However, a cubic term is pretty confusing for anyone to really understand the meaning of, so if I were to find that a cubic term was a significant improvement, I would probably prefer to fit the covariate as a categorical variable instead.  

Sally

From: Kenneth Lamb 
Sent: Friday, July 08, 2011 3:03 PM
To: Sally Gustafson
Subject: RE: schoen macro

Thank you Sally!

From: Sally Gustafson 
Sent: Friday, July 08, 2011 3:02 PM
To: Kenneth Lamb
Subject: schoen macro

http://mayoresearch.mayo.edu/mayo/research/biostat/sasmacros.cfm

Set up a dataset with a time variable, event variable, predictors.  No phreg necessary.



%schoen  (time= patime_days,
  event=pafail ,
  xvars=  young pdri local type1,
   data=test,
   cen_vl=0,
  strata= ,
  outsch=schr,
  outbt=schbt,
  plot=t,   ** plot = r, or t
  vref=yes,
  points=yes,
  pvars= ,
   method=spline,
  df=4,
  smooth= ,
  alpha=.05,
  rug=no,
  ties=efron);
