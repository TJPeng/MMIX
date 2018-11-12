######## Load Data ############


data_frame <- read.csv('C:/Users/xiey20/Documents/Marketing Science/GIO/Cross Product/Marketing Mix/02 Data/actemra_adstock_data_allzips_1y.csv')
samples_frame <- read.csv('C:/Users/xiey20/Documents/Marketing Science/GIO/Cross Product/Marketing Mix/02 Data/actemra_adstock_data_allzips_1y_samples.csv')
calls_frame <- read.csv('C:/Users/xiey20/Documents/Marketing Science/GIO/Cross Product/Marketing Mix/02 Data/actemra_adstock_data_allzips_1y_calls.csv')
sp_frame <- read.csv('C:/Users/xiey20/Documents/Marketing Science/GIO/Cross Product/Marketing Mix/02 Data/actemra_adstock_data_allzips_1y_sp.csv')
web_frame <- read.csv('C:/Users/xiey20/Documents/Marketing Science/GIO/Cross Product/Marketing Mix/02 Data/actemra_adstock_data_allzips_1y_web.csv')


########## Transformation and Adstock Functions #############

trf <- function(x,rate = 0.5){
  return(x^rate)
}

adstockk <- function(x, geo, rate=0){
  vec = c()
  df = data.frame(x,geo)
  for (i in unique(geo)){
    y = as.vector(df$x[df$geo == i])
    l = as.numeric(stats::filter(x=y, filter=rate, method="recursive"))
    vec = append(vec,l)
  }
  return(vec)
}


########## Find optimal transformation and adstock values ###########


### Calls ###

modcalls = nlsLM(ACT_VIALS_SA ~ b1 * adstockk(trf(ACTTOT_MD_CALLS,alpha1), ZIP3, ad1),
             data = calls_frame,
             algorithm = "port", control = nls.lm.control(maxfev = 1000, maxiter = 1000),
             start     = c(b1=1, alpha1=0.01,ad1 = 0),
             lower     = c(b1=-Inf, alpha1=-Inf,ad1 = -Inf),
             upper     = c(b1=Inf, alpha1=Inf,ad1 = Inf)
)

summary(modcalls)


### Plug in values found in model above

calls_adstock <- adstockk(trf(data_frame$ACTTOT_MD_CALLS,0.693469), data_frame$ZIP3, 0.639084)
mydata1 <- data.frame(data_frame, calls_adstock)


### Samples Model ###


modsamples = nlsLM(ACT_VIALS_SA ~ b1 * adstockk(trf(ACT_SAMPLES,alpha1), ZIP3, ad1),
                 data = samples_frame,
                 algorithm = "port", control = nls.lm.control(maxiter = 1000),
                 start     = c(b1=1, alpha1=0.01,ad1 = 0),
                 lower     = c(b1=-Inf, alpha1=-Inf,ad1 = -Inf),
                 upper     = c(b1=Inf, alpha1=Inf,ad1 = Inf)
)

summary(modsamples)


### Plug in values found in model above

samples_adstock <- adstockk(trf(data_frame$ACT_SAMPLES,0.665443), data_frame$ZIP3, 0.775374)
mydata2 <- data.frame(mydata1, samples_adstock)


### Speaker Programs Model ###

modsp = nlsLM(ACT_VIALS_SA ~ b1 * adstockk(trf(ACT_MD_BR_SP_ATTND,alpha1), ZIP3, ad1),
              data = sp_frame,
              algorithm = "port", control = nls.lm.control(maxiter = 1000),
              start     = c(b1=1, alpha1=0.01,ad1 = 0),
              lower     = c(b1=-Inf, alpha1=-Inf,ad1 = -Inf),
              upper     = c(b1=Inf, alpha1=Inf,ad1 = Inf)
)


summary(modsp)

### Plug in values found in model above

sp_adstock <- adstockk(trf(data_frame$ACT_MD_BR_SP_ATTND,0.241499), data_frame$ZIP3, 0.913537)
mydata3 <- data.frame(mydata2, sp_adstock)


### Web Model ###


modweb = nlsLM(ACT_VIALS_SA ~ b1 * adstockk(trf(ACT_TOT_VISITS,alpha1), ZIP3, ad1),
              data = web_frame,
              algorithm = "port", control = nls.lm.control(maxiter = 1000),
              start     = c(b1=1, alpha1=0.01,ad1 = 0),
              lower     = c(b1=-Inf, alpha1=-Inf,ad1 = -Inf),
              upper     = c(b1=Inf, alpha1=Inf,ad1 = Inf)
)

summary(modweb)


### Plug in values found in model above

web_adstock <- adstockk(trf(data_frame$ACT_TOT_VISITS,0.685246), data_frame$ZIP3, 0.137890)
mydata4 <- data.frame(mydata3, web_adstock)


######## Write Data #########

write.csv(mydata4, file='C:/Users/xiey20/Documents/Marketing Science/adstock_act_trf_1y_allzips_st.csv')
