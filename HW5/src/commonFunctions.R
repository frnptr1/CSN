

saveNodesDegreeOnFile <- function(t, k, baseName) {
    
    filename = paste(baseName, sprintf("%05d", t), ".csv", sep = "")
    cat("Saving degree sequence", filename, "\n")
    write.csv(k, file=filename, row.names = FALSE)
    cat("Done.\n")
}

saveNodesSequenceOnFile <- function(t, k, baseName) {
    
    filename = paste(baseName, sprintf("%05d", t), ".csv", sep = "")
    cat("Saving degree sequence", filename, "\n")
    write.csv(k, file=filename, row.names = FALSE)
    cat("Done.\n")
}


plotVertexEvolution <- function(table, x, vid) {
    
    plot <- ggplot(data = table) +
        aes(x = sequ, y = table[, x]) +
        geom_point(color = '#0c4c8a') +
        labs(title = paste('Vertex',vid, 'degree evolution'),
             x = 't',
             y = 'k') +
        theme_calc()
    
    return(plot)
}


plotAllEvolutions <- function(table, title){
    p1 <- plotVertexEvolution(table, 2, 1)
    p2 <- plotVertexEvolution(table, 3, 10)
    p3 <- plotVertexEvolution(table, 4, 100)
    p4 <- plotVertexEvolution(table, 5, 1000)
    grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2, top=title)
}


loadModelExecutions <- function(files) {
    
    ks.1 = read.csv2(files[1]); ks.1 = ks.1$x
    ks.2 = read.csv2(files[2]); ks.2 = ks.2$x
    ks.3 = read.csv2(files[3]); ks.3 = ks.3$x
    ks.4 = read.csv2(files[4]); ks.4 = ks.4$x
    
    table = data.frame(sequ = seq(length(ks.1)), ks.1, ks.2, ks.3, ks.4)
    return(table)
}


# get the -2LogLikelihood of a function
get_2LL = function(x) {
   
   attributes(summary(x))$m2logL
   
}


# get the MLE of a parameter
get_estimate = function(x) {
   
   attributes(summary(x))$coef[1]

}


# C constant of displaced Poisson
logf <- function(i) {
   
   sum(log(seq(1, i)))
   
}


# minus log-likelihood of the displaced poisson function
minus_log_likelihood_poiss = function(lambda, M, N, C) {
   
   -(M*log(lambda)
     -N*(lambda+log(1-exp(-lambda)))
     -C)
   
}


# minus log-likelihood of displaced geometric distribution
minus_log_likelihood_geom <- function(q, N, M) {
   
   -N*log(q)-(M-N)*log(1-q)
   
}


# minus log-likelihood of zeta function
minus_log_likelihood_zeta <- function(gamma, M_prime, N) {
   
   N * log(zeta(gamma)) + gamma * M_prime
   
}


# hmax function - harmonic number function
hmax = function(kmax, gamma) {
   
   k_list = seq(1,kmax)
   out = sum(k_list^(-gamma))
   return(out)
   
}

# minus log-likelihood of zeta right truncated function
minus_log_likelihood_rt_zeta <- function(gamma, kmax, M_prime, N) {
   
   gamma*M_prime + N*hmax(kmax, gamma)
   
}


# AIC EVALUATION
# function to compute the Akaike Information Criterion
get_AIC <- function(m2logL,K,N) {
   
   a = m2logL + 2*K*N/(N-K-1)# AIC with a correction for sample size
   return(a)
   
}




model_selection_degree_distribution = function(x){
   
   M = sum(x)
   M_prime = sum(log(x))
   N = length(x)
   C = 0
   for(j in 1:length(x)){
      C = C+logf(x[j])
   }
   lower_k = max(x)
   
   
   #MLE POISSON
   mle_pois <- mle(minus_log_likelihood_poiss,
                   start = list(lambda = M/N),
                   fixed = list(M=M, N=N, C=C),
                   method = "L-BFGS-B",
                   lower = c(1.0000001))
   
   
   # MLE GEOM
   mle_geom <- mle(minus_log_likelihood_geom,
                   start = list(q=N/M),
                   fixed = list(N=N, M=M),
                   method = "L-BFGS-B",
                   lower = c(0.0000001),
                   upper = c(0.9999999))
   
   
   # MLE ZETA
   mle_zeta <- mle(minus_log_likelihood_zeta,
                   start = list(gamma = 2),
                   fixed = list(M_prime = M_prime, N=N),
                   method = "L-BFGS-B",
                   lower = c(1.0000001))
   
   
   # MLE RT ZETA
   gamma_opt = mle(minus_log_likelihood_rt_zeta,
                   start = list(gamma = 1.00001),
                   fixed = list(kmax = lower_k, M_prime = M_prime, N=N),
                   method = "L-BFGS-B",
                   lower = c(1.00001))
   
   mle_zeta_rt = mle(minus_log_likelihood_rt_zeta,
                     start = list(kmax = lower_k),
                     fixed = list(gamma = get_estimate(gamma_opt), M_prime = M_prime, N=N),
                     method = "L-BFGS-B",
                     lower = c(lower_k))
   
   
   
 # list of objects mle
   mle_for_params = list(mle_pois, mle_geom, mle_zeta, gamma_opt, mle_zeta_rt)
   
   # parameters
   params_vector = sapply(mle_for_params, get_estimate)
   params_matrix = matrix( c(params_vector[1], NA, 
                             params_vector[2], NA,
                             params_vector[3], NA,
                             params_vector[4], params_vector[5]),
                           nrow = 4, byrow = TRUE)
   
   mle_for_2ll = list(mle_pois, mle_geom, mle_zeta, mle_zeta_rt)
   # -2logLikelihood of each function
   L = sapply(mle_for_2ll, get_2LL)
   
   # number of parameters
   K = c(1,1,1,2)
   
   
   # AIC value for each distribution
   AIC_vect = mapply(AIC, mle_for_2ll)
   
   
   df = data.frame("Model" = c("Poisson", "Geometric", "Zeta", "RT Zeta"),
                   "2LL" = L,
                   "AIC" = AIC_vect,
                   "Param1" = params_matrix[,1],
                   "Param2" = params_matrix[,2])
   return(df)
   
}


model_selection_vertex_growth = function(i) {
   
   # parameters of each model
   mod_params = c(1, 1, 2, 2, 2, # without intercept
                  2, 2, 3, 3, 3) # with intercept
   
   # models without intercept
   model0 = nlsLM(i ~ a*t, start=list(a=1))
   model1 = nlsLM(i ~ a*sqrt(t),start=list(a=1))
   model2 = nlsLM(i ~ a*(t)^b, start = list(a=0.1, b=1))
   model3 = nlsLM(i ~ a*exp(c*t), start = list(a=1,c=0.0001))
   model4 = nlsLM(i ~ a*(log(abs(t+d1))), start=list(a=0.1 ,d1=1))
   
   # models with intercept
   model0i = nlsLM(i ~ a*t+d,start=list(a=1, d=1))
   model1i = nlsLM(i ~ a*sqrt(t)+d,start=list(a=1, d=1))
   model2i = nlsLM(i ~ a*((t)^b)+d, start = list(a=0.1, b=1, d=1), control=nls.lm.control(maxiter = 150))
   model3i = nlsLM(i ~ d+a*exp(c*t), start = list(a=1,c=0.01, d=0.1))
   model4i = nlsLM(i ~ a*(log(abs(t+d1)))+d2, start=list(a=10 ,d1=1000, d2=-100))
   
   # the models computer are now inserted in a list to compact the code
   models_list = list(model0, model1, model2, model3, model4,
                      model0i, model1i, model2i, model3i, model4i)
   
   get_RSS = function(x) return(sum(residuals(x)^2)) #check
   get_AIC = function(rss, par) return(t.max*log(2*pi) + t.max*log(rss/t.max) + t.max + 2*(par + 1))
   get_params = function(x) return(as.vector(coef(x)))
   
   # get parameter estimate from models..
   pm = lapply(models_list, get_params)
   # replace missing parameters with NA (some models have 1 params others 2, others 3)..
   pm = lapply(pm, `length<-`, max(lengths(pm)))
   # transform it into matrix to append in the dataframe
   pm = do.call(rbind, pm)
   
   # residual sum of squares
   rss = sapply(models_list, get_RSS)
   
   # Akaike information criterion
   aic_vect = mapply(get_AIC,rss, mod_params)
   
   model_name = c("linear regr", "plaw 0.5", "plaw", "expo", "log",
                  "linear regr+i", "plaw 0.5+i", "plaw+i", "expo+i", "log+i")
   # model selection growth + preferential attachment 
   ms_gp = data.frame("Model" = model_name,
                      "RSS"= rss,      # residual square of sum no intercept
                      "AIC"=aic_vect,  # aic no intercept
                      "Param1"=pm[,1],          # param 1
                      "Param2"=pm[,2],          # param 2
                      "Param3"=pm[,3])          # param 
   # add model names to df
   
   # show the one that fit better
   better_fit = ms_gp[which.min(ms_gp$AIC),]
   
}


plot_ki = function(x, y, z){
  
   # it depends on which graph we are plotting the theoretical distribution changes
   if (z=="gp"){
       
       plot(x[,2],main =  "Growth + Preferential attachment \n Vertices degree & Theoretical k(t) evolution",xlab = "t", ylab = "k(t)", type = "l",lwd = 2, xlim = c(1000,t.max))
       lines(x[,3], col="red", lwd = 2)
       lines(x[,4], col = "green", lwd = 2)
       lines(x[,5], col = "blue", lwd = 2)
       lines(m.0*sqrt(x[, 1]), col="orchid", lwd = 2)
       grid()
       
   }else if(z=="gr"){
       
       res = m.0*log(m.0+x[,1]-1)
       idx = length(res) - 1
       plot(x[,2],main = "Growth + Random attachment \n Vertices degree & Theoretical k(t) evolution", xlab = "t", ylab = "k(t)", type = "l",lwd = 2, xlim = c(1000,t.max), ylim = c(0, res[idx]))
       lines(x[,3], col="red", lwd = 2)
       lines(x[,4], col = "green", lwd = 2)
       lines(x[,5], col = "blue", lwd = 2)
       lines(m.0*log(m.0+x[,1]-1), col="orchid", lwd = 2)
       grid()
       
   }else if(z=="ngp"){
       
       n.0 = 2000
       res =((2*m.0)/(n.0))*x[, 1]
       idx = length(res) - 1
       plot(x[,2],main = "NO Growth + Preferential attachment \n Vertices degree & Theoretical k(t) evolution", xlab = "t", ylab = "k(t)", type = "l",lwd = 2, xlim = c(0,t.max), ylim = c(0, res[idx]))
       lines(x[,3], col="red", lwd = 2)
       lines(x[,4], col = "green", lwd = 2)
       lines(x[,5], col = "blue", lwd = 2)
       lines(res, col="orchid", lwd = 2)
       grid()
   }
      
   legend(y, legend = c("k1(t)", "k10(t)", "k100(t)", "k1000(t)", "theoretical k(t)"),
          lty = 1, lwd = 2,col = c("black", "red", "green", "blue", "orchid"))
}


