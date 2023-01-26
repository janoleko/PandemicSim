#' Simulate a pandemic
#'
#' With this function you can simulate a pandemic for a population of dots living in a small box.
#' For simplicitely they move completly random. You have a lot of options to change the pandemic.
#' For example you can change the number of individuals `K`or increase the population's `mobility`. You can also make the disease more infectious or more deadly with `inf_prob` and `deadfactor`.
#' You can change how fast individuals recover with `recprob` and you can even simulate the effect of countermeasures with `measures`, `mobility_reduction` and `inf_prob_reduction`.
#'
#' @param n Number of time points to simulate
#' @param K Number of individuals living in the box
#' @param inf0 Number of infected individuals at t = 0
#' @param mobility A value determining the mobility of the population
#' @param radius A value determining the radius in which infections can happen
#' @param inf_prob Probability that an individual gets infected when in proximity of an infected individual
#' @param recprob Probability to recover if infected (corresponds to a geometric distribution for recover time)
#' @param resusprob Probability to become susceptible again after infection and recovery (corresponds to a geometric distribution for the time to become susceptible again)
#' @param deadfactor Expected proportion of infected individuals that die
#' @param measures Proportion of infected population at which to start countermeasures against the pandemic
#' @param mobility_reduction Mobility reduction in percent if measures are active
#' @param inf_prob_reduction Reduction of the infection probability in percent if measures are active (proxy for e.g. wearing masks)
#'
#' @return Returns a pandemic object that can be used for plotting the pandemic with the `plot_pandemic()` function.
#' @export
#'
#' @examples
#'
#' ## Example 1: Simulating a pandemic with default values
#'
#' pandemic = sim_pandemic()
#'
#' ## Example 2: Changing values
#' # more aggressive pandemic with stronger countermeasures
#' pandemic = sim_pandemic(mobility = 0.5, deadfactor = 0.01, measures = 0.8, mobility_reduction = 0.5)
sim_pandemic = function(n = 500, K = 500, inf0 = 1 , mobility = 0.3, radius = 0.2,
                        inf_prob = 0.7, recprob = 0.1, resusprob = 0.05, deadfactor = 0.005,
                        measures = 0.8, mobility_reduction = 0.26, inf_prob_reduction = 0.4){
  steplength = mobility
  sigma = 0.25
  epsilon = radius
  infprob = inf_prob

  X = list()
  x = matrix(NA, K, 3)
  x[,3] = rep(1,K)
  x[sample(1:K, inf0), 3] = 2
  I = numeric(n)
  S = numeric(n)
  S[1] = K
  R = numeric(n)
  D = numeric(n)


  # Initializing the dots
  for (k in 1:K){
    x[k,1:2] = stats::runif(2,-5,5)
    X[[1]] = x
  }

  # creating progress bar
  pb = utils::txtProgressBar(min = 2, max = n, initial = 0, style = 3)

  cat("Simulation started\n")

  for (i in 2:n){
    # Movement
    step = stats::rgamma(K,shape = steplength^2/sigma^2, scale = sigma^2/steplength)
    angle = stats::runif(K,-pi,pi)
    x = X[[i-1]]
    aliveind = which(x[,3] != 4)
    x[aliveind,1] = x[aliveind,1] + step[aliveind]*cos(angle[aliveind])
    x[aliveind,2] = x[aliveind,2] + step[aliveind]*sin(angle[aliveind])
    X[[i]] = x
    for (k in 1:K){
      if ((X[[i-1]][k,1] < 5 & X[[i]][k,1] > 5) | (X[[i-1]][k,1] > -5 & X[[i]][k,1] < -5) |
          (X[[i-1]][k,2] > -5 & X[[i]][k,2] < -5) | (X[[i-1]][k,2] < 5 & X[[i]][k,2] > 5)){
        X[[i]][k,] = X[[i-1]][k,]
      }
    }

    # Counting
    inf = which(x[,3] == 2)
    sus = which(x[,3] == 1)
    rec = which(x[,3] == 3)

    # Infections
    if (length(sus) > 0 & length(inf) > 0){
      for (k in 1:length(inf)){
        for (j in 1:length(sus)){
          r = (x[inf[k],1:2] - x[sus[j],1:2])
          d = sqrt(t(r)%*%r)

          if (d < epsilon){
            if (stats::rbinom(1,1,infprob) == 1){
              x[sus[j],3] = 2
            }
            else {x[sus[j],3] = 1}
          }
        }
      }
    }

    # Deads
    inf = which(x[,3] == 2)
    if (length(inf > 0)){
      dead = stats::rbinom(length(inf),1,deadfactor*(length(inf)/K))
      for (k in 1:length(inf)){
        if (dead[k] == 1){
          x[inf[k],3] = 4
        }
        else{
          x[inf[k],3] = 2
        }
      }
    }

    # Recoveries
    inf = which(x[,3] == 2)
    if (length(inf > 0)){
      recover = stats::rbinom(length(inf), 1, recprob)
      for (k in 1:length(inf)){
        if (recover[k] == 1){
          x[inf[k],3] = 3
        }
        else{
          x[inf[k],3] = 2
        }
      }
    }

    # Becoming Susceptible again
    rec = which(x[,3] == 3)
    if (length(rec) > 0){
      resus = stats::rbinom(length(rec), 1, resusprob)
      for (k in 1:length(rec)){
        if(resus[k] == 1){
          x[rec[k],3] = 1
        }
        else{
          x[rec[k],3] = 3
        }
      }
    }

    # Counting again
    inf = which(x[,3] == 2)
    sus = which(x[,3] == 1)
    rec = which(x[,3] == 3)
    dead = which(x[,3] == 4)

    X[[i]][,3] = x[,3]
    S[i] = length(sus)
    I[i] = length(inf)
    R[i] = length(rec)
    D[i] = length(dead)

    # Counter measures
    if (I[i] > measures*K){
      steplength = mobility*mobility_reduction
      infprob = inf_prob*inf_prob_reduction
    }

    utils::setTxtProgressBar(pb,i)
  }
  cat("\nSimulation completed\n")
  cat(paste("Deads:",D[n]))

  return(list(
    Infected = I, Susceptible = S, Recovered = R, Dead = D, X = X, n = n, measures = measures
  ))
}
