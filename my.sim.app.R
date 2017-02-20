my.sim <- function(n=100, graph='random', graphParam=list(z=2), M=100, distr='unif', distrParam=list(), kars=F, karsParam=list()){
  ### Input perematers:
  # n = number of agents
  # graph = 'random' or 'scalefree'
  # graphParam = list with parameters (either of them depending on the graph type chosen):
  #             z = average degree for a random graph,
  #             gamma = power-law exponent value
  # M = number of simulations
  # distr = preferred threshold distribution
  # ditsrParam = list with distribution parameters (either of them depending on the distribution chosen):
  #             'const' : tau = value of the constant threshold, rioters = fraction of initial rioters,
  #             'unif'  : empty list
  #             'normal': mean = mean, sd = standard deviations
  # kars = TRUE or FALSE,
  #	 TRUE being for Karsai's setting, i.e. with spontaneous adoption
  #	 and a proportion of nodes that never adopt (should be provided further)
  # karsParam = list with spontaneous adoption parameters (if it is chosen):
  #             r = fraction of never-adopting nodes,
  #             pn = probability of spontaneous adoption
  
  M <- as.numeric(M); n <- as.numeric(n);
  S <- numeric(M)
  out <- list()
  
  initial <- final <- thresholds <- matrix(rep(NA, M*n), nrow=M)
  graphs <- list(rep(NA, n))
  
  for (j in 1:M){
    # keep track of states of all agents 
    if (graph == 'random')
      rand.graph <- erdos.renyi.game(n, p.or.m=graphParam$z/n, type='gnp', directed=F)
    if (graph=='scalefree')
      rand.graph <- barabasi.game(n, power=graphParam$gamma, zero.appeal=10, directed=F)
    
    # save the very first graph for plotting    
    if (j==1) out$graph <- rand.graph
    
    out$graphs[[j]] <- rand.graph
    
    states <- rep(0, n)
    prev_size <- 0
    
    # if we consider Karsai's Model, we should also denote proportion of never-adoopting agents r
    # and probability of spontaneous adoption pn
    r <- 0
    pn <- 0
    active <- 0
    if (kars){
      #cat(karsParam)
      r <- karsParam$r
      pn <- karsParam$pn
      active <- floor(n*(1-r))
    }
    
    # generate new thresholds
    if (distr == 'unif')
      th <- sort(round(runif(n), 2))
    if (distr == 'const'){
      th <- c(rep(0, n*distrParam$rioters), rep(distrParam$tau, n*(1-distrParam$rioters)))
    }
    if (distr == 'normal')
      th <- sort(round(pmax(pmin(1, rnorm(n, mean=distrParam$mean, sd=distrParam$sd)), 0), 2))
    
    # if we have non-zero proportion of never-adopting agents, put their thresholds to bigger than 1
    if (r>0)
      th[active:n] <- 1.01
    
    # shuffle thresholds, as they were ordered
    th <- sample(th)
    
    thresholds[j,] <- th
    
    # keep never adopting nodes so that they are not affected by spontaneous adoption
    never_adopting <- th>1

    # matrix used for collecting the states through the simulation
    # will be used later for plotting
    if (j==1) {SS <- matrix(rep(NA, n), nrow=1);
    #          cat('Creating states matrix of size' , dim(SS), '... \n')
    }
    
    # change state of rioters with threshold 0
    states[th==0] <- 1
    
    initial[j,] <- states
    
    new_rioters <- which(th==0)
    if (j==1) {SS[1, ] <- states;
    #          cat('Updating states matrix of size ', dim(SS), '...\n') 
    }
    
    # empty vector for the neighbors for now
    neighbors <- numeric(0)
    cascade_size <- sum(states)
    
    adj.matrix <- as_adj(rand.graph, type='both', sparse=F)
    # list to keep number of neighbors for each node
    all_neighbors <- apply(adj.matrix, 1, sum)
    rioting_neighbors <- rep(0,n)
    
    while (cascade_size - prev_size != 0){
      
      # we only consider neighbors of new rioters
      if (length(new_rioters) >1 ){
        nn <- which(adj.matrix[new_rioters,]==1, arr.ind=T)[,2]
      } else {
        nn <- unique(which(adj.matrix[new_rioters,]==1))
      }
      
      # calculate, how many times each neighbor is mentioned, using table
      # E.g., if a node is mentioned 2 times, it has 2 new rioting neighbors
      neighbors <- as.numeric(rownames(as.matrix(table(nn))))
      # add new rioting neighbors to the exiscting ones
      rioting_neighbors[neighbors] <- as.vector(table(nn)) + rioting_neighbors[neighbors]
      
      # keep rioters of the current step
      old_rioters <- which(states==1)
      # change state if threshold is exceeded
      states[neighbors] <- th[neighbors] <= rioting_neighbors[neighbors]/all_neighbors[neighbors]
      
      # if state of anyone is changed, add him to the new rioters
      # but only is he is not already rioting
      new_rioters <- setdiff(which(states == 1), old_rioters)
      
      # spontaneous adoption for Karsai's model
      if (kars){
        spont <- runif(n) <= pn
        # we either keep previous state, if it was already 1
        # or change state to 1, if spontaneous adoption was successful
        new_states <- as.numeric(states | spont)
        # keep never adopting nodes down
        new_states[never_adopting] <- 0
        # add spontaneous adopters to the new rioters
        new_rioters <- c(new_rioters, (1:n)[(new_states)==1 & (states==0)])
        # update the states
        states <- new_states
      }
      
     if (j==1) {
     #   cat('Updating states matrix with vector of size ', length(states), '...\n'); 
        SS <- rbind(SS, states)
    #    cat('Now its size is ', dim(SS), '\n\n')
        }
      
      if (length(new_rioters)==0)
        break;
      
      prev_size <- cascade_size
      cascade_size <- sum(states)
      
    }
    
    final[j, ] <- states
    S[j] <- cascade_size
  }
  
  out$states <- SS
  out$cascades <- S
  out$thresholds <- thresholds
  out$initial <- initial
  out$final <- final
  return(out)
}
