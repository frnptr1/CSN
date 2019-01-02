PREFIX = "../data/BA_no_growth_v_"

#################
### Variables ###
#################
filenamesBA.no.growth <- c("../data/BA_no_growth_v_00001.csv", 
                 "../data/BA_no_growth_v_00010.csv", 
                 "../data/BA_no_growth_v_00100.csv",
                 "../data/BA_no_growth_v_01000.csv")

filename.final.BA.no.growth <- "../data/BA_no_growth_v_10000.csv"

#################
### Functions ###
#################

getTwoRandomNodesPreferentialAttNoGrowth <- function(sum.kj, m.0, nodeList, k){
    

    # Compute probs.
    ## Adding offset so everyone has a chance to be selected
    pi.k = ((k + 1)/(sum.kj + 1))

    selectedNodes <- sample(nodeList, m.0, prob = pi.k)
    return(selectedNodes)
}




generateBarabasiAlbertModelNoGrowth <- function(ts, n.max, m.0, v.track) {
    
    nodeList = seq(n.max)   ## Vector representing idx from 1..n.max
    k = rep(0, n.max)       ## Degrees of each node. Start on 0
    sum.kj = 0              ## Total sum of degrees
    
    adj.mat = Matrix(0, ncol = n.max, nrow = n.max, sparse=TRUE) 
    
    v.track.1 <- c(0)
    v.track.2 <- c(0)
    v.track.3 <- c(0)
    v.track.4 <- c(0)
    
    for(t in seq(ts)){
        
        n = sample(nodeList, 1, prob = NULL)
        shouldGetRandNodes <- TRUE
        
        while(shouldGetRandNodes){
            selectedNodes <- getTwoRandomNodesPreferentialAttNoGrowth(sum.kj, m.0, nodeList, k)
            
            n1 = selectedNodes[1]
            n2 = selectedNodes[2]
            
            ## Not allowing multiedges and assuring the stub connects to two diff. nodes.
            if(n1 != n2 & n != n1 & n != n2 & adj.mat[n, n1] == 0 & adj.mat[n, n2] == 0){
                shouldGetRandNodes = FALSE
            }
        }
        
        # Add edges on adj.mat
        adj.mat[n, n1] <- 1
        adj.mat[n1, n] <- 1
        
        adj.mat[n, n2] <- 1
        adj.mat[n2, n] <- 1
        
        ## Add num. of edges added on each it to the total sum
        sum.kj <- sum.kj + 4
        
        ## Increase degree of each vertice
        k[n1] <- k[n1] + 1
        k[n2] <- k[n2] + 1
        k[n] <- k[n] + m.0
        
        # Keep track of our 4 vertices
        v.track.1 <- append(v.track.1, k[v.track[1]])
        v.track.2 <- append(v.track.2, k[v.track[2]])
        v.track.3 <- append(v.track.3, k[v.track[3]])
        v.track.4 <- append(v.track.4, k[v.track[4]])
        
        
        # Increase n
        n = n + 1
        
        # Printing to keep track of the execution :)
        if(t%%100 == 0){
            cat("t: ", t,"\n")
        }
    }
    
    # Save 4 vertices tracking
    saveNodesDegreeOnFile(v.track[1], v.track.1, PREFIX)
    saveNodesDegreeOnFile(v.track[2], v.track.2, PREFIX)
    saveNodesDegreeOnFile(v.track[3], v.track.3, PREFIX)
    saveNodesDegreeOnFile(v.track[4], v.track.4, PREFIX)

    return(k)
}



#################
## Running code #
#################

runBANoGrowth <- function(t.max) {
    
    m.0 <- 2
    n.max = 2000
    
    start = Sys.time()
    k = generateBarabasiAlbertModelNoGrowth(t.max, n.max, m.0, timestamps)
    end = Sys.time()
    
    elapsedTime = end - start
    cat("Elasped time: ", elapsedTime, "\n")
    
    return(k)
}



if(!LOAD_EXISTING_RUN_BA_NO_GROWTH){
    
    if(APPLY_PROFILING){
        Rprof(tmp <- tempfile())
        runBANoGrowth(t.max)
        Rprof()
        summaryRprof(tmp)
    } else {
        final.k = runBANoGrowth(t.max)
        saveNodesDegreeOnFile(t.max, final.k, PREFIX)
    }
}

table.BA.no.growth <- loadModelExecutions(filenamesBA.no.growth)
final.BA.no.growth.seq <- read.csv2(filename.final.BA.no.growth); final.BA.no.growth.seq <- final.BA.no.growth.seq$x
