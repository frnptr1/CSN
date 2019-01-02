PREFIX = "../data/BA_growth_rand_v_"

#################
### Variables ###
#################
filenamesBA.Rand.Att <- c("../data/BA_growth_rand_v_00001.csv", 
                          "../data/BA_growth_rand_v_00010.csv", 
                          "../data/BA_growth_rand_v_00100.csv",
                          "../data/BA_growth_rand_v_01000.csv")

filenameBA.Rand.Att.Final <- "../data/BA_growth_rand_v_10000.csv"
#################
### Functions ###
#################

getTwoRandomNodes <- function(m.0, nodeList, n){
    
    n = n - 1
    selectedNodes <- sample(nodeList[1:n], m.0, prob = NULL)
    return(selectedNodes)
}


generateBarabasiAlbertModelRandAttachment <- function(ts, n.0, m.0, v.track) {
    
    n.max = ts + n.0
    nodeList = seq(n.max)   ## Vector representing idx from 1..n.max
    k = rep(0, n.max)       ## Degrees of each node. Start on 0
    
    n = n.0 + 1
    
    v.track.1 <- c(0)
    v.track.2 <- c(0)
    v.track.3 <- c(0)
    v.track.4 <- c(0)
    
    for(t in seq(ts)){
        
        shouldGetRandNodes <- TRUE
        
        while(shouldGetRandNodes){
            selectedNodes <- getTwoRandomNodes(m.0, nodeList, n)
            
            n1 = selectedNodes[1]
            n2 = selectedNodes[2]
            
            ## Not allowing multiedges and assuring the stub connects to two diff. nodes.
            if(n1 == n2){
                shouldGetRandNodes = TRUE
            } else{
                shouldGetRandNodes = FALSE
            }
        }
        
        
        ## Increase degree of each vertice
        k[n1] <- k[n1] + 1
        k[n2] <- k[n2] + 1
        k[n] <- k[n] + m.0
        
        
        # Keep track of our 4 vertices
        v.track.1 <- append(v.track.1, k[v.track[1] + n.0])
        v.track.2 <- append(v.track.2, k[v.track[2] + n.0])
        v.track.3 <- append(v.track.3, k[v.track[3] + n.0])
        v.track.4 <- append(v.track.4, k[v.track[4] + n.0])
        
        
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


runBA.Rand.Attach <- function(t.max) {
    
    n.0 <- 3
    m.0 <- 2
    
    start = Sys.time()
    k = generateBarabasiAlbertModelRandAttachment(t.max, n.0, m.0, timestamps)
    end = Sys.time()
    
    elapsedTime = end - start
    cat("Elasped time: ", elapsedTime, "\n")
    
    return(k)
}



if(!LOAD_EXISTING_RUN_BA_RAND){
    if(APPLY_PROFILING){
        Rprof(tmp <- tempfile())
        final.k =runBA.Rand.Attach(t.max)
        Rprof()
        summaryRprof(tmp)
    } else {
        final.k = runBA.Rand.Attach(t.max)
        saveNodesDegreeOnFile(t.max, final.k, PREFIX)
    }
} 


table.BA.Rand <- loadModelExecutions(filenamesBA.Rand.Att)

