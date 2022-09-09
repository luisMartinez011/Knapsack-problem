n<- readline(prompt="Enter the number of items: ")
v_min <- readline(prompt="Enter the min volume: ")
v_max <- readline(prompt="Enter the max volume: ")
w_min <- readline(prompt="Enter the min weight: ")
w_max <- readline(prompt="Enter the max weight: ")

n<- as.numeric(n)
v_min<- as.numeric(v_min)
v_max<- as.numeric(v_max)
w_min<- as.numeric(w_min)
w_max<- as.numeric(w_max)

exitLoop <- 0


W <- (0.3)*n*((w_min + w_max) / 2)
 
items <- c(1:n)


value=sample(v_min:v_max, n, replace=TRUE)
weight=sample(w_min:w_max, n, replace=TRUE)
data=data.frame(items,weight,value)

rig <- readline(prompt="Enter the name for the instance file: ")
rig <- paste(rig, ".txt")
write.table(data, file = rig, sep = "\t",
            row.names = FALSE)

#data


byValue=function()
{
    cat("\n Heuristic by value\n")
    current_weight <- 0 
    current_value <- 0 
    sortedByValue <- data[order(-value),]
    
    for(i in 1:nrow(sortedByValue)){
        
        if(current_weight + sortedByValue$weight[i] > W){
            
            if(i == nrow(sortedByValue)){
                break;
            }else{
                next;
            }  
        }
        current_weight <- sortedByValue$weight[i] + current_weight
        current_value <- sortedByValue$value[i] + current_value
    }
    cat("\n Final weight: ", current_weight)
    cat("\n Final value: ", current_value)
    cat("\n ********************\n")
}


byWeight=function()
{
    cat("\n ********************\n")   
    cat("\n Heuristic by weight\n")
    current_weight <- 0 
    current_value <- 0 
    sortedByWeight <- data[order(weight),]
    
    for(i in 1:nrow(sortedByWeight)){
        if(current_weight + sortedByWeight$weight[i] > W){
            break;
        }
        current_weight <- sortedByWeight$weight[i] + current_weight
        current_value <- sortedByWeight$value[i] + current_value
    }
    cat("\n Final weight: ", current_weight)
    cat("\n Final value: ", current_value)
    cat("\n ********************\n")
}


byRatio=function()
{
    cat("\n ********************\n")
    cat("\n Heuristic by ratio\n")
    ratio <- data[,3]/data[,2]
    ratioDataFrame <- data.frame(data, ratio)
    sortedByratio <- ratioDataFrame[order(-ratio),]
    #print(sortedByratio)
    current_weight <- 0 
    current_value <- 0 
    
    for(i in 1:n){
        if(current_weight + sortedByratio$weight[i] > W){
            break;
        }
        current_weight <- sortedByratio$weight[i] + current_weight
        current_value <- sortedByratio$value[i] + current_value
    }
    
    cat("\n Final weight: ", current_weight)
    cat("\n Final value: ", current_value)
    cat("\n ********************\n")
}



while(exitLoop <1){
    cat("\nSelect an heuristic:")
    cat("\n1. Heuristic 1")
    cat("\n2. Heuristic 2")
    cat("\n3. Heuristic 3")
    cat("\n4. Run all the Heuristics")
    cat("\n5. Exit")
    optionHeuristic <- readline("\n")




    if(optionHeuristic == 1){
        start.time <- Sys.time()
        byValue()
    }else if(optionHeuristic == 2){
        start.time <- Sys.time()
        byWeight()
        end.time <- Sys.time()
time.taken <- end.time - start.time

    }else if(optionHeuristic == 3){
        start.time <- Sys.time()
        byRatio()
        end.time <- Sys.time()
time.taken <- end.time - start.time
    }else if(optionHeuristic == 4){
        start.time <- Sys.time()
        byValue()
        byWeight()
        byRatio()
        end.time <- Sys.time()
    time.taken <- end.time - start.time
    }else{
        exitLoop = 1
    }
    
    cat("\n")
cat("Knapsack capacity: ", W)
cat("\nItems: ", n) 
cat("\nTime taken: ",time.taken)
cat("\n")
cat("\n ********************\n")
}


cat("\n")
