start.time <- Sys.time()

opcion <- 2

n = 1000000
v_min <- 1
v_max <- 10
w_min <- 1
w_max <- 25

W <- (0.3)*n*((w_min + w_max) / 2)
#items <- sample(1:n, n, replace=TRUE)
cat("peso total", W) 
items <- c(1:n)


value=sample(v_min:v_max, n, replace=TRUE)
weight=sample(w_min:w_max, n, replace=TRUE)
data=data.frame(items,weight,value)
cat("\nnumero total de items", n) 
cat("\n")
#data

#create the function that we want to optimize
byValue=function()
{
    current_weight <- 0 
    current_value <- 0 
    sortedByValue <- data[order(-value),]
    #print(sortedByValue)
    
    for(i in 1:nrow(sortedByValue)){
        #print(sortedByValue$value[i])
        if(current_weight + sortedByValue$weight[i] > W){
            break;
        }
        current_weight <- sortedByValue$weight[i] + current_weight
        current_value <- sortedByValue$value[i] + current_value
    }
    cat("\n Peso final: ", current_weight)
    cat("\n Valor final: ", current_value)
}


byWeight=function()
{
    current_weight <- 0 
    current_value <- 0 
    sortedByWeight <- data[order(weight),]
    #print(sortedByWeight)
    
    for(i in 1:nrow(sortedByWeight)){
        #print(sortedByWeight$weight[i])
        if(current_weight + sortedByWeight$weight[i] > W){
            break;
        }
        current_weight <- sortedByWeight$weight[i] + current_weight
        current_value <- sortedByWeight$value[i] + current_value
    }
    cat("\n Peso final: ", current_weight)
    cat("\n Valor final: ", current_value)
}


byRatio=function()
{
    
    #print(data[,3]/data[,2])
    ratio <- data[,3]/data[,2]
    ratioDataFrame <- data.frame(data, ratio)
    sortedByratio <- ratioDataFrame[order(-ratio),]
    #print(sortedByratio)
    current_weight <- 0 
    current_value <- 0 
    
    for(i in 1:n){
        #print(sortedByratio$weight[i])
        if(current_weight + sortedByratio$weight[i] > W){
            break;
        }
        current_weight <- sortedByratio$weight[i] + current_weight
        current_value <- sortedByratio$value[i] + current_value
    }
    
    cat("\n Peso final: ", current_weight)
    cat("\n Valor final: ", current_value)
}

if(opcion==1){
    byValue()
} else if( opcion == 2){
    byWeight()
}else{
    byRatio()
}


cat("\n")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken