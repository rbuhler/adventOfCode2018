# -*-*-*-*-*-*-*-*-*-*-*-*
# UNIT TESTING
# -*-*-*-*-*-*-*-*-*-*-*-*
# ASSERTS
myAssert.integer.equals<-function(message, expected, actual){
  if (expected != actual){
    
    print("Integer Assertion ERROR : Not equals")
    print(paste("Expected [ ", expected, " ]" )) 
    print(paste("Actual   [ ", actual, " ]"))
    print(paste("Message  : ", message))
    
  }
}
myAssert.data.frame.equals<-function(message, expected, actual){
  vec_act <- vector()
  vec_exp <- vector()
  
  size_act <- nrow(actual)
  size_exp <- nrow(expected)
  
# DATA FRAME SIZE  
  if (size_act != size_exp){
    print("Data Frame Assertion ERROR : Not same row number")
    print(paste("Expected size [ ", size_exp, " ]" )) 
    print(paste("Actual size   [ ", size_act, " ]"))
    print(paste("Message       : ", message))
  }else{
# DATA FRAME ROWS    
    for( i in 1:size_act){
     vec_act <- actual[i,]
     vec_exp <- expected[i,]
     
     size_vec_act <- length(vec_act)
     size_vec_exp <- length(vec_exp)
     
     if ( size_vec_act != size_vec_exp){
       print("Data Frame Assertion ERROR : Actual row ", paste(i), " differs from Expected")
       print(paste("Expected size [ ", size_vec_exp, " ]" )) 
       print(paste("Actual size   [ ", size_vec_act, " ]"))
       print(paste("Message       : ", message))
     }else{
       compare <- as.matrix(actual) == as.matrix(expected)
       count_diff <- table(compare)
       
       if( length(count_diff) > 1 ){
         print("Data Frame Assertion ERROR : Actual row differs from Expected")
         print(paste("Differences [ ", count_diff["FALSE"], " ]" )) 
         print(paste("Matches [ ",count_diff["TRUE"], " ]"))
         print(paste("Message       : ", message)) 
       }
     }
    }
  }
}