# -*-*-*-*-*-*-*-*-*-*-*-*
# SOLUTION LIBRARY
# -*-*-*-*-*-*-*-*-*-*-*-*

# -*-*-*-*-*-*-*-*-*-*-*-*

chronal_calibration<-function(iv_array){
  
  data <- data.frame("payload" = c(iv_array))
  data_length = nrow(data)
  
  curr        = 0 
  new_current = 0
  cha         = 0 
  new_change  = 0
  res         = 0 
  new_result  = 0
  
  for( count in 1:data_length ){
  #   
    cha <- data[count, 1]
    res <- cha + curr
  
    if( count == 1 ){
      
      new_result = c(res)
      new_change = c(cha)
      new_current = c(curr)      
    
    }else{
      
      new_result = c(new_result, res)
      new_change = c(new_change, cha)
      new_current = c(new_current, curr)
    }
        curr<- res
  }
  result <- data.frame(new_current, new_change, new_result )
  
}

# # -*-*-*-*-*-*-*-*-*-*-*-*
# # UNIT TESTING
# # -*-*-*-*-*-*-*-*-*-*-*-*
# # ASSERTS

source('app/library/Assert.R')

# -*-*-*-*-*-*-*-*-*-*-*-*
# VARIANTS
# -*-*-*-*-*-*-*-*-*-*-*-*

# write.csv(payload, file = "app//payload//Day1_chronal_calibration.csv",row.names=FALSE, na="")

# variant <- c(+1, +1, +1)
# expected <- 3
# actual <- chronal_calibration(variant)
# message  = '1'
# myAssert.integer.equals(message, expected, actual)
# 
# variant <- c(+1, +1, -2)
# expected <- 0
# actual <- chronal_calibration(variant)
# message  = '2'
# myAssert.integer.equals(message, expected, actual)
# 
# variant <- c(-1, -2, -3)
# expected <- -6
# actual <- chronal_calibration(variant)
# message  = '3'
# myAssert.integer.equals(message, expected, actual)

# -------- GET THE RESULT AFTER TESTING 

payload <- read.csv(file = "app//payload//Day1_chronal_calibration.csv",header=FALSE)
result <- chronal_calibration(payload)
result
