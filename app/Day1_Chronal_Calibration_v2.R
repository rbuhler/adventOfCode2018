# -*-*-*-*-*-*-*-*-*-*-*-*
# SOLUTION LIBRARY
# -*-*-*-*-*-*-*-*-*-*-*-*

# -*-*-*-*-*-*-*-*-*-*-*-*

chronal_calibration<-function(iv_current, iv_change){
  
  change <- data.frame("payload" = c(iv_change))
  change_length = nrow(change)
  
  curr        = 0
  new_current = vector()
  cha         = 0 
  new_change  = vector()
  res         = iv_current 
  new_result  = vector()
  
  for( count in 1:change_length ){
    curr <- res
    cha  <- change[count, 1]
    res  <- cha + curr
  
    new_result = c(new_result, res)
    new_change = c(new_change, cha)
    new_current = c(new_current, curr)

  }
  result <- data.frame(new_current, new_change, new_result )
  
}

find_double<-function( iv_vect_a, iv_vect_b ){
  
  lenght_a <- nrow(iv_vect_a)
  lenght_b <- nrow(iv_vect_a)
  found    <- FALSE
  twice    <- 0
  result   <- c( found, twice )
  
  for( i in 1:lenght_b ){
    chk_b <- iv_vect_b[i, 3]
    
    for( j in 1:lenght_a ){
      chk_a <- iv_vect_a[i, 3]
      
      if ( chk_b == chk_a && found == FALSE ){
        twice = chk_b
        found  = TRUE
        break
      }
    }
    
    if ( found == TRUE ){
      result <- c( found, twice )
      break
    }
  }
}

first_double_frequency<-function(iv_array){
  
  vector_B <- iv_array
  
  repeat{
    
    vector_A <- chronal_calibration(0, vector_B)
    vector_B <- chronal_calibration(vector_A[nrow(vector_A),3], vector_A)
    
    result = find_double(vector_A, vector_B)
    
    break
    
  }

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

variant <- c(+1, -1)
expected <- 0
actual <- first_double_frequency(variant)
message  = '1'
myAssert.integer.equals(message, expected, actual)

variant <- c(+3, +3, +4, -2, -4)
expected <- 10
actual <- first_double_frequency(variant)
message  = '2'
myAssert.integer.equals(message, expected, actual)

variant <- c(-6, +3, +8, +5, -6)
expected <- 5
actual <- first_double_frequency(variant)
message  = '3'
myAssert.integer.equals(message, expected, actual)

variant <- c(+7, +7, -2, -7, -4)
expected <- 14
actual <- first_double_frequency(variant)
message  = '4'
myAssert.integer.equals(message, expected, actual)

# -------- GET THE RESULT AFTER TESTING 

payload <- read.csv(file = "app//payload//Day1_chronal_calibration.csv",header=FALSE)
result <- first_double_frequency(payload)

print("--- Result ---")
print(result)
