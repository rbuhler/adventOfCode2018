# -*-*-*-*-*-*-*-*-*-*-*-*
# SOLUTION LIBRARY
# -*-*-*-*-*-*-*-*-*-*-*-*

# -*-*-*-*-*-*-*-*-*-*-*-*

chronal_calibration<-function(iv_array){
  
  result <- sum(iv_array)
  
  result
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

variant <- c(+1, +1, +1)
expected <- 3
actual <- chronal_calibration(variant)
message  = '1'
myAssert.integer.equals(message, expected, actual)

variant <- c(+1, +1, -2)
expected <- 0
actual <- chronal_calibration(variant)
message  = '2'
myAssert.integer.equals(message, expected, actual)

variant <- c(-1, -2, -3)
expected <- -6
actual <- chronal_calibration(variant)
message  = '3'
myAssert.integer.equals(message, expected, actual)

# -------- GET THE RESULT AFTER TESTING 

payload <- read.csv(file = "app//payload//Day1_chronal_calibration.csv",header=FALSE)
result <- chronal_calibration(payload)

print("--- Result ---")
print(result)
