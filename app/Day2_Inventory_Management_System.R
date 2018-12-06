# -*-*-*-*-*-*-*-*-*-*-*-*
# SOLUTION LIBRARY
# -*-*-*-*-*-*-*-*-*-*-*-*

# -*-*-*-*-*-*-*-*-*-*-*-*

count_duplicates<-function(id_list){
  
  
  list_size <- nrow(id_list)
  
  for(i in 1:id_list){
    id_row <- id_list[i]
    
    id_size <- length(id_row){
      for (j in 1:id_size){
        
      }
    }
    
  }
  
}

get_checksum<-function(id_list){
  
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

# write.csv(payload, file = "app//payload//Day2_Inventory_Management_System.cvs",row.names=FALSE, na="")

variant <- c("abcdef","bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab")
expected <- 12
actual <- first_double_frequency(variant)
message  = '1'
myAssert.integer.equals(message, expected, actual)

# -------- GET THE RESULT AFTER TESTING 

# payload <- read.csv(file = "app//payload//Day2_Inventory_Management_System.cvs",header=FALSE)
# result <- first_double_frequency(payload)
# 
# print("--- Result ---")
# print(result)
