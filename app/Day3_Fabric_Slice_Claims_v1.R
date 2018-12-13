# -*-*-*-*-*-*-*-*-*-*-*-*
# SOLUTION LIBRARY
# -*-*-*-*-*-*-*-*-*-*-*-*

# -*-*-*-*-*-*-*-*-*-*-*-*

count_duplicates<-function(id_list){
  
  list_size <- length(id_list)
  str_result <- ""
  count <- 0
  result <- vector()
  
  for(i in 1:list_size){
    id_rowA <- as.character(id_list[i])
    id_size <- nchar(id_rowA)
    
    for (j in 1:list_size){
      id_rowB <- as.character(id_list[j])
      
      vec_rowA <- paste0(unlist(strsplit(id_rowA, "")))
      vec_rowB <- paste0(unlist(strsplit(id_rowB, "")))
      count <- id_size
      
      for (k in 1:id_size){
        if (vec_rowA[k] == vec_rowB[k]){
            str_result <- paste0( str_result, vec_rowA[k])
            count <- count -1
        }
      }
      
      if ( count == 1 ) {
        result[length(result)+1] <- str_result
      }else{
        count <- 0 
        str_result <- ""
      }
      
    }
  }
  result <- unique(result)
}

# # -*-*-*-*-*-*-*-*-*-*-*-*
# # UNIT TESTING
# # -*-*-*-*-*-*-*-*-*-*-*-*
# # ASSERTS

source('app/library/Assert.R')

# -*-*-*-*-*-*-*-*-*-*-*-*
# VARIANTS
# -*-*-*-*-*-*-*-*-*-*-*-*

# write.csv(payload, file = "app//payload//Day2_Inventory_Management_System.csv",row.names=FALSE, na="")

variant <- c("abcde","fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")
expected <- "fgij"
actual <- count_duplicates(variant)
message  = '1'
myAssert.integer.equals(message, expected, actual[1])

# -------- GET THE RESULT AFTER TESTING 

payload <- read.csv(file = "app//payload//Day2_Inventory_Management_System2.csv",header=FALSE)$V1
result <- count_duplicates(payload)

print("--- Result ---")
print(result)
