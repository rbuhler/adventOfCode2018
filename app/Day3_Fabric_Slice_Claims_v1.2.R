# -*-*-*-*-*-*-*-*-*-*-*-*
# SOLUTION LIBRARY
# -*-*-*-*-*-*-*-*-*-*-*-*

# -*-*-*-*-*-*-*-*-*-*-*-*

read_claims<-function(claim_list){

  claims <- data.frame()
  
  list_size <- lengths(claim_list)
  for (i in 1:list_size){
    read <- claim_list[i,1]
    claim_size <- nchar(read)
    
    numbers <- "1234567890"
    step <- ""
    char_order <- ""
    char_left <- ""
    char_top <- ""
    char_wide <- ""
    char_tall <- ""
    coma <- FALSE
    x <- FALSE
    
    for(j in 1:claim_size){
      char <- substr(read,j,j)
      
      if (step == "sequence"){
        if (grepl(char, numbers, fixed=TRUE)){
          char_order <- paste0(char_order, char)
        }
      }
      
      if (step == "position"){
        if (grepl(char, numbers, fixed=TRUE)){
          
          if (!coma){
            char_left <- paste0(char_left, char)          
          }else{
            char_top <- paste0(char_top, char)  
          }          
        }else{
          if (char == ','){
            coma <- TRUE
          }
        }
      }
      
      if (step == "dimention"){
        if (grepl(char, numbers, fixed=TRUE)){
          
          if(!x){
            char_wide <- paste0(char_wide, char)  
          }else{
            char_tall <- paste0(char_tall, char)  
          }
        }else{
          if( char == 'x'){
            x <- TRUE
          }
        }
      }
      
      # --------------------------------    
      if (char == "#"){
        step <- "sequence"
      }
      
      if(char == "@"){
        step <- "position"
      }
      
      if(char == ":"){
        step <- "dimention"
      }    
      
    }
    
    order <- as.numeric(char_order)
    left <- as.numeric(char_left)
    top <- as.numeric(char_top)
    wide <- as.numeric(char_wide)
    tall <- as.numeric(char_tall)
    
    vector <- c(order, left, top, wide, tall)    
    claims <- rbind(claims, vector)
        
  }
  names(claims)<-c("position", "left", "top", "wide", "tall")
  
  claims
    
}

count_overlap<-function(claim_list){
  ptm <- proc.time()
  
  return <- 0
  count <- 0
  claims_vector <- vector()
  
  order  <- 0
  X_col  <- 0
  y_row  <- 0
  length <- 0
  height <- 0

  # read claim list
  list_size <- length(claim_list)
  for (i in 1:list_size){
    read <- read_claim(claim_list[i])
    names(read)<-c("position", "left", "top", "wide", "tall")
    
    order  <- as.numeric(read["position"])
    x_col  <- as.numeric(read["left"])
    y_row  <- as.numeric(read["top"])
    length <- as.numeric(read["wide"])
    height <- as.numeric(read["tall"])
    
     for (k in 1:height){
      for(j in 1:length) {
        
          v <- paste0(k+y_row, 'X',j+x_col)
          claims_vector <- c(claims_vector, v)
          # print (paste0(i, " of ", list_size))
        
      }      
    }
  }

  count <- table(claims_vector)
  duplicates <- table(count)
  return <- as.numeric(duplicates[2])
  
  print("count_overlap")
  print(proc.time() - ptm)
  
  return
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

# Unit Testing
# Load scenario

file <- "app//payload//Day3_UT_001_HAPPY_PATH.csv"
payload <- read_delim("app/payload/Day3_UT_001_HAPPY_PATH.csv", 
                      "\t", quote = "\\\"", escape_double = FALSE, 
                      col_names = FALSE, trim_ws = TRUE)

# read_claims
claim_list <- read_claims(payload)

actual <- claim_list
expected <- data.frame()

  
  row_frame <- c(1, 1, 3, 4, 4)
expected <- rbind (expected, row_frame)
  row_frame <- c(2, 3, 1, 4, 4)
expected <- rbind (expected, row_frame)
  row_frame <- c(3, 5, 5, 2, 2)
expected <- rbind (expected, row_frame)
names(expected)<-c("position", "left", "top", "wide", "tall")

message  = 'read_claims Variant 1'
myAssert.integer.equals(message, expected, actual)

# count_overlap
actual <- count_overlap(claim_list)
expected <- 4
message  = 'count_overlap Variant 1'
myAssert.integer.equals(message, expected, actual[1])

# -------- GET THE RESULT AFTER TESTING 

# file <- "app//payload//Day3_Fabric_Slice_Claims.csv"
# payload <- read.delim(file, header = TRUE, sep = "\t", quote = "\"")
# 
# if (is.data.frame(payload)){
#   payload <- as.vector(t(payload))
# }
# 
# result <- count_overlap(payload)
# 
# print("--- Result ---")
# print(result)
