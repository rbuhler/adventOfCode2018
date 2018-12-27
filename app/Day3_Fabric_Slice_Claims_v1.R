# -*-*-*-*-*-*-*-*-*-*-*-*
# SOLUTION LIBRARY
# -*-*-*-*-*-*-*-*-*-*-*-*

# -*-*-*-*-*-*-*-*-*-*-*-*

read_claim<-function(claim_list){
  
  numbers <- "1234567890"
  result <- vector()
  result_frame <- data.frame()
  list_size <- length(claim_list)
  
  for (i in 1:list_size){
    seq  <- 0
    left <- 0
    top  <- 0
    wide <- 0
    tall <- 0
    step <- ""
    char_order <- ""
    char_left  <- ""
    char_top   <- ""
    char_wide  <- ""
    char_tall  <-""
    coma <- FALSE
    x <- FALSE
    
    claim <- claim_list[i]
    claim_size <- nchar(claim)
    
    for(j in 1:claim_size){
      char <- substr(claim,j,j)
      
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
    
    result <- c(order, left, top, wide, tall)
    result_frame <- rbind(result_frame, result) 
    
  }
  
  result_frame
}

count_overlap<-function(claim_list){
  
  return <- 0
  count <- 0
  claims_vector <- vector()
  
  order  <- 0
  X_col  <- 0
  y_row  <- 0
  length <- 0
  height <- 0
  
  read <- read_claim(claim_list)
  list_size <- nrow(read)
  
  for (i in 1:list_size){
    names(read)<-c("position", "left", "top", "wide", "tall")
    
    order  <- as.numeric(read[i,"position"])
    x_col  <- as.numeric(read[i,"left"])
    y_row  <- as.numeric(read[i,"top"])
    length <- as.numeric(read[i,"wide"])
    height <- as.numeric(read[i,"tall"])
    
    print( paste0("Processing ", ( i / list_size) * 100, " %"))
    
     for (k in 1:height){
      for(j in 1:length) {
        
          v <- paste0(k+y_row, 'X',j+x_col)
          claims_vector <- c(claims_vector, v)
        
      }      
    }
  }

  count <- table(claims_vector)
  duplicates <- table(count)
  return <- as.numeric(sum( sum(duplicates) - duplicates[1]) )
  
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

# claim_one   = "#1 @ 1,3: 4x4"
# claim_two   = "#2 @ 3,1: 4x4"
# claim_three = "#3 @ 5,5: 2x2"
# 
# claim_list = c(claim_one, claim_two, claim_three)
# 
# variant <- claim_list
# expected <- 4
# actual <- count_overlap(variant)
# message  = 'Variant 1'
# myAssert.integer.equals(message, expected, actual[1])

# -------- GET THE RESULT AFTER TESTING 

file <- "app//payload//Day3_Fabric_Slice_Claims.csv"
payload <- read.delim(file, header = FALSE, sep = "\t", quote = "\"")

if (is.data.frame(payload)){
  payload <- as.vector(t(payload))
}

result <- count_overlap(payload)

print("--- Result ---")
print(result)
