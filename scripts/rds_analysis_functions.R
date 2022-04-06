# function to extract recruiter id in the coupon data
get_recruiter_id <- function(data){
  idx <- rep(NA,length(nrow(data)))
  for(i in 1:nrow(data)){
    j <- which(data$recruit_id[i] == data$seed_coupon1|data$recruit_id[i] == data$seed_coupon2)
    if(length(j)==0){
      idx[i] <- "Seed" 
    } else {
      idx[i] <- data$pid[j]
    }
  }
  return(idx)
}
