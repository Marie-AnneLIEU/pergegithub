# ecart_temp <- function(list.data,rest_period,s_d,span){
#   plage <- round(seq(-20,45,s_d),digits = 1)
#   # ecart_temp <-  data.frame(matrix(ncol = length(plage)+1))
#   # colnames(ecart_temp)<- c("Date",as.character(plage))
#   seg <- c()
#   for (i in 1:length(list.data)){
#     data <- list.data[[i]]
#     rest <- rest_period[[i]]
#     temp <- data$ext_smooth
#     t_amb <- data$data_smooth
#     ecart_sec <- data$Ecart
#     if(nrow(rest)>0){
#     if (!is.na(rest$start[1])){
#       rest_seg <- c()
#     for (j in 1:nrow(rest)){
#       rest_seg[[j]] <- deltaT_period(j,temp,t_amb,ecart_sec,plage,rest,s_d)
#     }
#     t <- c()
#     for (m in 1:length(rest_seg)){
#       if(is.data.frame(rest_seg[[m]]))
#       t <- rbind(t,rest_seg[[m]])
#     }
#     date <- rep(data$date[1],nrow(t))
#     t <- cbind(date,t)
#     seg[[i]] <- t}
#
#     else{
#         seg[[i]] <- NA}
#     }
#     else{
#       seg[[i]] <- NA
#     }}
#   return(seg)
# }
