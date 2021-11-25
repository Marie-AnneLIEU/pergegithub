# res_deperdition <- function(seg,list.data,s_d){
#   plage <- round(seq(-20,45,s_d),digits = 1)
#   res_deper_list <- c()
#   for (k in 1:(length(plage))){
#     res_deper_list[[k]] <- NA
#   }
#   # res_deper_tab <-  data.frame(matrix(ncol = length(plage)+1))
#   # colnames(res_deper_tab)<- c("Date",as.character(plage))
#   names(res_deper_list)<- as.character(plage)
#
#   full_list <- c()
#   for (i in 1:length(seg)){
#     if(is.data.frame(seg[[i]])){
#       full_list <- rbind(full_list,seg[[i]])
#     }
#   }
#   if(!is.null(full_list)){
#   if(nrow(full_list)>0){
#   for(j in 1:nrow(full_list)){
#     a <- full_list$plage_t_ext[j]
#     res_deper_list[[a]] <- c(res_deper_list[[a]], full_list$dif_temp[j])
#     res_deper_list[[a]] <- res_deper_list[[a]][!is.na(res_deper_list[[a]])]
#   }}}
#   return(res_deper_list)
# }
