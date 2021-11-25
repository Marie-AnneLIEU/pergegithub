#
#
# deltaT_tableau <- function(list.data,span,s_d){
#
#   chambre <- c()
#
#   ## detecter les colonnes manquantes gen----
#   list_names <- names(list.data)
#   missing_col_gen <- missing_column_gen(list.data)
#
#   ## enlever les fichiers ou il ya des colonnes manquantes
#   if(!is.null(missing_col_gen)){
#     list.data <- list.data[-missing_col_gen]
#   }
#
#   if (length(list.data)>0){
#
#   list.data <- clean_na_gen(list.data)
#
#   if (length(list.data)>0){
#
#   list.data <- Ecart(list.data)
#   list.data <- ext_smooth_list(list.data,span)
#
#   ## enlever les listes  ext = vide ----
#   delete_ext_null <- c()
#   for (i in 1:length(list.data)){
#     data <- list.data[[i]]
#     if(is.null(data$ext_smooth)){
#       delete_ext_null <- c(delete_ext_null,i)
#     }
#   }
#
#   if (!is.null(delete_ext_null)){
#     list.data <- list.data[-delete_ext_null]
#   }
#
#   if(length(list.data)>0){
#
#
#   for (j in 1:3){
#
#     num_chambre <- j
#
#     ## detecter les colonnes manquantes----
#     list_names <- names(list.data)
#     missing_col_cham <- missing_column_chambre(list.data,num_chambre)
#
#     ## enlever les fichiers ou il ya des colonnes manquantes
#     if(!is.null(missing_col_cham)){
#       list.data <- list.data[-missing_col_cham]
#     }
#
#     if (length(list.data)>0){
#
#     list.data <- clean_na_chambre(list.data, num_chambre)
#
#     if(length(list.data)>0){
#
#     # lissage
#     list.data <- smooth_list(list.data,span,num_chambre)
#
#     ## enlever les chambres non declares ----
#     delete_data_null <- c()
#     for (i in 1:length(list.data)){
#       data <- list.data[[i]]
#       if(is.null(data$data_smooth)){
#         delete_data_null <- c(delete_data_null,i)
#       }
#     }
#
#     if (!is.null(delete_data_null)){
#       list.data <- list.data[-delete_data_null]
#     }
#
#     if(length(list.data)>0){
#
#     rest_period <- rest.period(list.data,num_chambre = num_chambre)
#     seg <- ecart_temp(list.data,rest_period,s_d,span=span)
#     list <- res_deperdition(seg,list.data,s_d)
#
#     chambre[[j]] <- list
#     }
#     else{
#       chambre[[j]] <- NULL
#     }}
#     else{
#       chambre[[j]] <- NULL
#     }
#   }}}
#   else{
#     chambre <- NULL
#   }}
#   else{
#     chambre <- NULL
#   }}
#   else{
#     chambre <- NULL
#   }
#
#
#   return(chambre)
# }
