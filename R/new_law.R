#' @title calculer la nouvelle loi d'eau
#' @usage new_law(fin,P_TEXTMAX,P_DTCHAUD,P_TMAX)
#' new_law_chambre(chambre,fin,P_TEXTMAX,P_DTCHAUD,P_TMAX)
#' @description retour un tableau contenant les nouveaux paramètres de la loi d'eau (pente, origine, décalage parallèle, intervalles de confiance)
#' @description \code{new_law_chambre} est la version chambre-spécifique de \code{new_law}
#' @param fin sortie de la fonction \code{mix}
#' @param P_TEXTMAX température maximale de l'extérieur au-delà de laquelle le système de chauffage ne fonctionne pas
#' @param P_DTCHAUD Delta chaudière
#' @param P_TMAX température maximale de la loi d'eau
#' @export

new_law <- function(fin,P_TEXTMAX,P_DTCHAUD,P_TMAX){
  require(segmented)
  law <- c()
  for(i in 1:3){
  # rup<-rep(0,2)
  del <- which(!is.na(fin[[i]]$tab_text_cmaxamb_mix[1,-1]))
  if(length(del)>0){
    x <- as.numeric(colnames(fin[[i]]$tab_text_cmaxamb_mix[1,-1])[del])
    y <- as.numeric(fin[[i]]$tab_text_cmaxamb_mix[1,-1][del])
    y_dep <- y - as.numeric(fin[[i]]$tab_ind_mix[1,-1][del])
    del_y <- which(y_dep<P_TMAX)
    y <- y[del_y]
    y_dep <- y_dep[del_y]
    x <- x[del_y]
    # y_t <- rep(NA,length(y))
    # for (j in 1:length(y)){
    #   y_t[j] <- min(y[j],P_TMAX)
    # }
    # y_t2 <- rep(NA,length(y))
    # for(k in 1:length(y)){
    #   y_t2[k] <- max(y_t[k],P_TEXTMAX+P_DTCHAUD)
    # }
    # y <- y_t2
    #detect changepoints
    if(length(y)>0){
      mod <- lm(y~x)
      mod_dep <- lm(y_dep~x)
      coef <- 0
      ord <- mean(y)
      ic_pente <- NA
      ic_origine <- NA
      # seglm1 <- try(segmented.lm(mod,seg.Z = ~x,npsi=npsi))
      # seglm2 <- try(segmented.lm(mod_dep,seg.Z = ~x,npsi=npsi_dep))

      # if(!is.error(seglm1)){
      #   coef <- slope(seglm1)$x[slope_choice]
      #   ord <- intercept(seglm1)$x[slope_choice]
      #
      #   upper_coef <- coef-1.28*slope(seglm1)$x[npsi+slope_choice]
      #   lower_coef <- coef+1.28*slope(seglm1)$x[npsi+slope_choice]
      #
      #   upper_ord <- ord-1.28*coef(summary(seglm1))[(2*(npsi+1))+1]
      #   lower_ord <- ord+1.28*coef(summary(seglm1))[(2*(npsi+1))+1]
      #
      #   ic_pente<-paste0("[",round(upper_coef,digits = 2),":",round(lower_coef,digits = 2),"]")
      #   ic_origine<-paste0("[",round(upper_ord,digits = 2),":",round(lower_ord,digits = 2),"]")
      #
      #   rup[1]<-1
      #   mod<-seglm1
      # }
      # else{
      if(!is.na(mod$coefficients[2])){
        coef <- mod$coefficients[2]
        ord <- mod$coefficients[1]

        upper_coef <- coef-1.28*coef(summary(mod))[4]
        lower_coef <- coef+1.28*coef(summary(mod))[4]

        upper_ord <- ord-1.28*coef(summary(mod))[3]
        lower_ord <- ord+1.28*coef(summary(mod))[3]

        ic_pente<-paste0("[",round(upper_coef,digits = 2),":",round(lower_coef,digits = 2),"]")
        ic_origine<-paste0("[",round(upper_ord,digits = 2),":",round(lower_ord,digits = 2),"]")

      }
      coef_dep <- 0
      ord_dep <- mean(y_dep)
      # if(!is.error(seglm2)){
      #   mod_dep<- seglm2
      #   rup[2]<-1
      #   coef_dep <- slope(seglm2)$x[slope_choice_dep]
      #   ord_dep <- intercept(seglm2)$x[slope_choice_dep]
      # }
      # else{
      if(!is.na(mod_dep$coefficients[2])){
        coef_dep <- mod_dep$coefficients[2]
        ord_dep <- mod_dep$coefficients[1]
      }
      yx<-data.frame(y_dep,y,x)
      list_param <- data.frame(round(coef,digits = 2),round(ord,digits = 2),ic_pente,ic_origine
                               ,round(ord-(-coef*P_TEXTMAX+P_DTCHAUD+P_TEXTMAX),digits = 2))
      colnames(list_param) <- c("Pente","Origine","IC_pente","IC_origine","Décalage parallèle")
      list_param_dep <- data.frame(round(coef_dep,digits = 2),round(ord_dep,digits = 2),
                                   round(ord_dep-(-coef_dep*P_TEXTMAX+P_DTCHAUD+P_TEXTMAX),digits = 2))
      results <- list(list_param=list_param,yx=yx,mods=mod,mods_dep=mod_dep,list_param_dep=list_param_dep)}
    else{results <- NULL}}
  else{results <- NULL
  }
  law[[i]]<-results}
  return(law)}

#' @inherit new_law
#' @export
#new law
new_law_chambre <- function(chambre,fin,P_TEXTMAX,P_DTCHAUD,P_TMAX){
  require(segmented)
  i<-chambre
  # rup<-rep(0,2)
    del <- which(!is.na(fin[[i]]$tab_text_cmaxamb_mix[1,-1]))
    if(length(del)>0){
      x <- as.numeric(colnames(fin[[i]]$tab_text_cmaxamb_mix[1,-1])[del])
      y <- as.numeric(fin[[i]]$tab_text_cmaxamb_mix[1,-1][del])
      y_dep <- y - as.numeric(fin[[i]]$tab_ind_mix[1,-1][del])
      del_y <- which(y_dep<P_TMAX)
      y <- y[del_y]
      y_dep <- y_dep[del_y]
      x <- x[del_y]
      # y_t <- rep(NA,length(y))
      # for (j in 1:length(y)){
      #   y_t[j] <- min(y[j],P_TMAX)
      # }
      # y_t2 <- rep(NA,length(y))
      # for(k in 1:length(y)){
      #   y_t2[k] <- max(y_t[k],P_TEXTMAX+P_DTCHAUD)
      # }
      # y <- y_t2
      #detect changepoints
      if(length(y)>0){
        mod <- lm(y~x)
        mod_dep <- lm(y_dep~x)
        coef <- 0
        ord <- mean(y)
        ic_pente <- NA
        ic_origine <- NA
        # seglm1 <- try(segmented.lm(mod,seg.Z = ~x,npsi=npsi))
        # seglm2 <- try(segmented.lm(mod_dep,seg.Z = ~x,npsi=npsi_dep))

        # if(!is.error(seglm1)){
        #   coef <- slope(seglm1)$x[slope_choice]
        #   ord <- intercept(seglm1)$x[slope_choice]
        #
        #   upper_coef <- coef-1.28*slope(seglm1)$x[npsi+slope_choice]
        #   lower_coef <- coef+1.28*slope(seglm1)$x[npsi+slope_choice]
        #
        #   upper_ord <- ord-1.28*coef(summary(seglm1))[(2*(npsi+1))+1]
        #   lower_ord <- ord+1.28*coef(summary(seglm1))[(2*(npsi+1))+1]
        #
        #   ic_pente<-paste0("[",round(upper_coef,digits = 2),":",round(lower_coef,digits = 2),"]")
        #   ic_origine<-paste0("[",round(upper_ord,digits = 2),":",round(lower_ord,digits = 2),"]")
        #
        #   rup[1]<-1
        #   mod<-seglm1
        # }
        # else{
          if(!is.na(mod$coefficients[2])){
            coef <- mod$coefficients[2]
            ord <- mod$coefficients[1]

            upper_coef <- coef-1.28*coef(summary(mod))[4]
            lower_coef <- coef+1.28*coef(summary(mod))[4]

            upper_ord <- ord-1.28*coef(summary(mod))[3]
            lower_ord <- ord+1.28*coef(summary(mod))[3]

            ic_pente<-paste0("[",round(upper_coef,digits = 2),":",round(lower_coef,digits = 2),"]")
            ic_origine<-paste0("[",round(upper_ord,digits = 2),":",round(lower_ord,digits = 2),"]")

            }
        coef_dep <- 0
        ord_dep <- mean(y_dep)
        # if(!is.error(seglm2)){
        #   mod_dep<- seglm2
        #   rup[2]<-1
        #   coef_dep <- slope(seglm2)$x[slope_choice_dep]
        #   ord_dep <- intercept(seglm2)$x[slope_choice_dep]
        # }
        # else{
          if(!is.na(mod_dep$coefficients[2])){
            coef_dep <- mod_dep$coefficients[2]
            ord_dep <- mod_dep$coefficients[1]
          }
        yx<-data.frame(y_dep,y,x)
  list_param <- data.frame(round(coef,digits = 2),round(ord,digits = 2),ic_pente,ic_origine
                           ,round(ord-(-coef*P_TEXTMAX+P_DTCHAUD+P_TEXTMAX),digits = 2))
  colnames(list_param) <- c("Pente","Origine","IC_pente","IC_origine","Décalage parallèle")
  list_param_dep <- data.frame(round(coef_dep,digits = 2),round(ord_dep,digits = 2),
                               round(ord_dep-(-coef_dep*P_TEXTMAX+P_DTCHAUD+P_TEXTMAX),digits = 2))
  results <- list(list_param=list_param,yx=yx,mods=mod,mods_dep=mod_dep,list_param_dep=list_param_dep)}
  else{results <- NULL}}
  else{results <- NULL
  }
  return(results)}
