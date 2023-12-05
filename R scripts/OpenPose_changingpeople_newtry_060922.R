
# deal with changing persons in OpenPose-output

library("lpSolve")
library("circular")

# the script already works pretty well, but I think it would improve even more if we would also have the difference in number of keypoints from the previous round
# POSSIBLE improvement: when there's a difference (decline) in number of people from the previous to the current one > only use the pre_avg.eucl.dist


# prep
#parentfolder <- "Y:/student/LowCost/gmw/lisettehoekstra/Data_Music_movements_creativity/2021-2022/Data/Session1/OpenPose/OpenPose_output/"
parentfolder <- (dirname(rstudioapi::getSourceEditorContext()$path))
setwd(parentfolder)

filename_openpose_data <- list.files(pattern = "openpose_2D_nohands_noface.csv")

openpose_data <- read.csv(filename_openpose_data)

no_frames <- max(openpose_data$frame_no)
no_people_total <- (ncol(openpose_data)-2)/3
col_ids_x <- seq(3, 3*no_people_total+2, 3)

openpose_data_final <- openpose_data
openpose_data_final[,3:ncol(openpose_data_final)] <- NA

# test remove data points with c < .3
for(t in col_ids_x) {
  id_temp_small_c <- which(openpose_data[,t+2] < .3)
  
  openpose_data[id_temp_small_c,t] <- 0
  openpose_data[id_temp_small_c,t+1] <- 0
  openpose_data[id_temp_small_c,t+2] <- 0
  
  rm(id_temp_small_c)
}

pb <- txtProgressBar(min = 0, max = no_frames, style = 3)

avg_x_y_pre <- data.frame(person = 1:no_people_total,
                          avg_x = rep(NA, no_people_total),
                          avg_y = rep(NA, no_people_total),
                          code_keypoints = rep(NA, no_people_total))

obj.val <- data.frame(n = 1:no_frames,
                      obj.function = rep(NA, no_frames),
                      obj.function.cor.pp = rep(NA, no_frames))

func_remove_col0 <- function(x) {
  if(all(is.na(x))) {
    x
  } else if(sum(as.numeric(x)) == 0) { 
    rep(NA, length(x))
    # } else if (length(which(which(x == 0) == T) == 24)) {
    #     rep(NA, length(x))
  }
  else {
    x
  }
}

func_count_0s_col <- function(x) {
  if(all(is.na(x)) == T) {
    length(which(is.na(x))) >= 24
  } else {
    length(which(x == 0)) >= 24
  }
  
}

frame.prev.keypoints <- data.frame(p = 1:no_people_total,
                                   frame = rep(NA, no_people_total))

# func_user.input <- function(prompt) {
#   if(interactive()) {
#     return(readline(prompt))
#   } else {
#     cat(prompt)
#     return(readLines("stdin", n = 1))
#   }
# }

# now the columns switch around

# next step n = 5172

# extract data
for(n in 1:no_frames) { #1:no_frames
  
  setTxtProgressBar(pb, n)
  
  openpose_data_sub <- openpose_data[which(openpose_data$frame_no == n),]
  
  openpose_data_sub <- cbind(openpose_data_sub[,1:2],
                             apply(openpose_data_sub[,3:ncol(openpose_data_sub)], 2,
                                   func_remove_col0))
  
  no_people_sub_cur <- length(which(!is.na(openpose_data_sub[1,col_ids_x])))
  people_sub_cur <- which(!is.na(openpose_data_sub[1,col_ids_x]))
  
  # # # for error checken
  # openpose_data_pre <- openpose_data[which(openpose_data_final$frame_no == n-1),]
  # no_people_sub_pre <- length(which(!is.na(openpose_data_pre[1,col_ids_x])))
  # people_sub_pre <- which(!is.na(openpose_data_pre[1,col_ids_x]))
  
  if(n == 1) {
    openpose_data_final[(((n)*25)-24):((n)*25),] <- openpose_data_sub
    openpose_data_pre <- openpose_data_sub
    
    no_people_sub_pre <- no_people_sub_cur
    people_sub_pre <- people_sub_cur
    
    for(p in 1:length(people_sub_pre)) {
      
      p_temp <- people_sub_pre[p]
      
      x_temp <- ifelse(openpose_data_pre[,col_ids_x[people_sub_pre[p]]] == 0, NA, openpose_data_pre[,col_ids_x[people_sub_pre[p]]])
      y_temp <- ifelse(openpose_data_pre[,col_ids_x[people_sub_pre[p]]+1] == 0, NA, openpose_data_pre[,col_ids_x[people_sub_pre[p]]+1])
      
      avg_x_y_pre$avg_x[p_temp] <- mean(x_temp, na.rm = T)
      avg_x_y_pre$avg_y[p_temp] <- mean(y_temp, na.rm = T)
      
      avg_x_y_pre$code_keypoints[p_temp] <- paste(ifelse(openpose_data_pre[,col_ids_x[p_temp]] == 0, "x", "a"), collapse = "")
      
      rm(x_temp, y_temp)
    }
    
    rm(openpose_data_sub, no_people_sub_cur, people_sub_cur, p)
    next
  }
  
  if(no_people_sub_cur == 0) {
    openpose_data_final[(((n)*25)-24):((n)*25),] <- openpose_data_sub
    
    rm(list=setdiff(ls(), c("parentfolder", "no_frames", "no_people_sub_pre", "people_sub_pre", 
                            "act.mean.angle.pre", "n", "openpose_data", "openpose_data_final",
                            "openpose_data_pre", "pb", "no_people_total", "col_ids_x", "avg_x_y_pre", 
                            "obj.val", "func_remove_col0", "func_count_0s_col", "func_user.input",
                            "filename_openpose_data", "frame.prev.keypoints",
                            ls()[which(grepl("^keypoints_pre", ls()))])))
    
    next
  }
  
  # hier dubbel checken, want soms gekke uitkomsten. bv bij n = 848
  avail_keypoints_pre <- ls()[which(grepl("keypoints_pre", ls()))]
  avail_keypoints_pre_num <- as.numeric(unlist(lapply(avail_keypoints_pre,
                                                      function(x) substr(x, 14, 14))))
  
  if(no_people_sub_pre < no_people_total) {
    missing_people <- (1:no_people_total)[which(!(1:no_people_total %in% people_sub_pre))]
    
    avail_keypoints_temp <- missing_people[missing_people %in% avail_keypoints_pre_num]
    
    if(length(avail_keypoints_temp) > 0) {
      for(h in avail_keypoints_temp) {
        openpose_data_pre[,(h*3):((h*3)+2)] <- eval(as.symbol(paste0("keypoints_pre", h)))
      }
      
      people_sub_pre <- which(!is.na(openpose_data_pre[1,col_ids_x]))
      no_people_sub_pre <- length(people_sub_pre)
    }
  } else {
    avail_keypoints_temp <- vector()
  }
  
  #for(p in c(3,2,1)) {
  # 
  # people_sub_pre <- eval(as.symbol(paste0("people_sub_pre", p)))
  # openpose_data_pre <- eval(as.symbol(paste0("openpose_data_pre", p)))
  
  # create difference matrices for each of the bodyparts
  # maybe instead of doing this for all the keypoints, maybe it is better to do this for the average eucl dist... that seemed to work pretty well earlier
  # and for the difference in angle?
  #comb_temp <- expand.grid(people_sub_cur, people_sub_pre)
  #comb_temp <- expand.grid(1:max(people_sub_cur), 1:max(people_sub_pre))
  comb_temp <- expand.grid(people_sub_cur, people_sub_pre)
  # comb_temp <- cbind(comb_temp,
  #                    as.data.frame(matrix(NA,
  #                                         nrow = nrow(comb_temp),
  #                                         ncol = 4*25)))
  # list_colnames_comb_temp <- paste0(rep(c("x.diff_", "y.diff_", "c.diff_", "eucl.dist_"), each = 25), 
  #                                   rep(openpose_data_sub$bodyparts, 4))
  # 
  # colnames(comb_temp)[3:ncol(comb_temp)] <- list_colnames_comb_temp
  
  comb_temp$diff.keypoints <- NA
  
  comb_temp$avg_eucl.dist <- NA
  comb_temp$eucl.dist_Nose <- NA
  comb_temp$eucl.dist_Neck <- NA
  comb_temp$avg_x_diff <- NA
  comb_temp$avg_y_diff <- NA
  comb_temp$avg_c_diff <- NA
  # comb_temp$diff.no.keypoints <- NA
  # #comb_temp$mean.angle <- NA
  comb_temp$act.mean.angle <- NA
  comb_temp$abs.act.mean.angle <- NA
  comb_temp$diff.act.mean.angle <- NA
  comb_temp$pre.frame.avg_eucl.dist <- NA
  comb_temp$pre.diff.keypoints <- NA
  comb_temp$product.avg_eucl.dist_diff.keypoints <- NA
  # #comb_temp$
  
  for(c in 1:nrow(comb_temp)) { #nrow(comb_temp)
    p1_temp_sub <- comb_temp$Var1[c] #p1 in this loop refers to openpose_data_sub
    p2_temp_sub <- comb_temp$Var2[c] #p2 in this loop refers to openpose_data_pre
    
    p1_temp_x.y.c <- openpose_data_sub[,col_ids_x[p1_temp_sub]:(col_ids_x[p1_temp_sub]+2)]
    p1_temp_x.y.c[,1] <- ifelse(p1_temp_x.y.c[,3] < 0.3, NA, p1_temp_x.y.c[,1]) # treshold of c < .3 based on Hur & Bosch, 2022
    p1_temp_x.y.c[,2] <- ifelse(p1_temp_x.y.c[,3] < 0.3, NA, p1_temp_x.y.c[,2]) # treshold of c < .3 based on Hur & Bosch, 2022
    
    p2_temp_x.y.c <- openpose_data_pre[,col_ids_x[p2_temp_sub]:(col_ids_x[p2_temp_sub]+2)]
    p2_temp_x.y.c[,1] <- ifelse(p2_temp_x.y.c[,3] < 0.3, NA, p2_temp_x.y.c[,1]) # treshold of c < .3 based on Hur & Bosch, 2022
    p2_temp_x.y.c[,2] <- ifelse(p2_temp_x.y.c[,3] < 0.3, NA, p2_temp_x.y.c[,2]) # treshold of c < .3 based on Hur & Bosch, 2022
    
    p1.p2_temp_x.y.c <- cbind(p1_temp_x.y.c, p2_temp_x.y.c)
    p1.p2_temp_x.y.c$x_diff <- p1.p2_temp_x.y.c[,1] - p1.p2_temp_x.y.c[,4]
    p1.p2_temp_x.y.c$y_diff <- p1.p2_temp_x.y.c[,2] - p1.p2_temp_x.y.c[,5]
    p1.p2_temp_x.y.c$c_diff <- p1.p2_temp_x.y.c[,3] - p1.p2_temp_x.y.c[,6]
    p1.p2_temp_x.y.c$eucl.dist <- sqrt(p1.p2_temp_x.y.c$x_diff^2 + p1.p2_temp_x.y.c$y_diff^2)
    p1.p2_temp_x.y.c$angle_p1 <- atan2(p1.p2_temp_x.y.c[,1], p1.p2_temp_x.y.c[,2])
    p1.p2_temp_x.y.c$angle_p2 <- atan2(p1.p2_temp_x.y.c[,4], p1.p2_temp_x.y.c[,5])
    p1.p2_temp_x.y.c$angle.diff <- as.numeric(abs(as.circular(p1.p2_temp_x.y.c$angle_p1) - as.circular(p1.p2_temp_x.y.c$angle_p2)))
    p1.p2_temp_x.y.c$angle.diff.act <- as.numeric(as.circular(p1.p2_temp_x.y.c$angle_p1) - as.circular(p1.p2_temp_x.y.c$angle_p2))
    
    # angle_25_bp <- atan2(na.omit(p1.p2_temp_x.y.c$x_diff), na.omit(p1.p2_temp_x.y.c$y_diff))
    
    # comb_temp[c, 3:27] <- abs(p1.p2_temp_x.y.c$x_diff)
    # comb_temp[c, 28:52] <- abs(p1.p2_temp_x.y.c$y_diff)
    # comb_temp[c, 53:77] <- abs(p1.p2_temp_x.y.c$c_diff)
    # comb_temp[c, 78:102] <- p1.p2_temp_x.y.c$eucl.dist
    
    # different conceptualization of keypoints
    keypoints_p1 <- paste(ifelse(is.na(p1.p2_temp_x.y.c[,1]), "x", "a"), collapse = "")
    keypoints_p2 <- paste(ifelse(is.na(p1.p2_temp_x.y.c[,4]), "x", "a"), collapse = "")
    
    comb_temp$avg_eucl.dist[c] <- mean(p1.p2_temp_x.y.c$eucl.dist, na.rm = T)
    
    if(is.na(comb_temp$avg_eucl.dist[c])) {
      comb_temp$avg_eucl.dist[c] <-
        sqrt(
          (mean(p1.p2_temp_x.y.c[,1], na.rm = T) - mean(p1.p2_temp_x.y.c[,4], na.rm = T))^2 +
            (mean(p1.p2_temp_x.y.c[,2], na.rm = T) - mean(p1.p2_temp_x.y.c[,5], na.rm = T))^2
        )
    }
    
    comb_temp$eucl.dist_Nose[c] <- p1.p2_temp_x.y.c$eucl.dist[1]
    comb_temp$eucl.dist_Neck[c] <- p1.p2_temp_x.y.c$eucl.dist[2]
    comb_temp$avg_x_diff[c] <- mean(p1.p2_temp_x.y.c$x_diff, na.rm = T)
    comb_temp$avg_y_diff[c] <- mean(p1.p2_temp_x.y.c$y_diff, na.rm = T)
    comb_temp$avg_c_diff[c] <- mean(p1.p2_temp_x.y.c$c_diff, na.rm = T)
    comb_temp$diff.keypoints[c] <- as.numeric(adist(keypoints_p1, keypoints_p2))
    
    # comb_temp$mean.angle[c] <- as.numeric(mean.circular(circular(as.numeric(na.omit(p1.p2_temp_x.y.c$angle.diff))))) 
    comb_temp$act.mean.angle[c] <- as.numeric(mean.circular(circular(as.numeric(na.omit(p1.p2_temp_x.y.c$angle.diff.act)))))
    comb_temp$abs.act.mean.angle[c] <- abs(as.numeric(mean.circular(circular(as.numeric(na.omit(p1.p2_temp_x.y.c$angle.diff.act))))))
    # 
    comb_temp$diff.act.mean.angle[c] <- abs(ifelse(n > 2,
                                                   as.numeric(as.circular(act.mean.angle.pre[comb_temp$Var1[c]]) - as.circular(comb_temp$act.mean.angle[c])),
                                                   0))
    
    avg_x_temp <- mean(p1.p2_temp_x.y.c[,1], na.rm =T)
    avg_y_temp <- mean(p1.p2_temp_x.y.c[,2], na.rm =T)
    
    comb_temp$pre.frame.avg_eucl.dist[c] <- sqrt((avg_x_temp-avg_x_y_pre$avg_x[which(avg_x_y_pre$person == p2_temp_sub)])^2 +
                                                   (avg_y_temp-avg_x_y_pre$avg_y[which(avg_x_y_pre$person == p2_temp_sub)])^2)
    
    comb_temp$pre.diff.keypoints[c] <- as.numeric(adist(keypoints_p1, avg_x_y_pre$code_keypoints[which(avg_x_y_pre$person == p2_temp_sub)]))
    comb_temp$product.avg_eucl.dist_diff.keypoints <- comb_temp$avg_eucl.dist * (comb_temp$diff.keypoints/25)
    
    rm(p1_temp_sub, p2_temp_sub, p1_temp_x.y.c, p2_temp_x.y.c, p1.p2_temp_x.y.c, keypoints_p1, keypoints_p2, avg_x_temp, avg_y_temp)
  }
  
  #comb_temp$costs <- comb_temp$avg_eucl.dist*comb_temp$diff.no.keypoints
  
  comb_temp[is.na(comb_temp)] <- 0
  colnames(comb_temp) <- c("cur", "pre",
                           colnames(comb_temp)[3:15])
  
  for(s in avail_keypoints_temp) {
    id_row_prev.keypoints <- which(comb_temp$pre == s)
    
    diff_frame_temp <- n - frame.prev.keypoints$frame[which(frame.prev.keypoints$p == s)]
    
    comb_temp[id_row_prev.keypoints,3:15] <- comb_temp[id_row_prev.keypoints,3:15]/(0.6 + (0.4/diff_frame_temp))
    
    rm(id_row_prev.keypoints, diff_frame_temp)
  }
  
  # #check change
  # comb_temp$avg_eucl.dist <- ifelse(comb_temp$diff.keypoints > 10, 100, comb_temp$avg_eucl.dist)
  # comb_temp$eucl.dist_Nose <- ifelse(comb_temp$diff.keypoints > 10, 100, comb_temp$eucl.dist_Nose)
  # comb_temp$eucl.dist_Neck <- ifelse(comb_temp$diff.keypoints > 10, 100, comb_temp$eucl.dist_Neck)
  
  # for(c in 1:nrow(comb_temp)) {
  #   if(comb_temp$eucl.dist_Nose[c] > 100) {
  #     comb_temp[c,3:14] <- 0
  #   }
  # }
  
  # first calculate assign matrix based on number of keypoints. This will be an important variable 
  # assign_matrix_diff.keypoints <- matrix(as.numeric(comb_temp$diff.keypoints),
  #                                        nrow = max(people_sub_pre), 
  #                                        ncol = max(people_sub_cur),
  #                                        byrow = T)
  assign_matrix_diff.keypoints <- matrix(as.numeric(comb_temp$diff.keypoints),
                                         nrow = length(people_sub_pre), 
                                         ncol = length(people_sub_cur),
                                         byrow = T)
  # 
  # if(max(people_sub_cur) > max(people_sub_pre)) {
  #   assign_matrix_diff.keypoints <- rbind(assign_matrix_diff.keypoints,
  #                                         matrix(0, nrow = max(people_sub_cur)-max(people_sub_pre), ncol = max(people_sub_cur)))
  # }
  # 
  # if(max(people_sub_pre) > max(people_sub_cur)) {
  #   #id_col_temp <- which(!people_sub_pre %in% 1:no_people_sub_cur))
  #   assign_matrix_diff.keypoints <- cbind(assign_matrix_diff.keypoints, #check this, whether the empty row should be at the end
  #                                         matrix(0, ncol = max(people_sub_pre)-max(people_sub_cur), nrow = max(people_sub_pre)))
  # }
  
  if(length(people_sub_cur) > length(people_sub_pre)) {
    assign_matrix_diff.keypoints <- rbind(assign_matrix_diff.keypoints,
                                          matrix(0, nrow = length(people_sub_cur)-length(people_sub_pre), ncol = length(people_sub_cur)))
  }
  
  if(length(people_sub_pre) > length(people_sub_cur)) {
    #id_col_temp <- which(!people_sub_pre %in% 1:no_people_sub_cur))
    assign_matrix_diff.keypoints <- cbind(assign_matrix_diff.keypoints, #check this, whether the empty row should be at the end
                                          matrix(0, ncol = length(people_sub_pre)-length(people_sub_cur), nrow = length(people_sub_pre)))
  }
  
  
  
  assign_solve_diff.keypoints <- lp.assign(assign_matrix_diff.keypoints)
  assign_imp.solution_diff.keypoints <- assign_solve_diff.keypoints$solution
  
  #The value of the objective function is the total cost over all variables that are assigned the value 1 by the solver.
  
  # assign(paste0("assign_imp.solution_diff.keypoints", p), assign_imp.solution_diff.keypoints)
  # 
  # rm(assign_matrix_diff.keypoints, assign_solve_diff.keypoints, assign_imp.solution_diff.keypoints)
  # 
  # if(!p == 1) {
  #   rm(comb_temp)
  #}
  # }
  
  #id_nonNAs <- which(as.numeric(apply(comb_temp[,3:ncol(comb_temp)], 2,
  # function(x) any(is.na(x)))) == F)
  
  #but then we need like 25*4 assign matrixes and solves per row, times the number of possible combinations.... that's a lot.
  
  list_colnames_comb_temp <- colnames(comb_temp)[c(4:9,11:12)]
  #list_colnames_comb_temp <- colnames(comb_temp)[c(4:9)]
  
  for(b in list_colnames_comb_temp) {
    
    id_col <- which(grepl(paste0("^",b), colnames(comb_temp)) == T)
    
    # assign_matrix_temp <- matrix(as.numeric(comb_temp[,id_col]),
    #                              nrow = max(people_sub_pre),
    #                              ncol = max(people_sub_cur),
    #                              byrow = T)
    # 
    # if(max(people_sub_cur) > max(people_sub_pre)) {
    #   assign_matrix_temp <- rbind(assign_matrix_temp,
    #                               matrix(0, nrow = max(people_sub_cur)-max(people_sub_pre), ncol = max(people_sub_cur)))
    # }
    # 
    # if(max(people_sub_pre) > max(people_sub_cur)) {
    #   assign_matrix_temp <- cbind(assign_matrix_temp,
    #                               matrix(0, ncol = max(people_sub_pre)-max(people_sub_cur), nrow = max(people_sub_pre)))
    # }
    
    assign_matrix_temp <- matrix(as.numeric(comb_temp[,id_col]),
                                 nrow = length(people_sub_pre),
                                 ncol = length(people_sub_cur),
                                 byrow = T)
    
    if(length(people_sub_cur) > length(people_sub_pre)) {
      assign_matrix_temp <- rbind(assign_matrix_temp,
                                  matrix(0, nrow = length(people_sub_cur)-length(people_sub_pre), ncol = length(people_sub_cur)))
    }
    
    if(length(people_sub_pre) > length(people_sub_cur)) {
      assign_matrix_temp <- cbind(assign_matrix_temp,
                                  matrix(0, ncol = length(people_sub_pre)-length(people_sub_cur), nrow = length(people_sub_pre)))
    }
    
    assign_solve_temp <- lp.assign(assign_matrix_temp)
    assign_solution_temp <- assign_solve_temp$solution
    
    assign(paste0("assign_solve_", b), assign_solve_temp)
    assign(paste0("assign_solution_", b), assign_solution_temp)
    
    rm(id_col, assign_matrix_temp, assign_solve_temp, assign_solution_temp)
  }
  
  
  
  # assign_matrix_eucl.dist <- matrix(as.numeric(comb_temp$avg_eucl.dist),
  #                                   nrow = max(people_sub_pre), 
  #                                   ncol = max(people_sub_cur),
  #                                   byrow = T)
  # 
  # if(max(people_sub_cur) > max(people_sub_pre)) {
  #   assign_matrix_eucl.dist <- rbind(assign_matrix_eucl.dist,
  #                                         matrix(0, nrow = max(people_sub_cur)-max(people_sub_pre), ncol = max(people_sub_cur)))
  # }
  # 
  # if(max(people_sub_pre) > max(people_sub_cur)) {
  #   #id_col_temp <- which(!people_sub_pre %in% 1:no_people_sub_cur))
  #   assign_matrix_eucl.dist <- cbind(assign_matrix_eucl.dist, #check this, whether the empty row should be at the end
  #                                         matrix(0, ncol = max(people_sub_pre)-max(people_sub_cur), nrow = max(people_sub_pre)))
  # }
  # 
  # 
  # assign_matrix_diff.angle <- matrix(as.numeric(comb_temp$abs.act.mean.angle),
  #                                    nrow = max(people_sub_pre), 
  #                                    ncol = max(people_sub_cur),
  #                                    byrow = T)
  # 
  # if(max(people_sub_cur) > max(people_sub_pre)) {
  #   assign_matrix_diff.angle <- rbind(assign_matrix_diff.angle,
  #                                    matrix(0, nrow = max(people_sub_cur)-max(people_sub_pre), ncol = max(people_sub_cur)))
  # }
  # 
  # if(max(people_sub_pre) > max(people_sub_cur)) {
  #   #id_col_temp <- which(!people_sub_pre %in% 1:no_people_sub_cur))
  #   assign_matrix_diff.angle <- cbind(assign_matrix_diff.angle, #check this, whether the empty row should be at the end
  #                                    matrix(0, ncol = max(people_sub_pre)-max(people_sub_cur), nrow = max(people_sub_pre)))
  # }
  # 
  # assign_matrix_diff.direction <- matrix(as.numeric(comb_temp$diff.act.mean.angle),
  #                                        nrow = max(people_sub_pre), 
  #                                        ncol = max(people_sub_cur),
  #                                        byrow = T)
  # 
  # if(max(people_sub_cur) > max(people_sub_pre)) {
  #   assign_matrix_diff.direction <- rbind(assign_matrix_diff.direction,
  #                                    matrix(0, nrow = max(people_sub_cur)-max(people_sub_pre), ncol = max(people_sub_cur)))
  # }
  # 
  # if(max(people_sub_pre) > max(people_sub_cur)) {
  #   #id_col_temp <- which(!people_sub_pre %in% 1:no_people_sub_cur))
  #   assign_matrix_eucl.dist <- cbind(assign_matrix_eucl.dist, #check this, whether the empty row should be at the end
  #                                    matrix(0, ncol = max(people_sub_pre)-max(people_sub_cur), nrow = max(people_sub_pre)))
  # }
  # 
  # assign_solve_eucl.dist <- lp.assign(assign_matrix_eucl.dist)
  # assign_solution_eucl.dist <- assign_solve_eucl.dist$solution
  # assign_objval_eucl.dist <- assign_solve_eucl.dist$objval # see whether we can do something with this
  # 
  # assign_solve_diff.keypoints <- lp.assign(assign_matrix_diff.keypoints)
  # assign_solution_diff.keypoints <- assign_solve_diff.keypoints$solution
  # assign_objval_diff.keypoints <- assign_solve_diff.keypoints$objval # see whether we can do something with this
  # 
  # assign_solve_diff.angle <- lp.assign(assign_matrix_diff.keypoints)
  # assign_solution_diff.angle <- assign_solve_diff.angle$solution
  # assign_objval_diff.angle <- assign_solve_diff.angle$objval # see whether we can do something with this
  
  # if(n > 2) {
  #   assign_solve_diff.direction <- lp.assign(assign_matrix_diff.direction)
  #   assign_solution_diff.direction <- assign_solve_diff.direction$solution
  #   assign_objval_diff.direction <- assign_solve_diff.direction$objval # see whether we can do something with this
  # }
  
  # cols_with_most_0s_sub <- apply(openpose_data_sub[,3:ncol(openpose_data_sub)], 2,
  #                                func_count_0s_col)
  # cols_with_most_0s_pre <- apply(openpose_data_pre[,3:ncol(openpose_data_pre)], 2,
  #                                func_count_0s_col)
  
  # if(max(people_sub_pre) > max(people_sub_cur)) {
  #   #assign_matrix_total <- assign_solution_pre.frame.avg_eucl.dist * 3 + assign_solution_pre.diff.keypoints #+ assign_imp.solution_diff.keypoints*2
  #   
  #   list_solutions <- ls()[grepl("assign_solution", ls())]
  #   
  #   for(l in 1:length(list_solutions)) {
  #     if(l == 1) {
  #       assign_matrix_total <- eval(as.symbol(list_solutions[l]))
  #     } 
  #     
  #     if (l == 3) {
  #       
  #       assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*3
  #     } 
  #     
  #     
  #     if(l == 7|l == 8) {
  #       assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*3
  #       
  #     } 
  #     
  #     if(l == 9) {
  #       assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*5
  #     }
  #     
  #     if(l == 10) {
  #       assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*10
  #     }
  #     
  #     if(l == 11) {
  #       assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*3
  #       
  #     } else {
  #       assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))
  #     }
  #   }
  # } else {
  #   
  #   list_solutions <- ls()[grepl("assign_solution", ls())]
  #   
  #   for(l in 1:length(list_solutions)) {
  #     if(l == 1) {
  #       assign_matrix_total <- eval(as.symbol(list_solutions[l]))
  #     } 
  #     
  #     if (l == 3) {
  #       
  #       assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*10
  #     } 
  #     
  #     
  #     if(l == 7|l == 8|l == 10) {
  #       assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*5
  #       
  #     } 
  #     
  #     if(l == 11) {
  #       assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*3
  #       
  #     } else {
  #       assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))
  #     }
  #   }
  # }
  
  # if(length(people_sub_pre) > length(people_sub_cur)) {
  #   #assign_matrix_total <- assign_solution_pre.frame.avg_eucl.dist * 3 + assign_solution_pre.diff.keypoints #+ assign_imp.solution_diff.keypoints*2
  #   
  #   list_solutions <- ls()[grepl("assign_solution", ls())]
  #   
  #   for(l in 1:length(list_solutions)) {
  #     if(l == 1) {
  #       assign_matrix_total <- eval(as.symbol(list_solutions[l]))
  #     }
  #     #
  #     # if (l == 3) {
  #     #
  #     #   assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*3
  #     # }
  #     #
  #     #
  #     # if(l == 7|l == 8) {
  #     #   assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*3
  #     #
  #     # }
  #     #
  #     # if(l == 9) {
  #     #   assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*5
  #     # }
  #     
  #     if(l == 3) {
  #       assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*10
  #       #}
  #       
  #       # if(l == 11) {
  #       #   assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*3
  #       
  #     } else {
  #       assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))
  #     }
  #     # }
  #   }
  # } else {
  
  list_solutions <- ls()[grepl("assign_solution", ls())]
  
  for(l in 1:length(list_solutions)) {
    if(l == 1) {
      assign_matrix_total <- eval(as.symbol(list_solutions[l]))
    }
    
    if(l == 3) {
      assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l])) * 10
    }
    
    if(l == 8 | l == 7) {
      assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l])) * 5
      
      # if (l == 3) {
      #
      #   assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*10
      # }
      #
      #
      # if(l == 7|l == 8|l == 10) {
      #   assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*5
      #
      # }
      #
      # if(l == 11) {
      #   assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))*3
      
    } else {
      assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))
    }
  }
  # }
  
  # add assign_solution of diff.keypoints with extra weight
  
  # if(any(cols_with_most_0s_sub == T) | any(cols_with_most_0s_pre == T)) {
  #   assign_matrix_total <- assign_matrix_total + assign_imp.solution_diff.keypoints*15
  # } else {
  assign_matrix_total <- assign_matrix_total + assign_imp.solution_diff.keypoints*10
  # }
  
  # weigh prev columns less and less with time? FOR LATER 
  
  
  # if(n < 3) {
  #   assign_matrix_total <- assign_solution_eucl.dist + assign_solution_diff.keypoints + assign_solution_diff.angle
  # } else {
  #   assign_matrix_total <- assign_solution_eucl.dist + assign_solution_diff.keypoints + assign_solution_diff.angle + assign_solution_diff.direction
  # }
  
  assign_solve_total <- lp.assign(assign_matrix_total, direction = "max")
  
  assign_solution_total <- assign_solve_total$solution
  
  # remove extra rows again if there is a difference between length(people_sub_cur) en length(people_sub_pre)
  if(length(people_sub_cur) > length(people_sub_pre)) {
    diff_length_temp <- length(people_sub_cur) - length(people_sub_pre)
    
    assign_solution_total <- matrix(assign_solution_total[1:(nrow(assign_solution_total)-diff_length_temp),],
                                    nrow = nrow(assign_solution_total)-diff_length_temp)
    
    rm(diff_length_temp)
  }
  
  if(length(people_sub_cur) < length(people_sub_pre)) {
    diff_length_temp <- length(people_sub_pre) - length(people_sub_cur)
    
    assign_solution_total <- matrix(assign_solution_total[,1:(ncol(assign_solution_total)-diff_length_temp)],
                                    ncol = ncol(assign_solution_total)-diff_length_temp)
  }
  
  # complete assign_solution to include empty rows/cols
  if(no_people_sub_cur < no_people_total) {
    id_miss_sub_cur <- which(!(1:no_people_total %in% people_sub_cur))
    
    # id_miss_sub_cur <- ifelse(no_people_total - nrow(assign_solution_total) < length(id_miss_sub_cur),
    #                           id_miss_sub_cur[length(id_miss_sub_cur) - ((no_people_total - ncol(assign_solution_total) - 1))],
    #                           id_miss_sub_cur)
    
    #order_sub_cur <- c(people_sub_cur, id_miss_sub_cur)
    order_sub_cur <- rep(NA, no_people_total)
    order_sub_cur[id_miss_sub_cur] <- seq(no_people_sub_cur+1, no_people_total, 1)
    order_sub_cur[-id_miss_sub_cur] <- 1:no_people_sub_cur
    
    assign_solution_total <- cbind(assign_solution_total,
                                   matrix(0, nrow = nrow(assign_solution_total), ncol = length(id_miss_sub_cur)))
    
    assign_solution_total <- matrix(assign_solution_total[,order_sub_cur],
                                    nrow = nrow(assign_solution_total),
                                    ncol = ncol(assign_solution_total))
  } else {
    order_sub_cur <- people_sub_cur
  }
  
  if(no_people_sub_pre < no_people_total) {
    id_miss_sub_pre <- which(!(1:no_people_total %in% people_sub_pre))
    
    # check for whether extra row was already added
    
    # if(no_people_total - nrow(assign_solution_total) < length(id_miss_sub_pre)) {
    #   # id_miss_sub_pre <-   id_miss_sub_pre[length(id_miss_sub_pre) - ((no_people_total - nrow(assign_solution_total) - 1))] # maar het is ingewikkelder dan dit
    #   
    #   id_avai_sub_pre <- which(1:nrow(assign_solution_total) %in% people_sub_pre)
    #   id_miss_sub_pre2 <- which(!(1:nrow(assign_solution_total) %in% id_avai_sub_pre))
    #   
    #   id_miss_sub_pre3 <- id_miss_sub_pre[-which(id_miss_sub_pre == id_miss_sub_pre2)]
    #   
    #   order_sub_pre <- rep(NA, no_people_total)
    #   order_sub_pre[id_miss_sub_pre3] <- seq(nrow(assign_solution_total)+1, no_people_total, 1)
    #   order_sub_pre[id_miss_sub_pre2] <- 
    #   order_sub_pre[-id_miss_sub_pre] <- 1:no_people_sub_pre
    # }
    
    
    # dit klopt niet....? nu wel?
    #order_sub_pre <- c(people_sub_pre, id_miss_sub_pre)
    order_sub_pre <- rep(NA, no_people_total)
    order_sub_pre[id_miss_sub_pre] <- seq(no_people_sub_pre+1, no_people_total, 1)
    order_sub_pre[-id_miss_sub_pre] <- 1:no_people_sub_pre
    
    assign_solution_total <- rbind(assign_solution_total,
                                   matrix(0, ncol = no_people_total, nrow = length(id_miss_sub_pre)))
    
    assign_solution_total <- matrix(assign_solution_total[order_sub_pre,],
                                    nrow = nrow(assign_solution_total),
                                    ncol = ncol(assign_solution_total))
    #test <- assign_solution_total[order_sub_pre,]
  } else {
    order_sub_pre <- people_sub_pre
  }
  
  # check for empty rows/cols
  id_empty_col <- which(apply(assign_solution_total, 2, sum) == 0)
  id_empty_row <- which(apply(assign_solution_total, 1, sum) == 0)
  
  for(k in 1:length(id_empty_col)) {
    assign_solution_total[id_empty_row[k], id_empty_col[k]] <- 1
  }
  
  # check for large differences in keypoints and large differences in avg_pixels and create manual prompt for these instances.
  # hier gebleven, werkt nog niet perfect...
  check.for.prompt <- data.frame(col_pre = 1:no_people_total,
                                 col_post =  rep(NA, no_people_total),
                                 diff.keypoint =  rep(NA, no_people_total),
                                 avg_eucl.dist =  rep(NA, no_people_total),
                                 avg_eucl.dist2 = rep(NA, no_people_total))
  #pre.frame.avg_eucl.dist = rep(NA, no_people_total))
  
  # id_pre <- which(assign_solution_total[x,] > 0) #nieuw geprobeerd om n = 296 op te lossen, dus 
  
  for(d in 1:ncol(assign_solution_total)) { # hier gebleven 21-06-2022 10:54 wat te doen met discrepantie comb_temp? 
    check.for.prompt$col_post[d] <- which(assign_solution_total[d,] > 0)
    
    prev_row_assign <- which(people_sub_pre == d)
    post_col_assign <- which(people_sub_cur == check.for.prompt$col_post[d])
    
    if(length(prev_row_assign) == 0 | length(post_col_assign) == 0) {
      check.for.prompt$diff.keypoint[d] <- NA
      check.for.prompt$avg_eucl.dist[d] <- NA
      check.for.prompt$avg_eucl.dist2[d] <- NA
      
    } else {
      
      # check.for.prompt$diff.keypoint[d] <- assign_solve_diff.keypoints$costs[check.for.prompt$col_post[d],d]
      # check.for.prompt$avg_eucl.dist[d] <- assign_solve_avg_eucl.dist$costs[check.for.prompt$col_post[d],d]
      
      check.for.prompt$diff.keypoint[d] <- assign_solve_diff.keypoints$costs[prev_row_assign,post_col_assign]
      check.for.prompt$avg_eucl.dist[d] <- assign_solve_avg_eucl.dist$costs[prev_row_assign,post_col_assign]
      #check.for.prompt$pre.frame.avg_eucl.dist[d] <- assign_solve_pre.frame.avg_eucl.dist$costs[[prev_row_assign,post_col_assign]]
      
      check.for.prompt$avg_eucl.dist2[d] <- ifelse(check.for.prompt$avg_eucl.dist[d] == 0, NA,
                                                   min(assign_solve_avg_eucl.dist$costs[prev_row_assign,][-c(which(assign_solve_avg_eucl.dist$costs[prev_row_assign,] == check.for.prompt$avg_eucl.dist[d]),
                                                                                                             which(assign_solve_avg_eucl.dist$costs[prev_row_assign,] == 0))]))
      check.for.prompt$avg_eucl.dist2[d] <- ifelse(check.for.prompt$avg_eucl.dist2[d] == 0, NA, check.for.prompt$avg_eucl.dist2[d])
    }
    
    rm(prev_row_assign, post_col_assign)
  }
  
  check.for.prompt$diff_avg_eucl.dist <- abs(check.for.prompt$avg_eucl.dist - check.for.prompt$avg_eucl.dist2)
  
  #17-06-22 change to not check for things whereby the values are clear for most of the values. It used to be :
  # any(na.omit(check.for.prompt$diff.keypoint) > 10)) | any(na.omit(check.for.prompt$avg_eucl.dist) > 40)) | length(which(check.for.prompt$avg_eucl.dist == 0)) > 1)
  if((length(which(na.omit(check.for.prompt$diff.keypoint) > 10)) > 1 |
      length(which(na.omit(check.for.prompt$avg_eucl.dist) > 40)) > 1) |
     length(which(check.for.prompt$avg_eucl.dist == 0)) > 1 & no_people_sub_cur > 2 |
     length(which(check.for.prompt$diff_avg_eucl.dist < 10)) > 1 #|
     #(length(which(check.for.prompt$pre.frame.avg_eucl.dist > 40 & 
     #              (is.na(check.for.prompt$avg_eucl.dist) | check.for.prompt$avg_eucl.dist == 0))) > 0)) 
  ){
    
    print(check.for.prompt)
    
    # order_cols_correct <- 0
    # 
    # while(order_cols_correct == 0) {
    if(length(avail_keypoints_temp) > 0) {
      
      for(j in length(avail_keypoints_temp)) {
        
        prev_frame_j <- frame.prev.keypoints$frame[which(frame.prev.keypoints$p == avail_keypoints_temp[j])]
        
        print(paste0("WARNING: the keypoints from p ", avail_keypoints_temp[j], " are from frame ", prev_frame_j))
        
        rm(prev_frame_j)
      }
    }
    
    print("openpose_data_pre :")
    print(openpose_data_pre)
    print("---------")
    print("openpose_data_sub :")
    print(openpose_data_sub)
    print("---------") # hier gebleven 160622
    
    print("The suggested order is:")
    print(check.for.prompt[,1:2])
    print("If this order is correct, type '6'")
    print("If this order is incorrect, provide the correct order of participants in openpose_data_sub")
    print("If you made a mistake in the provided order, type '5' to start over")
    
    y <- 0
    
    while(y == 0) {
      order_cols <- vector()
      
      for(f in 1:no_people_total){
        #print(paste0("col.number in openpose_data_pre ", f))
        
        people_for_menu <- as.character(1:no_people_total)
        
        order_temp <- menu(c(people_for_menu, "start over", "order is correct"),
                           title = paste0("participant ", f, " in openpose_data_pre is participant ... in openpose_data_sub"))
        
        if(order_temp == (length(people_for_menu) + 2)) {
          order_cols <- as.numeric(check.for.prompt$col_post)
          
          y <- 1
          
          break
        }
        
        if(order_temp == (length(people_for_menu) + 1)) {
          break
        }
        
        #check check
        order_cols <- c(order_cols, order_temp)
        
        if(any(duplicated(order_cols))) {
          print("WARNING: You provided the same column number twice. Please start over.")
          break
        }
        
        if(length(order_cols) > (length(people_for_menu) - 1)) {
          y <- 1
        }
        
        rm(order_temp)
      }
    }    
    
    #   
    #   if(length(unique(order_cols)) == max(no_people_sub_cur, no_people_sub_pre)) {
    #     order_cols_correct <- 1
    #   } else {
    #     print("ERROR: Duplicate column numbers! Repeat procedure")
    #     rm(order_cols)
    #   }
    # }
    
    
    assign_solution_total <- matrix(0, nrow = no_people_total, ncol = no_people_total)
    
    for(s in 1:ncol(assign_solution_total)) {
      assign_solution_total[s,order_cols[s]] <- 1
    }
  }
  
  obj.val$obj.function[n] <- assign_solve_total$objval
  obj.val$obj.function.cor.pp[n] <- assign_solve_total$objval/no_people_sub_cur
  
  openpose_data_pre.final <- openpose_data_sub
  openpose_data_pre.final[,3:ncol(openpose_data_pre.final)] <- NA
  
  act.mean.angle.pre <- rep(NA, no_people_sub_cur)
  
  for(x in 1:nrow(assign_solution_total)) {
    #id_pre <- which(assign_solution_total[,x] > 0)
    id_pre <- which(assign_solution_total[x,] > 0) #nieuw geprobeerd om n = 296 op te lossen
    
    openpose_data_pre.final[,x*3] <- openpose_data_sub[,id_pre*3]
    openpose_data_pre.final[,(x*3)+1] <- openpose_data_sub[,(id_pre*3)+1]
    openpose_data_pre.final[,(x*3)+2] <- openpose_data_sub[,(id_pre*3)+2]
    
    act.mean.angle.temp <- comb_temp$act.mean.angle[which(comb_temp$Var1 == x & comb_temp$Var2 == id_pre)]
    
    act.mean.angle.pre[x] <- ifelse(length(act.mean.angle.temp) > 0, act.mean.angle.temp, NA)
    
    rm(id_pre, act.mean.angle.temp)
  }
  
  openpose_data_final[(((n)*25)-24):((n)*25),] <- openpose_data_pre.final
  
  # openpose_data_pre1 <- openpose_data_pre.final
  # no_people_sub_pre1 <- no_people_sub_cur
  # people_sub_pre1 <- which(!is.na(openpose_data_pre1[1,col_ids_x]))
  
  openpose_data_pre <- openpose_data_pre.final
  no_people_sub_pre <- no_people_sub_cur
  people_sub_pre <- which(!is.na(openpose_data_pre[1,col_ids_x]))
  
  # calculate average x and y coordinates of keypoints of people, and include this as one of the variables for the assignment
  # this needs to change
  for(p in 1:length(people_sub_pre)) {
    
    p_temp <- people_sub_pre[p]
    
    keypoints_p_temp <- openpose_data_pre.final[,(p_temp*3):((p_temp*3)+2)]
    
    assign(paste0("keypoints_pre", p_temp), keypoints_p_temp)
    
    frame.prev.keypoints$frame[which(frame.prev.keypoints$p == p_temp)] <- n
    
    rm(p_temp, keypoints_p_temp)
    
    # x_temp <- ifelse(openpose_data_pre[,col_ids_x[people_sub_pre[p]]] == 0, NA, openpose_data_pre[,col_ids_x[people_sub_pre[p]]])
    # y_temp <- ifelse(openpose_data_pre[,col_ids_x[people_sub_pre[p]]+1] == 0, NA, openpose_data_pre[,col_ids_x[people_sub_pre[p]]+1])
    # 
    # avg_x_y_pre$avg_x[p_temp] <- mean(x_temp, na.rm = T)
    # avg_x_y_pre$avg_y[p_temp] <- mean(y_temp, na.rm = T)
    # 
    # avg_x_y_pre$code_keypoints[p_temp] <- paste(ifelse(openpose_data_pre[,col_ids_x[p_temp]] == 0, "x", "a"), collapse = "")
    # 
    # rm(x_temp, y_temp)
  }
  
  
  # if(n > 2) {
  #   openpose_data_pre2 <- openpose_data_final[(((n-1)*25)-24):((n-1)*25),]
  #   people_sub_pre2 <- which(!is.na(openpose_data_pre2[1,col_ids_x]))
  #   no_people_pe2 <- length(people_sub_pre2)
  # }
  # 
  # if(n > 3) {  
  #   openpose_data_pre3 <- openpose_data_final[(((n-2)*25)-24):((n-2)*25),] # check this out later, go multiple frames back
  #   people_sub_pre3 <- which(!is.na(openpose_data_pre3[1,col_ids_x]))
  #   no_people_pe3 <- length(people_sub_pre3)
  # }
  
  
  rm(list=setdiff(ls(), c("parentfolder", "no_frames", "no_people_sub_pre", "people_sub_pre", 
                          "act.mean.angle.pre", "n", "openpose_data", "openpose_data_final",
                          "openpose_data_pre", "pb", "no_people_total", "col_ids_x", "avg_x_y_pre", 
                          "obj.val", "func_remove_col0", "func_count_0s_col", "func_user.input",
                          "filename_openpose_data", "frame.prev.keypoints", 
                          ls()[which(grepl("^keypoints_pre", ls()))])))
}

filename_for_export <- substr(filename_openpose_data, 1, nchar(filename_openpose_data)-30)

write.csv(openpose_data_final, paste0(filename_for_export, "openpose_FINAL.csv"), row.names = F)

