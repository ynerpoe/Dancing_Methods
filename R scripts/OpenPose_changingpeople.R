"This R code automates the tracking and assignment of persons in a sequence of frames generated by OpenPose.
Additionally, it incorporates an interactive mechanism to correct potential errors in the automatic assignment,
thus providing greater accuracy in data processing.

 Authors: 
 de Jonge-Hoekstra, L., & Repgen, M. H. (2020).I like to move it, move it: Package for social scientists who want to use OpenPose.
 Interdisciplinary Inspiration BSS Symposium, Groningen, Netherlands. https://doi.org/10.17605/OSF.IO/6S73D
"

library("lpSolve")
library("circular")

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

# extract data
for(n in 1:no_frames) { #1:no_frames
  
  setTxtProgressBar(pb, n)
  
  openpose_data_sub <- openpose_data[which(openpose_data$frame_no == n),]
  
  openpose_data_sub <- cbind(openpose_data_sub[,1:2],
                             apply(openpose_data_sub[,3:ncol(openpose_data_sub)], 2,
                                   func_remove_col0))
  
  no_people_sub_cur <- length(which(!is.na(openpose_data_sub[1,col_ids_x])))
  people_sub_cur <- which(!is.na(openpose_data_sub[1,col_ids_x]))
  
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
  
  comb_temp$diff.keypoints <- NA
  comb_temp$avg_eucl.dist <- NA
  comb_temp$eucl.dist_Nose <- NA
  comb_temp$eucl.dist_Neck <- NA
  comb_temp$avg_x_diff <- NA
  comb_temp$avg_y_diff <- NA
  comb_temp$avg_c_diff <- NA
  comb_temp$act.mean.angle <- NA
  comb_temp$abs.act.mean.angle <- NA
  comb_temp$diff.act.mean.angle <- NA
  comb_temp$pre.frame.avg_eucl.dist <- NA
  comb_temp$pre.diff.keypoints <- NA
  comb_temp$product.avg_eucl.dist_diff.keypoints <- NA
 
  
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
  
  comb_temp[is.na(comb_temp)] <- 0
  colnames(comb_temp) <- c("cur", "pre",
                           colnames(comb_temp)[3:15])
  
  for(s in avail_keypoints_temp) {
    id_row_prev.keypoints <- which(comb_temp$pre == s)
    
    diff_frame_temp <- n - frame.prev.keypoints$frame[which(frame.prev.keypoints$p == s)]
    
    comb_temp[id_row_prev.keypoints,3:15] <- comb_temp[id_row_prev.keypoints,3:15]/(0.6 + (0.4/diff_frame_temp))
    
    rm(id_row_prev.keypoints, diff_frame_temp)
  }
  
  assign_matrix_diff.keypoints <- matrix(as.numeric(comb_temp$diff.keypoints),
                                         nrow = length(people_sub_pre), 
                                         ncol = length(people_sub_cur),
                                         byrow = T)
  
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
  
  list_colnames_comb_temp <- colnames(comb_temp)[c(4:9,11:12)]
 
  for(b in list_colnames_comb_temp) {
    
    id_col <- which(grepl(paste0("^",b), colnames(comb_temp)) == T)
    
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
               
    } else {
      assign_matrix_total <- assign_matrix_total + eval(as.symbol(list_solutions[l]))
    }
  }
  
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
  check.for.prompt <- data.frame(col_pre = 1:no_people_total,
                                 col_post =  rep(NA, no_people_total),
                                 diff.keypoint =  rep(NA, no_people_total),
                                 avg_eucl.dist =  rep(NA, no_people_total),
                                 avg_eucl.dist2 = rep(NA, no_people_total))
   
  for(d in 1:ncol(assign_solution_total)) { # hier gebleven 21-06-2022 10:54 wat te doen met discrepantie comb_temp? 
    check.for.prompt$col_post[d] <- which(assign_solution_total[d,] > 0)
    
    prev_row_assign <- which(people_sub_pre == d)
    post_col_assign <- which(people_sub_cur == check.for.prompt$col_post[d])
    
    if(length(prev_row_assign) == 0 | length(post_col_assign) == 0) {
      check.for.prompt$diff.keypoint[d] <- NA
      check.for.prompt$avg_eucl.dist[d] <- NA
      check.for.prompt$avg_eucl.dist2[d] <- NA
      
    } else {
      
     check.for.prompt$diff.keypoint[d] <- assign_solve_diff.keypoints$costs[prev_row_assign,post_col_assign]
      check.for.prompt$avg_eucl.dist[d] <- assign_solve_avg_eucl.dist$costs[prev_row_assign,post_col_assign]
      
      check.for.prompt$avg_eucl.dist2[d] <- ifelse(check.for.prompt$avg_eucl.dist[d] == 0, NA,
                                                   min(assign_solve_avg_eucl.dist$costs[prev_row_assign,][-c(which(assign_solve_avg_eucl.dist$costs[prev_row_assign,] == check.for.prompt$avg_eucl.dist[d]),
                                                                                                             which(assign_solve_avg_eucl.dist$costs[prev_row_assign,] == 0))]))
      check.for.prompt$avg_eucl.dist2[d] <- ifelse(check.for.prompt$avg_eucl.dist2[d] == 0, NA, check.for.prompt$avg_eucl.dist2[d])
    }
    
    rm(prev_row_assign, post_col_assign)
  }
  
  check.for.prompt$diff_avg_eucl.dist <- abs(check.for.prompt$avg_eucl.dist - check.for.prompt$avg_eucl.dist2)
  
  if((length(which(na.omit(check.for.prompt$diff.keypoint) > 10)) > 1 |
      length(which(na.omit(check.for.prompt$avg_eucl.dist) > 40)) > 1) |
      length(which(check.for.prompt$avg_eucl.dist == 0)) > 1 & no_people_sub_cur > 2 |
      length(which(check.for.prompt$diff_avg_eucl.dist < 10)) > 1 #|
     
  ){
    
    print(check.for.prompt)
        
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
 
  openpose_data_pre <- openpose_data_pre.final
  no_people_sub_pre <- no_people_sub_cur
  people_sub_pre <- which(!is.na(openpose_data_pre[1,col_ids_x]))
    
  for(p in 1:length(people_sub_pre)) {
    
    p_temp <- people_sub_pre[p]
    
    keypoints_p_temp <- openpose_data_pre.final[,(p_temp*3):((p_temp*3)+2)]
    
    assign(paste0("keypoints_pre", p_temp), keypoints_p_temp)
    
    frame.prev.keypoints$frame[which(frame.prev.keypoints$p == p_temp)] <- n
    
    rm(p_temp, keypoints_p_temp)
    }
   
  rm(list=setdiff(ls(), c("parentfolder", "no_frames", "no_people_sub_pre", "people_sub_pre", 
                          "act.mean.angle.pre", "n", "openpose_data", "openpose_data_final",
                          "openpose_data_pre", "pb", "no_people_total", "col_ids_x", "avg_x_y_pre", 
                          "obj.val", "func_remove_col0", "func_count_0s_col", "func_user.input",
                          "filename_openpose_data", "frame.prev.keypoints", 
                          ls()[which(grepl("^keypoints_pre", ls()))])))
}

filename_for_export <- substr(filename_openpose_data, 1, nchar(filename_openpose_data)-30)

write.csv(openpose_data_final, paste0(filename_for_export, "openpose_FINAL.csv"), row.names = F)

