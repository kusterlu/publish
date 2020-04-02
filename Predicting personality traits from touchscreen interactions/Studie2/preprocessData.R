library(dplyr)

preprocessFBData <- function(FB.Werte, VPs, users){
  
  FB.Werte <- FB.Werte[FB.Werte$VP %in% VPs,]
  
  # # Invertierungen
  # FB.Werte$PQ_1 <- 8 - FB.Werte$p_PQ_1
  # FB.Werte$PQ_2 <- 8 - FB.Werte$p_PQ_2
  # FB.Werte$PQ_3 <- 8 - FB.Werte$p_PQ_3
  # FB.Werte$HQI_1 <- 8 - FB.Werte$p_HQI_1
  # FB.Werte$ATT_2 <- 8 - FB.Werte$p_ATT_2
  
  # Zusammenfassungen
  # FB.Werte$PQ <- rowMeans(cbind(FB.Werte$PQ_1, FB.Werte$PQ_2, FB.Werte$PQ_3, FB.Werte$PQ_4))
  # FB.Werte$ATT <- rowMeans(cbind(FB.Werte$ATT_1, FB.Werte$ATT_2))
  # FB.Werte$HQI <- rowMeans(cbind(FB.Werte$HQI_1, FB.Werte$HQI_2))
  # FB.Werte$HQS <- rowMeans(cbind(FB.Werte$HQS_1, FB.Werte$HQS_2))
  # FB.Werte$HQ <- rowMeans(cbind(FB.Werte$HQI_1, FB.Werte$HQI_2, FB.Werte$HQS_1, FB.Werte$HQS_2))
  # 
  # FB.Werte <- ddply(FB.Werte, c("VP"), transform, PQ.std = scale(PQ))
  # FB.Werte <- ddply(FB.Werte, c("VP"), transform, HQ.std = scale(HQ))
  # FB.Werte <- ddply(FB.Werte, c("VP"), transform, ATT.std = scale(ATT))
  
  
  ####
  # Original-Code: 
  # FB.Werte$VP.Touches <- rep(users, each=12)
  # Ersatz:
  FB.Werte$VP.Touches <- FB.Werte$UserId
  
  return(FB.Werte)
}

preprocessDemographicsData <- function(FB.Demograph, VPs, users){
  
  FB.Demograph <- FB.Demograph[FB.Demograph$VP %in% VPs,]
  #L FB.Demograph$VP.Touches <- users
  
  # FB.Demograph$ownsTablet <- toString(FB.Demograph$ownsTablet)
  # FB.Demograph$ownsTablet <- tolower(FB.Demograph$ownsTablet)
  # FB.Demograph$ownsTablet <- as.factor(FB.Demograph$ownsTablet)
  
  # FB.Demograph$Spielzeit <- NULL
  # FB.Demograph$Neutralisierungszeit <- NULL
  # FB.Demograph$Quiz.Freezing.Prob <- NULL
  # FB.Demograph$X..1x.in.der.Stunde <- NULL
  # FB.Demograph$X1x.in.der.Stunde <- NULL
  # FB.Demograph$X1x.am.Tag <- NULL
  # FB.Demograph$X1x.in.der.Woche <- NULL
  # FB.Demograph$ab.und.zu <- NULL
  # FB.Demograph$X..1x.in.der.Stunde.1 <- NULL
  # FB.Demograph$X1x.in.der.Stunde.1 <- NULL
  # FB.Demograph$X1x.am.Tag.1 <- NULL
  # FB.Demograph$X1x.in.der.Woche.1 <- NULL
  # FB.Demograph$ab.und.zu.1 <- NULL
  # FB.Demograph$ungest_rt <- NULL
  # FB.Demograph$Freezing <- NULL
  # FB.Demograph$TinyIcons <- NULL
  # FB.Demograph$ungest_rt.1 <- NULL
  # FB.Demograph$Freezing.1 <- NULL
  # FB.Demograph$TinyIcons.1 <- NULL
  
  # FB.Demograph.englishNames <- c("VP", "age", "sex", "occupation", "graduation", "ownsSmartphone", "SmartphoneUsageFrequency", "SmartphoneType", "ownsTablet", "TabletUsageFrequency", "TabletType", "Frequency.Freezing", "Bother.Freezing", "Frequency.Lapse", "Bother.Lapse", "Frequency.TinyIcons", "Bother.TinyIcons", "Frequency.Crash", "Bother.Crash", "LeastFavouriteGame", "Bother.Quiz", "Bother.Spell", "Neuroticism", "Extraversion", "Openness", "Agreeableness", "Conscientiousness", "VP.Touches")
  
  # names(FB.Demograph) <- FB.Demograph.englishNames
  
  # nrEntries <- length(FB.Demograph$Neuroticism)
  # 
  # FB.Demograph$Neuroticism.levels <- rep("low", nrEntries)
  # FB.Demograph$Neuroticism.levels[FB.Demograph$Neuroticism > 48/3] <- "medium"
  # FB.Demograph$Neuroticism.levels[FB.Demograph$Neuroticism > 2*48/3] <- "high"
  # FB.Demograph$Neuroticism.levels <- factor(FB.Demograph$Neuroticism.levels, levels = c("low", "medium", "high"))
  # 
  # FB.Demograph$Extraversion.levels <- rep("low", nrEntries)
  # FB.Demograph$Extraversion.levels[FB.Demograph$Extraversion > 48/3] <- "medium"
  # FB.Demograph$Extraversion.levels[FB.Demograph$Extraversion > 2*48/3] <- "high"
  # FB.Demograph$Extraversion.levels <- factor(FB.Demograph$Extraversion.levels, levels = c("low", "medium", "high"))
  # 
  # FB.Demograph$Openness.levels <- rep("low", nrEntries)
  # FB.Demograph$Openness.levels[FB.Demograph$Openness > 48/3] <- "medium"
  # FB.Demograph$Openness.levels[FB.Demograph$Openness > 2*48/3] <- "high"
  # FB.Demograph$Openness.levels <- factor(FB.Demograph$Openness.levels, levels = c("low", "medium", "high"))
  # 
  # FB.Demograph$Agreeableness.levels <- rep("low", nrEntries)
  # FB.Demograph$Agreeableness.levels[FB.Demograph$Agreeableness > 48/3] <- "medium"
  # FB.Demograph$Agreeableness.levels[FB.Demograph$Agreeableness > 2*48/3] <- "high"
  # FB.Demograph$Agreeableness.levels <- factor(FB.Demograph$Agreeableness.levels, levels = c("low", "medium", "high"))
  # 
  # FB.Demograph$Conscientiousness.levels <- rep("low", nrEntries)
  # FB.Demograph$Conscientiousness.levels[FB.Demograph$Conscientiousness > 48/3] <- "medium"
  # FB.Demograph$Conscientiousness.levels[FB.Demograph$Conscientiousness > 2*48/3] <- "high"
  # FB.Demograph$Conscientiousness.levels <- factor(FB.Demograph$Conscientiousness.levels, levels = c("low", "medium", "high"))
  
  
  return(FB.Demograph)
}

preprocessAppStartCloseData <- function(tracking.appStartClose, users){
  
  tracking.appStartClose <- tracking.appStartClose[tracking.appStartClose$user_id %in% users,]
  
  # for(u in User.ids){
  #   print( paste(u, length(appStarts$user_id[appStarts$user_id == u])))
  # }
  
  # even start with app 7
  # odd start with app 9
  
  appStarts <- subset(tracking.appStartClose, tracking.appStartClose$event==0)
  
  # return(appStarts)
}



mapFBToDB <- function(FB.Werte, touches.versuch, tracking.appStartClose, users){
  
  #initialize session with 1 for all entries
  touches.versuch$session <- rep(1, length(touches.versuch$id.down))
  startDiffs <- c(appStarts$timestamp[2:nrow(appStarts)] - appStarts$timestamp[1:nrow(appStarts)-1], 61)
  duration.break <- 60*4.5
  nrBreaks <- 0
  #appStarts <- subset(tracking.appStartClose, tracking.appStartClose$event==0)s
  j <- 13
  for (i in 1:dim(appStarts)[1]){
    appStarted <- appStarts$timestamp[i]
    diff <- startDiffs[i]
    # if two breaks follow each other, don't count session up for the second break
    if (diff > duration.break){
      nrBreaks <- nrBreaks + 1
    } else {
      nrBreaks <- 0
    }
    if (j > 12){
      j <- 1
    }
    touches.debug <- data.frame(touches.versuch$user_id, as.POSIXct(touches.versuch$timestamp.down), touches.versuch$session)
    touches.versuch$session[touches.versuch$timestamp.down > appStarted] <- rep(j, length(touches.versuch$session[touches.versuch$timestamp.down > appStarted]))
    touches.debug$session.new <- touches.versuch$session
    head(touches.debug[touches.debug$as.POSIXct.touches.versuch.timestamp.down.>appStarted,])
    if ((diff > 60) & (nrBreaks < 2)){
      j <- j+1
    }
  }
  
  
  
  for(VP in users){
    for(session in 1:12){
      touches.versuch$PQ[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$PQ[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$HQ[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$HQ[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$ATT[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$ATT[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$HQI[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$HQI[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$HQS[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$HQS[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      
      touches.versuch$PQ.std[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$PQ.std[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$HQ.std[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$HQ.std[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$ATT.std[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$ATT.std[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      
      touches.versuch$SEA[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$SEA[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$SEA.normalized[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$SEA.normalized[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$SEA.std[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$SEA.std[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      
      
      touches.versuch$PQ_1[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$PQ_1[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$PQ_2[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$PQ_2[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$PQ_3[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$PQ_3[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$PQ_4[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$PQ_4[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$HQI_1[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$HQI_1[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$HQI_2[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$HQI_2[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$HQS_1[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$HQS_1[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$HQS_2[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$HQS_2[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$ATT_1[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$ATT_1[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      touches.versuch$ATT_2[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$ATT_2[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      
      touches.versuch$App[touches.versuch$session == session & touches.versuch$user_id == VP] <- toString(FB.Werte$App[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP])
      touches.versuch$AppPos[touches.versuch$session == session & touches.versuch$user_id == VP] <- toString(FB.Werte$AppPos[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP])
      
      touches.versuch$Cond[touches.versuch$session == session & touches.versuch$user_id == VP] <- toString(FB.Werte$Cond[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP])
      touches.versuch$CondPos[touches.versuch$session == session & touches.versuch$user_id == VP] <- toString(FB.Werte$CondPos[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP])
      
      touches.versuch$Nr[touches.versuch$session == session & touches.versuch$user_id == VP] <- FB.Werte$Nr[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP]
      
      touches.versuch$PQ.levels[touches.versuch$session == session & touches.versuch$user_id == VP] <- toString(FB.Werte$PQ.levels[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP])
      touches.versuch$HQ.levels[touches.versuch$session == session & touches.versuch$user_id == VP] <- toString(FB.Werte$HQ.levels[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP])
      touches.versuch$ATT.levels[touches.versuch$session == session & touches.versuch$user_id == VP] <- toString(FB.Werte$ATT.levels[FB.Werte$Nr == session & FB.Werte$VP.Touches == VP])
      
      # touches.versuch$App <- as.factor(touches.versuch$App)
      # touches.versuch$AppPos <- as.factor(touches.versuch$AppPos)
      # 
      # touches.versuch$Cond <- as.factor(touches.versuch$Cond)
      # touches.versuch$CondPos <- as.factor(touches.versuch$CondPos)
      
    }
  }
  
  
  return(touches.versuch)
}

extractTouchAccuracy <- function(touches.experiment){
  
  apps <- attributes(touches.experiment$App)$levels
  conds <- attributes(touches.experiment$CondPos)$levels
  vps <- as.numeric(attributes(as.factor(touches.experiment$VP))$levels)
  
  touches.experiment$button_center_x <- -2000
  touches.experiment$button_center_y <- -2000
  
  touches.experiment$difference.touch_buttonCenter_x <- -2000
  touches.experiment$difference.touch_buttonCenter_y <- -2000
  
  touches.experiment$touchAccuracy <- -2000
  touches.experiment$touchAccuracy_x <- -2000
  touches.experiment$touchAccuracy_y <- -2000
  
  for(vp in vps){
    for(appName in apps){
      for(cond in conds){
        currentGameData <- touches.experiment[touches.experiment$VP == vp & touches.experiment$App == appName & touches.experiment$CondPos == cond,]
        l <- length(currentGameData$button_pressed)
        for(i in 1:l){
          touch_x <- currentGameData$x_location.down[i]
          touch_y <- currentGameData$y_location.down[i]
          
          if(currentGameData$button_pressed[i]){
            
            button_center_x <- currentGameData$button_origin_x.down[i] + currentGameData$button_width.down[i]/2
            button_center_y <- currentGameData$button_origin_y.down[i] + currentGameData$button_height.down[i]/2
            
          } else {
            
            j <- i
            
            while(!currentGameData$button_pressed[j] & (j<l)){
              j <- j+1
            }
            
            if (j > l){
              
              button_center_x <- NA
              button_center_y <- NA
              
            } else {
              
              button_center_x <- currentGameData$button_origin_x.down[j] + currentGameData$button_width.down[j]/2
              button_center_y <- currentGameData$button_origin_y.down[j] + currentGameData$button_height.down[j]/2
              
              currentGameData$button_width.down[i] <- currentGameData$button_width.down[j]
              currentGameData$button_height.down[i] <- currentGameData$button_height.down[j]
              
            }
          } # end check if button pressed
          
          currentGameData$button_center_x[i] <- button_center_x
          currentGameData$button_center_y[i] <- button_center_y
          
          currentGameData$difference.touch_buttonCenter_x[i] <- touch_x - button_center_x
          currentGameData$difference.touch_buttonCenter_y[i] <- touch_y - button_center_y
          
          currentGameData$touchAccuracy_x[i] <- (currentGameData$x_location.down[i]-currentGameData$button_center_x[i])/(0.5*currentGameData$button_width.down[i])
          currentGameData$touchAccuracy_y[i] <- (currentGameData$y_location.down[i]-currentGameData$button_center_y[i])/(0.5*currentGameData$button_height.down[i])
          currentGameData$touchAccuracy[i] <- currentGameData$touchAccuracy_x[i] * currentGameData$touchAccuracy_y[i]
          
          # print(with(currentGameData[i,], data.frame(button_pressed, correct_answer, x_location.down, button_origin_x.down, button_width.down, button_center_x, difference.touch_buttonCenter_x, y_location.down, button_origin_y.down, button_height.down, button_center_y, difference.touch_buttonCenter_y)))
        } # end for loop through data for one run
        
        touches.experiment$difference.touch_buttonCenter_x[touches.experiment$VP == vp & touches.experiment$App == appName & touches.experiment$CondPos == cond] <- currentGameData$difference.touch_buttonCenter_x
        touches.experiment$difference.touch_buttonCenter_y[touches.experiment$VP == vp & touches.experiment$App == appName & touches.experiment$CondPos == cond] <- currentGameData$difference.touch_buttonCenter_y
        
        touches.experiment$touchAccuracy_x[touches.experiment$VP == vp & touches.experiment$App == appName & touches.experiment$CondPos == cond] <- currentGameData$touchAccuracy_x
        touches.experiment$touchAccuracy_y[touches.experiment$VP == vp & touches.experiment$App == appName & touches.experiment$CondPos == cond] <- currentGameData$touchAccuracy_y
        touches.experiment$touchAccuracy[touches.experiment$VP == vp & touches.experiment$App == appName & touches.experiment$CondPos == cond] <- currentGameData$touchAccuracy
        
        touches.experiment <- ddply(touches.experiment, c("VP"), transform, touchAccuracy_x.std = scale(touchAccuracy_x))
        touches.experiment <- ddply(touches.experiment, c("VP"), transform, touchAccuracy_y.std = scale(touchAccuracy_y))
        touches.experiment <- ddply(touches.experiment, c("VP"), transform, touchAccuracy.std = scale(touchAccuracy))
        
        touches.experiment$button_center_x[touches.experiment$VP == vp & touches.experiment$App == appName & touches.experiment$CondPos == cond] <- currentGameData$button_center_x
        touches.experiment$button_center_y[touches.experiment$VP == vp & touches.experiment$App == appName & touches.experiment$CondPos == cond] <- currentGameData$button_center_y
        
        touches.experiment$button_width.down[touches.experiment$VP == vp & touches.experiment$App == appName & touches.experiment$CondPos == cond] <- currentGameData$button_width.down
        touches.experiment$button_height.down[touches.experiment$VP == vp & touches.experiment$App == appName & touches.experiment$CondPos == cond] <- currentGameData$button_height.down
        
        # View(head(with(touches.experiment[touches.experiment$VP == vp & touches.experiment$App == appName & touches.experiment$CondPos == cond,], data.frame(button_pressed, correct_answer, x_location.down, button_origin_x.down, button_width.down, button_touch_x_location.down, button_center_x, difference.touch_buttonCenter_x, touchAccuracy_x, y_location.down, button_origin_y.down, button_height.down, button_touch_y_location.down, button_center_y, difference.touch_buttonCenter_y, touchAccuracy_y)) , 25))
      }
    }
  }
  
  # View(with(touches.experiment, data.frame(button_pressed, correct_answer, x_location.down, button_origin_x.down, button_width.down, button_center_x, difference.touch_buttonCenter_x, y_location.down, button_origin_y.down, button_height.down, button_center_y, difference.touch_buttonCenter_y)))
  
  
  return(touches.experiment)
}

extractTouchFeatures <- function(touches){
  
  features <- NULL
  features$VP <- as.factor(touches$VP)
  features$Sex <- as.factor(touches$sex)
  features$App <- as.factor(touches$App)
  features$Cond <- as.factor(touches$Cond)
  features$Session <- as.factor(touches$session)
  
  features$swipe_length <- sqrt((touches$x_location.down - touches$x_location.up)^2 + (touches$y_location.down - touches$y_location.up)^2)
  # touches$swipe_speed <- touches$swipe_length / touches$touch_intervall.up
  features$swipe_speed <- features$swipe_length / as.numeric(touches$timestamp.up - touches$timestamp.down)
  features$isSwipe <- as.factor(features$swipe_length > 0)
  features$duration <- as.numeric(touches$timestamp.up - touches$timestamp.down)
  features$missedTarget <- as.factor(!touches$button_pressed)
  
  
  # touches$time_between_touches <- touches$timestamp.up - touches$timestamp.down
  
  features$time_between_touches <- c(NA,touches$timestamp.down[2:nrow(touches)] - touches$timestamp.up[1:(nrow(touches)-1)])
  
  if (is.null(touches$touchAccuracy_x)){
    touches <- extractTouchAccuracy(touches)
  }
  
  
  features$touchAccuracy_x <- touches$touchAccuracy_x
  features$touchAccuracy_y <- touches$touchAccuracy_y
  features$touchAccuracy <- touches$touchAccuracy
  
  features$touchAccuracy_x.std <- touches$touchAccuracy_x.std
  features$touchAccuracy_y.std <- touches$touchAccuracy_y.std
  features$touchAccuracy.std <- touches$touchAccuracy.std

  
  return(as.data.frame(features))
}