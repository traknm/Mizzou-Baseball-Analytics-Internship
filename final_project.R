library(tidyverse)
batter <- read_csv("/Users/tommie/Desktop/Stats Homework/Stat Models 2/final_project/batter_all.csv")

#Do Umpires get worse throughout games? Let's go.

#Seperate data by called balls and strikes
calledstrikes = batter[batter$description=="called_strike",]
calledballs = batter[batter$description=="ball",]
calls = rbind(calledstrikes, calledballs)
calls1 = calls[!is.na(calls$plate_x),]

#Real Calls
correctcall = c()
for (i in 1:2152610) {
  correctcall[i]=0
  if(calls1$plate_x[i] < 0.70833333333){ 
    if(calls1$plate_z[i] < 3.5111111111111111){
      if(calls1$plate_x[i] > -0.70833333333){
        if(calls1$plate_z[i]> 1.51111111111111){
          correctcall[i] = 1
        }
      }
      
    }
  }
}

Umpcall = c()
Umpcall = ifelse(calls1$description=="called_strike", yes = 1, no = 0)
mean(Umpcall)
mean(correctcall)
callsdf = data.frame(Umpcall, correctcall)

calls1 = data.frame(calls1, callsdf)

wrong = NULL
for(i in 1:2152610){
  wrong[i] = 0
  if(calls1$correctcall[i] != calls1$Umpcall[i]){
    wrong[i] = 1
  }
}
wrongdf = data.frame(wrong, calls1$inning)

table(wrongdf$calls1.inning)

ml <- lmList(correctcall ~ Umpcall | inning, calls1)
sapply(ml,coef)[,1:19] |> as.matrix()

library(lme4)
mixed_model <- lmer(Umpcall ~ balls + strikes + outs_when_up + (1 | inning),
              data = calls1, control=lmerControl(optimizer="Nelder_Mead"))
summary(mixed_model)

ranef(mixed_model)$inning

#Mixed Effects
mixed_model2 <- lmer(correctcall ~ Umpcall + balls + strikes + (1 | inning),
                    data = calls1)
summary(mixed_model2)

std = lm(correctcall ~ balls + strikes + inning + outs_when_up, data = calls1)
summary(std)

count1 = 0
count2 = 0
count3 = 0
count4 = 0
count5 = 0
count6= 0
count7 = 0
count8 = 0
count9 = 0
count10 = 0
count11 = 0
count12 = 0
count13 = 0
count14 = 0
count15 = 0
count16 = 0
count17 = 0
count18 = 0
count19 = 0
for (i in 1:2152610) {
  if(wrongdf$calls1.inning[i]==1){
    count1= count1 + 1}
  
  if(wrongdf$calls1.inning[i]==2){
    count2= count2 + 1}
  
  if(wrongdf$calls1.inning[i]==3){
    count3= count3 + 1}
  
  if(wrongdf$calls1.inning[i]==4){
    count4= count4 + 1}
  
  if(wrongdf$calls1.inning[i]==5){
    count5= count5 + 1}
  
  if(wrongdf$calls1.inning[i]==6){
    count6= count6 + 1}
  
  if(wrongdf$calls1.inning[i]==7){
    count7= count7 + 1}
  
  if(wrongdf$calls1.inning[i]==8){
    count8= count8+ 1}
  
  if(wrongdf$calls1.inning[i]==9){
    count9= count9 + 1}
  
  if(wrongdf$calls1.inning[i]==10){
    count10= count10 + 1}
  
  if(wrongdf$calls1.inning[i]==11){
    count11= count11 + 1}
  
  if(wrongdf$calls1.inning[i]==12){
    count12= count12 + 1}
  
  if(wrongdf$calls1.inning[i]==13){
    count13= count13 + 1}
  
  if(wrongdf$calls1.inning[i]==14){
    count14= count14 + 1}
  
  if(wrongdf$calls1.inning[i]==15){
    count15= count15 + 1}
  
  if(wrongdf$calls1.inning[i]==16){
    count16= count16 + 1}
  
  if(wrongdf$calls1.inning[i]==17){
    count17= count17 + 1}
  
  if(wrongdf$calls1.inning[i]==18){
    count18= count18 + 1}
  
  if(wrongdf$calls1.inning[i]==19){
    count19= count19 + 1}
}

counts = cbind(count1, count2, count3, count4, count5, count6,count7, count8,)

















