library(Qval)
library(GDINA)

Qmatrix <- function(skills,items,omit=0) {
  #create Q matrix
  #omit = 0 -> complete 
  #omit = 1,2,...,skills -> incomplete
  if (items < skills){
    return ("za mało pytań")
  }
  fullQ = permutations(2,skills,c(0,1),repeats.allowed = TRUE)
  if (omit > 0){
    omitted = sample(skills,omit)
    fullQ = fullQ[order(rowSums(fullQ)),][-1,][-omitted,]
    if (skills==2 && omit ==skills){
      return (matrix(rep(c(1,1),items),nrow=items))
    }  
    return (fullQ[c(seq(skills-omit),sample(nrow(fullQ),items-skills+omit,replace=TRUE)),])  
  }
  fullQ = fullQ[order(rowSums(fullQ)),][-1,]
  return (fullQ[c(seq(skills),sample(nrow(fullQ),items-skills,replace=TRUE)),])
}

incorrectQ <- function(skills,items,per=0.1,type="lower") {
  #create true and false Q matrix
  #introduce per % changes in Q matrix
  #change -> from 1 to 0 (no connection between attribute and task)
  
  if (items < skills){
    return ("za mało pytań")
  }
  fullQ = permutations(2,skills,c(0,1),repeats.allowed = TRUE)
  fullQ = fullQ[order(rowSums(fullQ)),][-1,]
  Q = (fullQ[c(seq(skills),sample(nrow(fullQ),items-skills,replace=TRUE)),])
  Q1 <- Q
  toChange = seq(items)[rowSums(Q) >1]
  toChange1s = lapply(toChange,findOnes,skills=skills,Q=Q)
  lens = unlist(lapply(toChange1s,length))
  toDelete = unlist(lapply(lens, sample,1))
  for (i in 1:length(toDelete)){
    toChange1s[[i]] = toChange1s[[i]][-toDelete[i]] 
  }
  lens1 = lens-1
  possChange = matrix(nrow=sum(lens1),ncol=2)
  for (i in 1:length(toChange)){wqa
    possChange[(sum(lens1[0:(i-1)])+1):(sum(lens1[1:i])),] = matrix(unlist(expand.grid(toChange[i],toChange1s[[i]])),ncol=2)
  }
  if (ceiling(items*per*skills)> nrow(possChange)){
    selected = matrix(possChange[sample(seq(nrow(possChange)),ceiling(items*skills*per),replace=TRUE),],ncol=2)
  }else {
    selected = matrix(possChange[sample(seq(nrow(possChange)),ceiling(items*per*skills)),],ncol=2)
  }
  for (i in 1:nrow(selected)){
    Q1[selected[i,1],selected[i,2]] = 0
  }
  return (list(Q,Q1))
}

simRunIncomplete <-function(skills,items,students,prob=0.7,gs = "low",guess=c(0.05,0.25),slip=c(0.05,0.25),omit=0){
  #one MC step
  #generate data from dina model and dina's calculate accuracy
  #Q matrix is complete if omit=0
  if (gs == "low"){
    guess=c(0.05,0.25)
    slip=c(0.05,0.25)
  } else if (gs == "high"){
    guess=c(0.25,0.4)
    slip=c(0.25,0.4)
  }
  guessV = runif(items,guess[1],guess[2])
  slipV = runif(items,slip[1],slip[2])
  Q = Qmatrix(skills,items,omit=omit)
  profiles = indProf(ncol(Q),prob,students)
  data = sim.din(q.matrix=Q,guess=guessV,slip=slipV,alpha = profiles)$dat
  model = din(data,Q,progress = FALSE)
  
  
  # calculate accuracy
  prob_diff = mean(abs(model$skill.patt-prob)[[1]])
  acaPred = as.vector(t(IRT.factor.scores(model)))
  acaTrue = as.vector(t(profiles))
  acaDiff = 1-sum(abs(acaPred-acaTrue))/length(acaPred)
  pcaPred = apply(IRT.factor.scores(model),1,paste,collapse="")
  pcaTrue = apply(profiles,1,paste,collapse="")
  pcaDiff = sum(pcaTrue == pcaPred)/length(pcaPred)
  pattProbDiff = mean(abs(model$attribute.patt[order(row.names(model$attribute.patt)),][,1]-
                            table(factor(apply(profiles,1,paste,collapse=""),levels=row.names(model$attribute.patt)))/nrow(profiles)))
  distinctProfs = max(din.equivalent.class(Q)$skillclasses[3]) #no of distinguishable attribute profiles
  return (c(
    acaDiff,pcaDiff,pattProbDiff,distinctProfs))
}

simRunIncorrectSEM<-function(skills,items,students,prob=0.7,guess=c(0.05,0.25),slip=c(0.05,0.25),per=0.1){
  #one MC step
  #generate data from dina model and dina's calculate accuracy based on on correct/incorrect/estimated Q matrix
  #per is percent of incorrect entries introduced to Q matrix
  guessV = runif(items,guess[1],guess[2])
  slipV = runif(items,slip[1],slip[2])
  Q = incorrectQ(skills,items,per=per)
  Qtrue = Q[[1]]
  Qfalse = Q[[2]]
  profiles = indProf(ncol(Qtrue),prob,students)
  data = sim.din(q.matrix=Qtrue,guess=guessV,slip=slipV,alpha = profiles)$dat
  model1 = din(data,Qtrue,progress = FALSE)
  model2 = din(data,Qfalse,progress = FALSE)
  estimQ = quiet(din.validate.qmatrix(model2)$q.matrix.prop) #estimating correct Q matrix using SEM delta method
  model3 = din(data,estimQ,progress = FALSE)
  
  # calculate accuracy
  acaPred1 = as.vector(t(IRT.factor.scores(model1)))
  acaPred2 = as.vector(t(IRT.factor.scores(model2)))
  acaPred3 = as.vector(t(IRT.factor.scores(model3)))
  acaTrue = as.vector(t(profiles))
  acaDiff1 = 1-sum(abs(acaPred1-acaTrue))/length(acaPred1) 
  acaDiff2 = 1-sum(abs(acaPred2-acaTrue))/length(acaPred2)
  acaDiff3 = 1-sum(abs(acaPred3-acaTrue))/length(acaPred3)
  pcaPred1 = apply(IRT.factor.scores(model1),1,paste,collapse="")
  pcaPred2 = apply(IRT.factor.scores(model2),1,paste,collapse="")
  pcaPred3 = apply(IRT.factor.scores(model3),1,paste,collapse="")
  pcaTrue = apply(profiles,1,paste,collapse="")
  pcaDiff1 = sum(pcaTrue == pcaPred1)/length(pcaPred1)
  pcaDiff2 = sum(pcaTrue == pcaPred2)/length(pcaPred1)
  pcaDiff3 = sum(pcaTrue == pcaPred3)/length(pcaPred3)  
  pattProbDiff1= mean(abs(model1$attribute.patt[order(row.names(model1$attribute.patt)),][,1]-
                            table(factor(apply(profiles,1,paste,collapse=""),levels=row.names(model1$attribute.patt)))/nrow(profiles)))
  pattProbDiff2= mean(abs(model2$attribute.patt[order(row.names(model2$attribute.patt)),][,1]-
                            table(factor(apply(profiles,1,paste,collapse=""),levels=row.names(model2$attribute.patt)))/nrow(profiles)))
  pattProbDiff3= mean(abs(model3$attribute.patt[order(row.names(model3$attribute.patt)),][,1]-
                            table(factor(apply(profiles,1,paste,collapse=""),levels=row.names(model3$attribute.patt)))/nrow(profiles)))
  estDiff = sum(abs(quiet(din.validate.qmatrix(model2)$q.matrix.prop)-Qtrue))
  trueDiff = sum(abs(Qtrue-Qfalse))
  return (c(
    acaDiff1,pcaDiff1,pattProbDiff1,
    acaDiff2,pcaDiff2,pattProbDiff2,
    acaDiff3,pcaDiff3,pattProbDiff3,
    estDiff, trueDiff))
}

simRunIncorrectGDI <-function(skills,items,students,prob=0.7,guess=c(0.05,0.25),slip=c(0.05,0.25),per=0.1){
  #one MC step
  #generate data from dina model and dina's calculate accuracy based on on correct/incorrect/estimated Q matrix
  #per is percent of incorrect entries introduced to Q matrix
  guessV = runif(items,guess[1],guess[2])
  slipV = runif(items,slip[1],slip[2])
  Q = incorrectQ(skills,items,per=per)
  Qtrue = Q[[1]]
  Qfalse = Q[[2]]
  profiles = indProf(ncol(Qtrue),prob,students)
  data = sim.din(q.matrix=Qtrue,guess=guessV,slip=slipV,alpha = profiles)$dat
  model1 = din(data,Qtrue,progress = FALSE)
  model2 = din(data,Qfalse,progress = FALSE)
  estimQ = validation(data,Qfalse,model="DINA")$Q.sug #estimating correct Q matrix using GDI index method
  model3 = din(data,estimQ,progress = FALSE)
  
  # calculate accuracy
  acaPred1 = as.vector(t(IRT.factor.scores(model1)))
  acaPred2 = as.vector(t(IRT.factor.scores(model2)))
  acaPred3 = as.vector(t(IRT.factor.scores(model3)))
  acaTrue = as.vector(t(profiles))
  acaDiff1 = 1-sum(abs(acaPred1-acaTrue))/length(acaPred1)
  acaDiff2 = 1-sum(abs(acaPred2-acaTrue))/length(acaPred2)
  acaDiff3 = 1-sum(abs(acaPred3-acaTrue))/length(acaPred3)
  pcaPred1 = apply(IRT.factor.scores(model1),1,paste,collapse="")
  pcaPred2 = apply(IRT.factor.scores(model2),1,paste,collapse="")
  pcaPred3 = apply(IRT.factor.scores(model3),1,paste,collapse="")
  pcaTrue = apply(profiles,1,paste,collapse="")
  pcaDiff1 = sum(pcaTrue == pcaPred1)/length(pcaPred1)
  pcaDiff2 = sum(pcaTrue == pcaPred2)/length(pcaPred1)
  pcaDiff3 = sum(pcaTrue == pcaPred3)/length(pcaPred3)
  pattProbDiff1= mean(abs(model1$attribute.patt[order(row.names(model1$attribute.patt)),][,1]-
                            table(factor(apply(profiles,1,paste,collapse=""),levels=row.names(model1$attribute.patt)))/nrow(profiles)))
  pattProbDiff2= mean(abs(model2$attribute.patt[order(row.names(model2$attribute.patt)),][,1]-
                            table(factor(apply(profiles,1,paste,collapse=""),levels=row.names(model2$attribute.patt)))/nrow(profiles)))
  pattProbDiff3= mean(abs(model3$attribute.patt[order(row.names(model3$attribute.patt)),][,1]-
                            table(factor(apply(profiles,1,paste,collapse=""),levels=row.names(model3$attribute.patt)))/nrow(profiles)))
  estDiff = sum(abs(estimQ-Qtrue))
  trueDiff = sum(abs(Qtrue-Qfalse))
  return (c(
    acaDiff1,pcaDiff1,pattProbDiff1,
    acaDiff2,pcaDiff2,pattProbDiff2,
    acaDiff3,pcaDiff3,pattProbDiff3,
    estDiff, trueDiff))
}