source("C:\\Users\\ola\\Desktop\\studia\\modele diagnostyki\\symulacje_funkcje.R")


students = 500
skills = c(2,3,4,6) #c(2,3,4,6,8)
itemsComm = sort(c(2^(seq(1:7)),3,6)) #2^seq(2:9)
#itemsComm = c(2,3,4,6,10,25,50,100,150)
RUN_PARAMS = 4
N=25

#DINA ACCURACY BASED ON Q MATRIX COMPLETNESS
skill = 4
N=50
items = c(4,10,15,20,30,50,100)
RUN_PARAMS = 4
omits = seq(skill+1)-1
simIncompleteQ = array(dim=c(N,RUN_PARAMS,length(items),length(omits)))
for (i_omits in 1:length(omits)){
  print(str_glue("omit={omits[i_omits]}"))
  a2 = array(dim=c(N,RUN_PARAMS,length(items)))
  for (i_items in 1:length(items)){
    print(str_glue("items={items[i_items]}"))
    a1 = array(dim=c(N,RUN_PARAMS))
    for (i in 1:N){
      a1[i,] =quiet(simRunIncomplete(skill,items[i_items],students,omit=omits[i_omits]))
    }
    a2[,,i_items] = a1
  }
  simIncompleteQ[,,,i_omits] = a2
}
saveRDS(simIncompleteQ,file=paste(directory,"IncompleteQ.rda",sep=""))
np$save(paste(directory,"IncompleteQ.npy",sep="") ,np$array(simIncompleteQ))


#DINA ACCURACY - COMPARISON BETWEEN CORRECT AND INCORRECT Q MATRIX, Q MATRIX VALIDATION
#DELTA SEM METHOD
skill = 4
per=sort(c(seq(0.05,0.2,0.03),0.01))
RUN_PARAMS = 11
items = c(10,15,30,50,100)
N=25
students=c(50,500,2000)
simIncorrectSEM = array(dim=c(N,RUN_PARAMS,length(items),length(per),length(students)))
for (m_st in 1:length(students)){
  a4 = array(dim=c(N,RUN_PARAMS,length(items),length(per)))
  for (i_per in 1:length(per)){
    print(str_glue("per={per[i_per]}"))
    a3 = array(dim=c(N,RUN_PARAMS,length(items)))
    for (j_items in 1:length(items)) {
      print(items[j_items])
      a2 = matrix(ncol = RUN_PARAMS,nrow=N)
      for (i in 1:N){
        #print(i)
        a2[i,] = simRunIncorrectSEM(skill,items[j_items],students[m_st],per=per[i_per])
        
        a3[,,j_items] = a2
      }
    }
    a4[,,,i_per] = a3
  }
  simIncorrectSEM[,,,,m_st] = a4
}
saveRDS(simIncorrectSEM,file=paste(directory,"IncorrectSEM.rda",sep=""))
np$save(paste(directory,"IncorrectSEM.npy",sep="") ,np$array(simIncorrectSEM))

#GDI METHOD
simIncorrectGDI = array(dim=c(N,RUN_PARAMS,length(items),length(per),length(M)))
for (m_st in 1:length(M)){
  a4 = array(dim=c(N,RUN_PARAMS2,length(items),length(per)))
  for (i_per in 1:length(per)){
    print(str_glue("per={per[i_per]}"))
    a3 = array(dim=c(N,RUN_PARAMS2,length(items)))
    for (j_items in 1:length(items)) {
      print(items[j_items])
      a2 = matrix(ncol = RUN_PARAMS2,nrow=N)
      for (i in 1:N){
        #print(i)
        a2[i,] = simRunIncorrectSEMGDI(skill,items[j_items],M[m_st],per=per[i_per])
        
        a3[,,j_items] = a2
      }
    }
    a4[,,,i_per] = a3
  }
  simIncorrectGDI[,,,,m_st] = a4
}

saveRDS(simIncorrectGDI,file=paste(directory,"IncorrectGDI.rda",sep=""))
np$save(paste(directory,"IncorrectGDI.npy",sep="") ,np$array(simIncorrectGDI))