#Lookup tables

##next states
next.vec = c(A = 'R', X1 = 'X2', X2 = 'R', C1 = 'C2', C2 = 'C3', C3 = 'R', D1 = 'D2', D2 = 'D3', 
             D3 = 'M', HR1 = 'HR2', HR2 = 'HR3', HR3 = 'R', HM1 = 'HM2', HM2 = 'HM3', HM3 = 'M',
             M = 'M', R = 'R')

##States transmitting to household
inf2house_vec = c(S = F, E = F, A = T, X1 = T, X2 = T, C1 = T, C2 = T, C3 = F, D1 = T, D2 = T, 
                  D3 = F, HR1 = T, HR2 = T, HR3 = F, HM1 = T, HM2 = T, HM3 = F,
                  M = F, R = F)

##States transmitting to community
inf2comm_vec = c(S = F, E = F, A = T, X1 = T, X2 = F, C1 = T, C2 = F, C3 = F, D1 = T, D2 = F, 
                 D3 = F, HR1 = T, HR2 = F, HR3 = F, HM1 = T, HM2 = F, HM3 = F,
                 M = F, R = F)

#load in Rda files of parameters from Sean's original package
##includes contact matrix, outcome probabilities, waiting time distribution parameters
flist = list.files('CMH/ABM/data/pars_data')
lapply(flist,load, envir = globalenv())
