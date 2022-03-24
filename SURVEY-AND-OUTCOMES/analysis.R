#
# Author: Christos Deligkaris
# Date: 3/16/2022
# Purpose: analyzes student survey and course learning outcomes
#
############################################################################
library(tidyverse)
library("readxl")
library(stringr)
install.packages("rcompanion")
install.packages("lmtest")
install.packages("DescTools")
library(rcompanion) #for Cramer's V

#set wd
dir_proj<-"/Users/christos/Google Drive/My Drive/JOBS/APPLICATIONS/2021-2022/MINERVA/MINERVA-PROJECT"
setwd(dir_proj) 

#load data
data_scs<-read_excel("./era.xlsx", sheet = "Student Course Survey")
data_clo<-read_excel("./era.xlsx", sheet = "Course Learning Outcomes")

############################################################################

# Purpose: calculate the frequencies of occurrence of each unique element in the list
# Input:  options - unique elements logically ordered, vector
#         responses - student responses (opinions) or assessment performace
# Output: freqs - the frequencies

get_freqs <- function(options, responses) {
  
  freqs = c() #vector that will hold all frequencies
  #assign frequencies
  i<-1
  for (x in options) {
    freqs[i]=sum(grepl(x,responses))
    i<-i+1
  }

  return(freqs)
}

############################################################################
# Q1

scs_q1<-data_scs %>% pull("Q1")
scs_q1_item<-scs_q1[1] #Q1
scs_q1_res<-scs_q1[2:length(scs_q1)] #all Q1 responses

scs_q1_options<-unique(scs_q1_res) #keep unique options in MC question
scs_q1_options #check order
scs_q1_options <- c(scs_q1_options[5],scs_q1_options[2],scs_q1_options[3],scs_q1_options[1],scs_q1_options[4]) #manually order them, to make sense

scs_q1_freq <- get_freqs(scs_q1_options,scs_q1_res) #frequencies
scs_q1_N <- sum(scs_q1_freq) #N number

#plot results
barplot(scs_q1_freq,xlab=paste("N=",scs_q1_N,sep=""),ylab="Frequency", main=scs_q1_item,cex.names=0.85,names.arg=scs_q1_options,cex.main=0.75)

############################################################################
# find the unique responses in the set of Q2 questions and logically order them
# Q2_1, Q2_2, Q2_3, Q2_4 appear to be using the same unique options in the MC questions 

scs_q21<-data_scs %>% pull("Q2_1")
scs_q21_item<-scs_q21[1] #Q2_1
scs_q21_res<-scs_q21[2:length(scs_q21)] #all Q2_1 responses

scs_q21_options<-unique(scs_q21_res) #keep unique options in MC question
scs_q21_options #check order
#manually order them, to make sense
scs_q21_options <- c(scs_q21_options[4],scs_q21_options[2],scs_q21_options[1],scs_q21_options[3]) 

# with scatter plots, overplotting can be an issue, these are some solutions
ggplot(mapping=aes(y=scs_q21_res,x=scs_q1_res))+geom_point(position="jitter")
ggplot(mapping=aes(y=scs_q21_res,x=scs_q1_res))+geom_count()
ggplot(mapping=aes(y=scs_q21_res,x=scs_q1_res))+geom_bin_2d()

#Cramer's V test can be used to find correlation between two categorical variables
cramerV(y=scs_q21_res,x=scs_q1_res) 

############################################################################
# Since Q2_1, Q2_2, Q2_3, Q2_4 appear to be using the same unique options in the MC questions so define a function
# to do their analysis

# Input:
#   data: data frame, all student data
#   question: string, question to be analyzed, must match a first row item from student data
#   options: logically ordered options that were provided to students in the MC question to be analyzed
#
# Returns: nothing
# 
# Purpose: creates a bar chart

q_analysis <- function(data, question,options,sstart) {
  
  data_q<-data %>% pull(question)
  data_q_item<-data_q[1] #question 
  data_q_res<-data_q[2:length(data_q)] #all responses
  
  data_q_options<-options
  
  data_q_freq <- get_freqs(data_q_options,data_q_res) #frequencies
  data_q_N <- sum(data_q_freq) #N number, use this to get N to avoid counting students that did not respond
  
  #plot results
  barplot(data_q_freq,xlab=paste("N=",data_q_N,sep=""),ylab="Frequency", main=substring(data_q_item,sstart),cex.names=0.9,names.arg=data_q_options,cex.main=0.7)
}

#########################################
# Now apply function to analyze all Q2 subquestions

lapply(c("Q2_1","Q2_2","Q2_3","Q2_4"),q_analysis,data=data_scs,options=scs_q21_options,sstart=80)

############################################################################
# find the unique choices students had for the Q3 and Q5 set of questions and logically order them

scs_q31<-data_scs %>% pull("Q3_1")
scs_q31_item<-scs_q31[1] #Q3_1
scs_q31_res<-scs_q31[2:length(scs_q31)] #all Q3_1 responses

scs_q31_options<-unique(scs_q31_res) #keep unique options in MC question
scs_q31_options #check order
# Note: Effective will match with grepl in all choices, so need to indicate the beginning of the string in regexp
scs_q31_options <- c(scs_q31_options[5],scs_q31_options[4],scs_q31_options[2],"^Effective",scs_q31_options[3]) #manually order them, to make sense, 
scs_q31_options_plot <- scs_q31_options
scs_q31_options_plot[4]<-"Effective" #use this for graphs

#########################################
# Now apply function to analyze all Q3 and Q5 questions because all appear to be using the same options as MC responses

lapply(c("Q3_1","Q3_2","Q3_3","Q3_4","Q3_5","Q3_6","Q3_7"),q_analysis,data=data_scs,options=scs_q31_options,sstart=1)
lapply(c("Q5_1","Q5_2","Q5_3","Q5_4"),q_analysis,data=data_scs,options=scs_q31_options,sstart=1)

############################################################################
# Manually examine the open ended questions Q4 and Q6

scs_q4<-data_scs %>% pull("Q4")
scs_q4_item<-scs_q4[1] #Q4
scs_q4_res<-scs_q4[2:length(scs_q4)] #all Q4 responses, includes blank entries
scs_q4_res<-scs_q4_res[grepl("",scs_q4_res)] #keep only actual student responses

scs_q6<-data_scs %>% pull("Q6")
scs_q6_item<-scs_q6[1] #Q6
scs_q6_res<-scs_q6[2:length(scs_q6)] #all Q6 responses, includes blank entries
scs_q6_res<-scs_q6_res[grepl("",scs_q6_res)] #keep only actual student responses

############################################################################
# organize and clean up the data

clo_tids<-data_clo %>% pull("Target Users ID")
clo_tids<-clo_tids[2:length(clo_tids)] #all target user IDs
clo_tids_u<-unique(clo_tids) #keep unique target IDs

clo_lo<-data_clo %>% pull("Learning Outcomes Name")
clo_lo<-clo_lo[2:length(clo_lo)] #all learning outcomes
clo_lo_u<-unique(clo_lo) #keep unique learning outcomes

clo_oa<-data_clo %>% pull("Outcome Assessments Type")
clo_oa<-clo_oa[2:length(clo_oa)] #all outcome assessments
clo_oa_u<-unique(clo_oa) #keep unique outcomes assessments

clo_as<-data_clo %>% pull("Outcome Assessments Score")
clo_as<-clo_as[2:length(clo_as)] #all scores
clo_as[is.na(clo_as)]<-0. # they get a zero if they did not complete an assignment
clo_as_u<-unique(clo_as) #keep unique scores
clo_as_u<-c(clo_as_u[5],clo_as_u[1],clo_as_u[3],clo_as_u[2],clo_as_u[4]) #manually organize them in a logical way

############################################################################
# examine student performance in different types of assignments

clo_as_poll<-clo_as[grepl(clo_oa_u[1],clo_oa)]

clo_as_poll_freq<-get_freqs(clo_as_u,clo_as_poll)
clo_as_poll_N<-sum(clo_as_poll_freq)

#plot results
barplot(clo_as_poll_freq,xlab=paste("N=",clo_as_poll_N,sep=""),ylab="Frequency", main=clo_oa_u[1],cex.names=0.7,names.arg=clo_as_u,cex.main=0.7)

oa_analysis <- function(data_as, data_as_u, data_oa, oa) {
  
  data_as_tba<-data_as[grepl(oa,data_oa)]
  
  data_as_tba_freq<-get_freqs(data_as_u,data_as_tba)
  data_as_tba_N<-sum(data_as_tba_freq)
  
  #plot results
  barplot(data_as_tba_freq,xlab=paste("N=",data_as_tba_N,sep=""),ylab="Frequency", main=oa,cex.names=0.9,names.arg=data_as_u,cex.main=0.85)
  
}

#examine overall student performance in the various assessment types  
lapply(clo_oa_u,oa_analysis,data_as=clo_as,data_as_u=clo_as_u,data_oa=clo_oa)
#examine overall student performance in the various outcomes 
lapply(clo_lo_u,oa_analysis,data_as=clo_as,data_as_u=clo_as_u,data_oa=clo_lo)

############################################################################
# examine individual student performance

#make all histograms
lapply(clo_tids_u,oa_analysis,data_as=clo_as,data_as_u=clo_as_u,data_oa=clo_tids)

st_means=c()
st_sd=c()
j=1
for (t in clo_tids_u) {

  clo_as_tba<-clo_as[grepl(t,clo_tids)]

  clo_as_tba_freq<-get_freqs(clo_as_u,clo_as_tba)
  
  st_means[j]=mean(clo_as_tba)
  st_sd[j]=sd(clo_as_tba)
  j=j+1
}

hist(st_means)
hist(st_sd)







