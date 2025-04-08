

rm(list=ls(all=TRUE))
gc(reset = TRUE)

library("dplyr")
library(Rmpfr)
library(lpSolve)
library(Matrix)
library(R.utils)



#---------------------------------------------
# Functions
#---------------------------------------------


A_mat = function(s){

	if(s >= 1){
		out = t(YY)
	}else{
		out = -t(XX)
	}

	return(out)

}

q_mat = function(s){

	if(s >= 1){
		out = m
	}else{
		out = n
	}

	return(out)

}

p_mat = function(s,kkk){

	if(s >= 1){
		out = as.numeric(P_I[kkk,])
	}else{
		out = -as.numeric(P_O[kkk,])
	}

	return(out)

}

D_mat = function(s){

	if(s >= 1){
		out = diag(n)
	}else{
		out = -diag(m)
	}

	return(out)

}


b_mat = function(s, kkk){

	if(s >= 1){
		out = as.numeric(YY[kkk,])
	}else{
		out = -as.numeric(XX[kkk,])
	}

	return(out)

}


LP_mat = function(s, MM, dd){

	out = rbind( 
		cbind(A_mat(s)*(dd/MM), matrix(0, q_mat(s), q_mat(1-s))),
		cbind(A_mat(1-s)*(dd/MM), D_mat(s)),
		c(rep(1, k), rep(0, q_mat(1-s))),
		cbind(diag(rep(1, k)), matrix(0, k, q_mat(1-s)))
	)

	return(out)

}


LP_mat_eq = function(s, MM, dd, slack){

	if(slack){

		out = rbind( 
			cbind(A_mat(s)*(dd/MM), -diag(q_mat(s))),
			c(rep(1, k), rep(0, q_mat(s)))
		)

	}else{

		out = rbind( 
			cbind(A_mat(s)*(dd/MM)),
			c(rep(1, k))
		)

	}

	return(out)

}









###################################################################
###################################################################
#
# USA state-level agricultural panel data
#
###################################################################
###################################################################


#---------------------------------------------
# Load data
#---------------------------------------------

setwd(".\BankingData")

data = read.table("Aly,Grabowski,Pasurka&Rangan-1990REStat-data.txt", header = FALSE)

Names = c('ID', 'BankID', 'X_1', 'X_2', 'X_3', 'P_1', 'P_2', 'P_3', 'Y_1', 'Y_2', 'Y_3', 'Y_4', 'Y_5', 'branch-dummy')

length(Names)

names(data) = Names

XX = data[,3:5]
YY = data[,9:13]
P_I = data[,6:8] 

m = ncol(YY)
n = ncol(XX)
k = nrow(XX)

ss = 1
kk = 2
MM = 2
dd = 1

DIR_M = c(rep(">=", n + m), "=", rep("<=", k))
DIR = c(rep(">=", q_mat(ss)), "<=")
DIR_M_relax = c(DIR_M, DIR_M, rep("<=", k), rep(">=", k) )
INTEGER_var = (k + q_mat(1-ss) + 1):(k + q_mat(1-ss) + k)

LL_second_block0 = cbind(diag(k), matrix(0,k,q_mat(1-ss)) )
LL_second_block1 = cbind(-diag(k), matrix(0,k,q_mat(1-ss)) )

LL_second_block = cbind(LL_second_block0, LL_second_block1)

LLP_matrix = LP_mat(ss, MM, dd) 
bb_k = c(b_mat(ss,kk), rep(0, q_mat(1-ss)), rep(MM, k + 1))
cc_k = c(rep(0,k), p_mat(ss,kk))

out_EDM = lp(direction = "min", objective.in = cc_k, const.mat = LLP_matrix, const.dir = DIR_M, const.rhs = bb_k, int.vec = 1:k)
		



