

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

setwd("./AgriculturalData")

Y1 = read.csv("Crop_output.csv", sep = ';')
Y2 = read.csv("Livestock_output.csv", sep = ';')
Y3 = read.csv("Other_output.csv", sep = ';')

P_O_1 = read.csv("Crop_prices.csv", sep = ';')
P_O_2 = read.csv("Livestock_prices.csv", sep = ';')
P_O_3 = read.csv("Other_output_prices.csv", sep = ';')

X1 = read.csv("Capital_input.csv", sep = ';')
X2 = read.csv("Land_input.csv", sep = ';')
X3 = read.csv("Labor_input.csv", sep = ';')
X4 = read.csv("Energy_input.csv", sep = ';')
X5 = read.csv("Chimical_input.csv", sep = ';')

P_I_1 = read.csv("Capital_prices.csv", sep = ';')
P_I_2 = read.csv("Land_prices.csv", sep = ';')
P_I_3 = read.csv("Labor_prices.csv", sep = ';')
P_I_4 = read.csv("Energy_prices.csv", sep = ';')
P_I_5 = read.csv("Chimical_prices.csv", sep = ';')


YYY = cbind(Y1[,2], Y2[,2], Y3[,2])
XXX = cbind(X1[,2], X2[2], X3[,2], X4[,2], X5[,2])

k = nrow(XXX)
m = ncol(YYY)
n = ncol(XXX)

rm(XXX, YYY)
gc(reset = TRUE)

ss = 1
year = 2
kk = 2
MM = 2
dd = 1

YY = cbind(Y1[,year], Y2[,year], Y3[,year])
XX = cbind(X1[,year], X2[,year], X3[,year], X4[,year], X5[,year])

P_O = cbind(as.numeric(P_O_1[,year]), as.numeric(P_O_2[,year]), as.numeric(P_O_3[,year]))
P_I = cbind(as.numeric(P_I_1[,year]), as.numeric(P_I_2[,year]), as.numeric(P_I_3[,year]), as.numeric(P_I_4[,year]), as.numeric(P_I_5[,year]))

k = nrow(XX)
m = ncol(YY)
n = ncol(XX)

DIR_M = c(rep(">=", n + m), "=", rep("<=", k))
DIR = c(rep(">=", q_mat(sss)), "<=")
DIR_M_relax = c(DIR_M, DIR_M, rep("<=", k), rep(">=", k) )
INTEGER_var = (k + q_mat(1-sss) + 1):(k + q_mat(1-sss) + k)

LL_second_block0 = cbind(diag(k), matrix(0,k,q_mat(1-sss)) )
LL_second_block1 = cbind(-diag(k), matrix(0,k,q_mat(1-sss)) )

LL_second_block = cbind(LL_second_block0, LL_second_block1)

LLP_matrix = LP_mat(ss, MM, dd) 
bb_k = c(b_mat(ss,kk), rep(0, q_mat(1-ss)), rep(MM, k + 1))
cc_k = c(rep(0,k), p_mat(ss,kk))

out_EDM = lp(direction = "min", objective.in = cc_k, const.mat = LLP_matrix, const.dir = DIR_M, const.rhs = bb_k, int.vec = 1:k)
		



