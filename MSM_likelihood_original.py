#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 25 13:04:43 2018

@author: root
"""

import numpy as np

def gofm(inpt,kbar):
    """
    A function that calculates all the possible volatility states
    """
    m0 = inpt[1]
    m1 = 2-m0
    kbar2 = 2**kbar
    g_m1 = np.arange(kbar2)
    g_m = np.zeros(kbar2)
    
    for i in range(kbar2):
        g =1
        for j in range(kbar):
            if np.bitwise_and(g_m1[i],(2**j))!=0:
                g = g*m1
            else:
                g = g*m0
        g_m[i] = g
    return(np.sqrt(g_m))
            
def transition_mat(A,inpt,kbar):
    b = inpt[0]
    gamma_kbar = inpt[2]
    gamma = np.zeros((kbar,1))
    gamma[0,0] = 1-(1-gamma_kbar)**(1/(b**(kbar-1)))
    for i in range(1,kbar):
        gamma[i,0] = 1-(1-gamma[0,0])**(b**(i))
    gamma = gamma*0.5
    gamma = np.c_[gamma,gamma]
    gamma[:,0] = 1 - gamma[:,1]
    kbar2 = 2**kbar
    prob = np.ones((kbar2,1))
    
    for i in range(kbar2):
        for m in range(kbar):
            prob[i,0] =prob[i,0] * gamma[kbar-m-1,
                np.unpackbits(np.array([i],dtype = np.uint8))[-(m+1)]]
            
    for i in range(2**(kbar-1)):
        for j in range(i,2**(kbar-1)):
            A[kbar2-i-1,j] = prob[np.rint(kbar2 - A[i,j]-1).astype(int),0]
            A[kbar2-j-1,i] = A[kbar2-i-1,j]
            A[j,kbar2-i-1] = A[kbar2-i-1,j]
            A[i,kbar2-j-1] = A[kbar2-i-1,j]
            A[i,j] = prob[np.rint(A[i,j]).astype(int),0]
            A[j,i] = A[i,j].copy()
            A[kbar2-j-1,kbar2-i-1] = A[i,j]
            A[kbar2-i-1,kbar2-j-1] = A[i,j]
        
    return(A)

def transition_prob(inpt,state_t,kbar): 
    kbar2 = 2**kbar
    b = inpt[0]
    gamma_kbar = inpt[2]
    gamma = np.zeros(kbar)
    gamma[0] = 1-(1-gamma_kbar)**(1/(b**(kbar-1)))
    probs = np.ones(kbar2)
    #print(gamma[0,0])
    for i in range(1,kbar):
        gamma[i] = 1-(1-gamma[0])**(b**(i))
    for i in range(kbar2):
        b_xor = np.bitwise_xor(state_t,i)
        val = np.unpackbits(np.arange(b_xor,b_xor+1,dtype = np.uint16).view(np.uint8))
        val = np.append(val[8:],val[:8])[-kbar:]
        for j,v in enumerate(val):
            if v == 1:
                probs[i] = probs[i]*gamma[j]
            else:
                probs[i] = probs[i]*(1-gamma[j])
    return(probs)
        
def MSM_likelihood(inpt,kbar,data,A_template,estim_flag,nargout =1):
    if not hasattr(inpt,"__len__"):
        inpt = [estim_flag[0],inpt,estim_flag[1],estim_flag[2]]
        
    sigma = inpt[3]/np.sqrt(252)
    k2 = 2**kbar
    A = transition_mat(A_template.copy(),inpt,kbar)
    g_m = gofm(inpt,kbar)
    T = len(data)
    pi_mat = np.zeros((T+1,k2))
    LLs = np.zeros(T)
    pi_mat[0,:] = (1/k2)*np.ones((1,k2))
    """
    Likelihood Algorithm
    """
    pa = (2*np.pi)**(-0.5)
    s = sigma*g_m
    w_t = data 
    w_t = pa*np.exp(-0.5*((w_t/s)**2))/s
    w_t = w_t + 1e-16

    for t in range(T):
        
        piA = np.dot(pi_mat[t,:],A)
        C = (w_t[t,:]*piA)
        ft = np.sum(C) # log
        if np.isclose(ft,0):
            pi_mat[t+1,1]=1
        else:
            pi_mat[t+1,:] = C/ft
        LLs[t] = np.log(np.dot(w_t[t,:],piA))
        
    LL = -np.sum(LLs)
    if np.any(np.isinf(LLs)):
        print("Log-likelihood is inf. Probably due to all zeros in pi_mat.")
    if nargout == 1:
        return(LL)
    else:
        return(LL,LLs)
