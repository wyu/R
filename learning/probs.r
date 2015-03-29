# sampling N times from a large population with A or B status, 
population=100000; red=55; sample=c(1000);
dhyper(c(1,3,55), red, population-red, sample);

# http://venus.unive.it/romanaz/statistics/lab_2012.html
# STATISTICS for ECONOMICS & MANAGEMENT
# LABORATORY 8 - MARCH 22nd, 2012
# BINOMIAL, HYPERGEOMETRIC AND OTHER DISCRETE DISTRIBUTIONS
############################################################################
# BASIC COMBINATORIAL FUNCTIONS
#FACTORIAL
factorial(10)
#COMBINATIONS, BINOMIAL COEFFICIENT
choose(10,4)
#COMPUTING BINOMIAL PROBABILITIES
# key function dbinom
# ?dbinom # get help
# A fair die is flipped 10 times.
# Define a "success" the occurrence of 1 or 6
# Problem 1: compute the prob of 5 successes
dbinom(5,10,1/3)                 # 0.1365645 
# Problem 2: compute the prob of more than 5 successes
sum(dbinom(6:10,10,1/3))         # 0.07656353 
# Problem 3: binomial prob histogram
100*round(dbinom(0:10,10,1/3),4) # prob table (%)    
#  1.73  8.67 19.51 26.01 22.76 13.66  5.69  1.63  0.30  0.03  0.00
plot(0:10,dbinom(0:10,10,1/3),type="h",lwd=3,
     xlab="No. of Successes",ylab="Binomial Probs",
     main="Binomial Distr. Bi(n=10, p=1/3)")  # binomial prob histogram
Problem 4 Solve Ex. 2.13 Rice

#COMPUTING HYPERGEOMETRIC PROBABILITIES
# key function dhyper
# ?dhyper # get help
# You choose at random, without replacement, 10 balls
# from the box containing 20 red (R), 50 white (W), 30 green (G) balls.
# Let us consider the number of G balls in the sample
# Problem 1: compute the prob of 5 G 
dhyper(5,30,70,10)                 # 0.09963728 
# Problem 2: compute the prob of at least 1 G
sum(dhyper(1:10,30,70,10))         # 0.9770828  
1-dhyper(0,30,70,10)               # 0.9770828

###################################################
# HYPERGEOMETRIC VERSUS BINOMIAL
n <- 10
probs <- data.frame(0:n,(0:n)/n,100*round(dbinom(0:n,n,0.3),4),
                    100*round(dhyper(0:10,30,70,n),4))
names(probs) <- c("abs_fr","rel_fr","bin_p","hyp_p")
probs
nsucc bin_p hyp_p
1      0  2.82  2.29
2      1 12.11 11.27
3      2 23.35 23.72
4      3 26.68 28.12
5      4 20.01 20.76
6      5 10.29  9.96
7      6  3.68  3.15
8      7  0.90  0.64
9      8  0.14  0.08
10     9  0.01  0.01
11    10  0.00  0.00
plot(0:10,dbinom(0:n,n,0.3),type="h",ylim=c(0,0.3),lwd=3,
     xlab="No. of Successes",ylab="Probs",
     main="Bi(n=10,p=0.3) vs HG(n=10,30,70)")
points((0:10)+0.2,dhyper(0:n,30,70,n),type="h",col="red",lwd=3)
legend("topright",col=c("black","red"),lty="solid",
       legend=c("Binomial","Hypergeometric"))
# SUPPOSE TO SAMPLE A LARGER POPULATION (1000 BALLS)
# WITH THE SAME PROPORTION OF GREEN BALLS
plot(0:10,dbinom(0:n,n,0.3),type="h",ylim=c(0,0.3),lwd=3,
     xlab="No. of Successes",ylab="Probs",
     main="Bi(n=10,p=0.3) vs HG(n=10,300,700)")
points((0:10)+0.2,dhyper(0:n,300,700,n),type="h",col="red",lwd=3)
legend("topright",col=c("black","red"),lty="solid",
       legend=c("Binomial","Hypergeometric"))
# WHAT DO YOU OBSERVE? WHAT IMPLICATIONS FOR STATISTICAL SAMPLING?
#####################################################
####################################################
# BEYOND DICHOTOMY
# MULTINOMIAL AND MULTIVARIATE HYPERGEOMETRIC
# YOU DRAW AT RANDOM 6 ITALIAN REGIONS (WR OR W/O R)
# AND CONSIDER THE SAMPLE FREQUENCY OF NORTHERN (N), CENTRAL (C) AND SOUTHERN (S) REGIONS
# RECALL: THERE ARE 8 NORTHERN, 4 CENTRAL AND 8 SOUTHERN REGIONS (ISTAT DEFINITION)
# PROBLEM 1 PROB OF 2N, 2C, 2S
# WR SAMPLING
dmultinom(c(2,2,2),6,c(0.4,0.2,0.4))
# W/O R SAMPLING
choose(8,2)*choose(4,2)*choose(8,2)/choose(20,6)
# PROBLEM 2 PROB OF NO CENTRAL REGIONS
# PROBLEM 3 PROB OF SAME NUMBER OF NORTHERN AND SOUTHERN REGIONS
# PROBLEM 4 PROB OF VENETO AND LAZIO REGIONS IN THE SAMPLE (ONLY W/O R SAMPLING)


