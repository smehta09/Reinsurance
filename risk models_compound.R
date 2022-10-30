# Risk models
#compund distribution
set.seed(8143)
n <- rpois(1000,100)
s <- numeric(1000)
for(i in 1:1000) {
  x <- rexp(n[i], rate=0.002)
  s[i] <- sum(x)
}  
s[98]

# compound with x= gamma
set.seed(123)
n <- rpois(10000,800)
s <- numeric(10000)
for(i in 1:10000) {
  x <- rgamma(n[i],shape=900,rate=2)
  s[i] <- sum(x)
} 
#XOL arrangement in compound distribution where z is reinsurer claims 


set.seed(250) 
M <-500 
n <- rpois(1000,500) 
s <- numeric(1000) 
for(i in 1:1000) 
{x <- rgamma(n[i],shape=600,rate=0.3) 
z <- pmax(0,x-M) 
s[i] <- sum(z)} 
s[500] 

#proportional arrangement in compound distribution where z is reinsurer claim
set.seed(250) 
M <-0.25 
n <- rpois(1000,500) 
s <- numeric(1000) 
for(i in 1:1000) 
{x <- rgamma(n[i],shape=600,rate=0.3) 
z <- (1-M)* x 
s[i] <- sum(z)} 
s[500] 



#probability that the insurer's total claim payments in a year exceed
#350,000 like this
length(s[s>350000])/length(s) 
#median using quantile function
quantile(s,0.5) 
# compound distribution of reinsurance is covered in 179.


#when 3 types of insurance is given in a portfolio with different n's
# and claim distributions with prob of claim, n=n1+n2+n3, IAI June 19 Q2
set.seed(2000)
rate1 <- 0.006
q1 <- 0.002
n1 <- 300
rate2 <- 0.007
q2 <- 0.001
n2 <- 500
rate3 <- 0.006
q3 <- 0.003
n3 <- 200
Sumofclaims_sim <- numeric(5000) 
Total_policies = n1+n2+n3
for (j in 1:5000){
  x<-numeric(Total_policies)
  for (i in 1:Total_policies) 
  {if (i<= n1){rate=rate1} else{if(i<=n1+n2){rate=rate2}else{rate=rate3}}
    x[i] <- rexp(1,rate=rate)}
  
  death<-numeric(1000)
  for (i in 1:Total_policies) 
  {if (i<= n1){prob=q1} else{if(i<=n1+n2){prob=q2}else{prob=q3}}
    death[i] <- rbinom(1,size=1,prob)}
  
  sim_claim <- x*death
  sim_claim<-sum(sim_claim)
  Sumofclaims_sim[j] <- sim_claim
}
Sumofclaims_sim



