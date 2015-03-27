#number of tests
N = 100
#number of nodes
m = 100
#x = vector(length = m)
#mean time to failure
MTTF = 5*365*24*1000
#repair time of single failure
MTTRS = 0.9*1000
#repair time of multiple failures
MTTRM = 0.2*1000

#data loss time
dl = vector(length = N)

y = vector(length = 10)
z = vector(length = 10)

#test of different MTTRM
testcase <- c(100)
tt <- c(0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.6) 

for(l in 1:1)
{
  
  m = 100
  #time to failure of each node
  x = vector(length = m)
  print(m)
  for(k in 1:N)
  {
    safe = 1
    dl[k] = 0
    MTTRM = 1000*tt[l]
    while(safe == 1)
    {
      
      #print(u[1])
      u = runif(m)*0.9+0.1
      for(j in 1:m)
      {
        x[j] = MTTF*(-log(u[j])) 
      }
      
      x <- sort(x)
      
      for(i in 1:(m-2))
      {
        if(x[i+1] - x[i] < MTTRS && x[i+2] - x[i] < MTTRS) 
        {
          if( x[i+2] - x[i+1] < MTTRM )
          {
            safe = 0
            dl[k] = dl[k] + MTTF
            #print(dl[k])
            break
          }
        }
      }
      dl[k] = dl[k] + MTTF
    }
    #print(dl[k])
  }
  y[l] = mean(dl)/1000
}

print(y)
