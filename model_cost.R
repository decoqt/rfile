N = 1000
m = 1000
#x = vector(length = m)
#un = vector(length = m)
#x_end = vector(length = m)
MTTF = 3312*1000
MTTRS = 0.9*1000
MTTRM = 0.1*1000
MTUA = 0.25*1000
DTUA = 0.292*1000
dl = vector(length = N)
run = vector(length = N)
y = vector(length = 10)

test <- c(0.125,0.15,0.2,0.25,0.3,0.35,0.4,0.45)

testm <-c(100,200,300,400,500,600,700,800,900,1000)
cost= vector(length = 10)
costt= vector(length = 10)

for(l in 1: 10)
{
  MTTRM = 1000*0.125
  m=testm[l]
  x = vector(length = m)
  un = vector(length = m)
  x_end = vector(length = m)
  for(k in 1:N)
  {
    #print(u[1])
    u = runif(m)*0.9+0.1
    uu = runif(m)
    for(j in 1:m)
    {
      x[j] = MTTF*(-log10(u[j]))
      un[j] = MTUA*(-log10(uu[j]))
      if(un[j] > MTUA) {
        costt[l] = costt[l] + 1
      }
    }
    
    x <- sort(x)
    x_end <- (un +x)
    
    for(i in 1:(m-1))
    {
      for(j in (i+1):m)
      {
        if(un[i] < DTUA && un[j] < DTUA)
        {
          if(x_end[i] > x[j] + MTTRM && un[j] > MTTRM) 
          {
            cost[l] = cost[l] + 1
          }
        }
      }
    }
  }
}
print(cost)
print(costt)
plot(cost)
plot(cost/costt)

