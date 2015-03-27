#number of tests
N = 10000
#number of nodes
m = 1000
#mean time to failure
MTTF = 3312*3600*10
#repair time of single failure
MTTRS = 0.9*36000
#detect time of single failure
MTTR = 32*600
#repair time of multiple failures
MTTRN = 2*600
#mean duration of unavailability time
MTUA = 65*600

#time to failure
x = vector(length = m)
#time to detect failure
x_d = vector(length = m)
#duration of unavailability
un = vector(length = m)
#end time of failure
x_end = vector(length = m)

#total time
dl = vector(length = N)
#total time of unavailability
aun = vector(length = N)
#total unavailability time using nothing
run = vector(length = N)
#total unavailability time using MTTR
wun = vector(length = N)
#total unavailability time using MTTRN
mun = vector(length = N)
#total unavailability time using zero detection time
jun = vector(length = N)

#record number of correlated failure 
count = vector(length = N)

#test <- c(0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.6)


for(l in 1:1)
{
  #MTTRM= 1000*test[l]
  for(k in 1:N)
  {
    h=1
    a=1
    dl[k] = 0
    
    #for ttf
    u1 = runif(m)
    
    #for duration
    u2 = runif(m)
    
    #for correlated
    u3 = runif(m)
    
    ua = runif(m)
    us = runif(m)
    rd = runif(m)
    
    for(f in 1:m)
    {
        un[f] = MTUA*(-log(u2[f]))
        if(un[f] < 15*600)
          un[f] = 15*600/(ua[f]*0.87+0.13)
        else if(un[f] > 115*600)
          un[f] = 115*600*(ua[f]*0.87+0.13)
    }


    for(f in 1:m)
    {
      x[f] = MTTF*(-log(u1[f]*(1-1/exp(1)) + 1/exp(1)))*100/129
    }
  
    #produce correlated failure
    if(0){
      f=f+1

      while(a<=m)
      {
        if(f>m) {
          print("here")
          break;
        }
      
        if(us[a] > 0.8) {
          x[f] = x[h] + 1200*u3[f]
          f = f+1;
          #triple failues or more
          #h = h-1;
        }
        a = a+1;
        h = h+1;  
      }
    
      if(a==m){
        print("wrong")
        k=k-1
      }
    }
    
    #sort
    x <- sort(x)
    x_d <- ceiling(x/3000)*3000
    x_end <- (un +x)
    
    for(i in 1:(m-2))
    {
      for(j in (i+1):min(i+10,m-1))
      { 
        if(x[j]-x[i] < 1200 )
          count[k] = count[k]+1
        
        for(l in (j+1):min(j+10,m))
        {        
          if(x[l] < x_end[i] && x[l] < x_end[j] && x[j]-x_d[i] <= 0.5*36000)
          {
            run[k] = run[k] + min(x_end[i]-x[l],x_end[j]-x[l],x_end[l]-x[l])
            
            if(x[l] - x_d[j] > MTTR)
            {
              wun[k] = wun[k] + 0 
            }
            else
            {
              wun[k] = wun[k] + min(x_end[i]-x[l],x_end[j]-x[l],x_end[l]-x[l])
            }
            
            if(x[l] - x[j] > MTTRN)
            {
              jun[k] = jun[k] + 0 
            }
            else
            {
              jun[k] = jun[k] + min(x_end[i]-x[l],x_end[j]-x[l],x_end[l]-x[l])
            }
            
            if(x[l] - x_d[j] > MTTRN)
            {
              mun[k] = mun[k] + 0 
            }
            else
            {
              mun[k] = mun[k] + min(x_end[i]-x[l],x_end[j]-x[l],x_end[l]-x[l])  
            }
            
          }    
        }
      }
    }
    dl[k] = MTTF
    aun[k] = sum(un)
  }
  
  
}

#output
tt = MTTF*m*100/129
tun = mean(aun)
print(tun/tt)
print(mean(count))

tu = mean(run)
twu = mean(wun)
tmu = mean(mun)
tju = mean(jun)

print(tu/tt)
print(twu/tt)
print(tmu/tt)
print(tju/tt)

print(tmu/twu)
print(tju/twu)


#plot(y)
#print(count)
#print(x)

