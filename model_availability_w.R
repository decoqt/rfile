#number of tests
N = 1000
#number of nodes
m = 1000
#mean time to failure
MTTF = 4.6*30*24*3600*10
#repair time of single failure
MTTRS = 0.9*36000
#detect time of single failure
MTTR = 30*600
#repair time of multiple failures
MTTRN = 2*60
#our repair time of multiple failures
MTTRO = 12.8*10
#our aggresive methods
MTTRAG = 1.28*10
#mean duration of unavailability time
MTUA = 65*600
#detect time of google
MTTRG = 15*600

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
#total unavailability time using google detection time
gun = vector(length = N)
#total unavailability time using our detection time
oun = vector(length = N)
#total unavailability time using aggresive detection time
agun = vector(length = N)

#record number of correlated failure 
count = vector(length = N)

test <- c(0,20,40,80,100,120,140,160)

tt = 8
tu <- vector(length = tt)
twu <- vector(length = tt)
tju <- vector(length = tt)
tgu <- vector(length = tt)
tmu <- vector(length = tt)
tou <- vector(length = tt)
tagu <- vector(length = tt)

for(cc in 1:tt)
{
  
  #MTTRM= 1000*test[l]
  for(k in 1:N)
  {
    run[k]=0
    wun[k]=0
    jun[k]=0
    mun[k]=0
    gun[k]=0
    oun[k]=0
    agun[k]=0
    un[k]=0
    aun[k]=0
    x[k]=0
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
        un[f] = 15*600/(u2[f]*0.5+0.5)
      #else if(un[f] > 115*600)
      #  un[f] = 115*600*(ua[f]*0.5+0.5)
    }
    
    
    for(f in 1:(m-test[cc]))
    {
      x[f] = MTTF*(-log(u1[f]*(1-1/exp(1)) + 1/exp(1)))*100/129
    }
    
    #produce correlated failure
    if(1){
      f=f+1
      
      while(a<=m)
      {
        if(f>m) {
          #print("here")
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
          if(x[l] < x_end[i] && x[l] < x_end[j])
          {
            if(x[l] - x_d[i] <= MTTR + MTTRS){
              run[k] = run[k] + min(x_end[i]-x[l],x_end[j]-x[l],x_end[l]-x[l])
            }
            
            if(x[l] - x_d[j] > MTTR || x[l] - x_d[i] > MTTR + MTTRS)
            {
              wun[k] = wun[k] + 0 
            }
            else
            {
              wun[k] = wun[k] + min(x_end[i]-x[l],x_end[j]-x[l],x_end[l]-x[l])
            }
            
            if(x[l] - x_d[j] > MTTRO || x[l] - x_d[i] > MTTR + MTTRS)
            {
              oun[k] = oun[k] + 0 
            }
            else
            {
              oun[k] = oun[k] + min(x_end[i]-x[l],x_end[j]-x[l],x_end[l]-x[l])
            }
            
            if(x[l] - x_d[j] > MTTRAG || x[l] - x_d[i] > MTTR + MTTRS)
            {
              agun[k] = agun[k] + 0 
            }
            else
            {
              agun[k] = agun[k] + min(x_end[i]-x[l],x_end[j]-x[l],x_end[l]-x[l])
            }
            
            if(x[l] - x[j] > MTTRN || x[l] - x_d[i] > MTTR + MTTRS)
            {
              jun[k] = jun[k] + 0 
            }
            else
            {
              jun[k] = jun[k] + min(x_end[i]-x[l],x_end[j]-x[l],x_end[l]-x[l])
            }
            
            if(x[l] - x_d[j] > MTTRN || x[l] - x_d[i] > MTTR + MTTRS)
            {
              mun[k] = mun[k] + 0 
            }
            else
            {
              mun[k] = mun[k] + min(x_end[i]-x[l],x_end[j]-x[l],x_end[l]-x[l])  
            }
            
            if(x[l] - x_d[j] > MTTRG || x[l] - x_d[i] > MTTR + MTTRS)
            {
              gun[k] = gun[k] + 0 
            }
            else
            {
              gun[k] = gun[k] + min(x_end[i]-x[l],x_end[j]-x[l],x_end[l]-x[l])  
            }
            
          }    
        }
      }
    }
    dl[k] = MTTF
    aun[k] = sum(un)
  }
  #output
  print(cc)
  tt = MTTF*m*100/129
  tun = mean(aun)
  print(tun/tt)
  print(mean(count))
  
  tu[cc] = mean(run)
  twu[cc] = mean(wun)
  tmu[cc] = mean(mun)
  tju[cc] = mean(jun)
  tgu[cc] = mean(gun)
  tou[cc] = mean(oun)
  tagu[cc] = mean(agun)
  
  print(tu[cc]/tt)
  print(twu[cc]/tt)
  print(tmu[cc]/tt)
  print(tju[cc]/tt)
  print(tgu[cc]/tt)
  print(tou[cc]/tt)
  print(tagu[cc]/tt)
  
  print(twu[cc]/tu[cc])
  print(tgu[cc]/tu[cc])
  print(tagu[cc]/tu[cc])
  print(tou[cc]/tu[cc])
  print(tmu[cc]/tu[cc])
  print(tju[cc]/tu[cc])
}



plot(tmu/twu)
#print(count)
#print(x)

