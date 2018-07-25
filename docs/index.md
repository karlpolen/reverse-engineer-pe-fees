# PE Fees Reverse Engineering


```r
source("basic financial.r",echo=FALSE)
require(zoo)
require(lubridate)
require(ggplot2)

#generate a cash flow for analysis
cf=zoo(c(-100,-200,200),as.Date("2015-1-1")+c(15,200,400))
nav=zoo(200,as.Date("2016-12-31"))
irr.z(mergesum.z(cf,nav))
```

```
## [1] 0.2840366
```



```r
cu=0.5
cry=.2
pref=.08
x=pfd.return(mergesum.z(cf,nav),int=pref,4,as.Date("2016-12-31"))
prefearned=sum(x$pref.paid)+lastinvec(x$acc.pref)
profitpaid=max(0,sum(x$pref.paid)+sum(x$residual)-lastinvec(x$unreturned))
excess1=profitpaid-prefearned
cuagg=lpsharecu=gpsharecu=0
if(cu>0) {
  cuagg=prefearned*cry/(cu-cry)
  lpsharecu=(1-cu)*cuagg
  gpsharecu=cu*cuagg
  if(excess1<lpsharecu) {
    cumult=lpsharecu/excess1
    lpsharecu=lpsharecu*cumult
    gpsharecu=gpsharecu*cumult
  }
}
excess2=excess1-lpsharecu
incentfee=gpsharecu+(excess2*(-1+1/(1-cry)))
incentfee
```

```
## 2016-12-31 
##         25
```

```r
profitpaid
```

```
## [1] 100
```

```r
incentfee/(profitpaid+incentfee)
```

```
## 2016-12-31 
##        0.2
```

Turn above in to a function.


```r
incentfee.rev=function(nav,cf,cu,pref=.08,cry=.2,fre=4) {
  #
  # reverse engineer an incentive fee from following info
  # nav-- a one element time series of the investor net asset value
  # cf -- a time series of calls (negative) and distributions (positive), 
  #       values after date of nav are ignored
  # cu -- the catchup percent as a decimal
  # pref -- the preferred return rate as a decimal
  # cry -- the carry rate as a decimal
  # fre -- the periodicity of calculation, 4 for quarterly (the default and industry convention)
  #
  # returns a zoo data.frame object which is the incentive fee earned cumulatively through date of nav
  # only works for a one level structure with catchup
  # calculated on accrual and cash basis, both with european waterfall style calculations
  #
  # data validation
  if (!is.zoo(cf)) stop ("cf must be a zoo object")
  if (!is.zoo(nav)) stop ("nav must be a zoo object")
  if (cu<0|cu>1) stop ("cu must be 0<=cu<=1")
  if (pref<0|pref>1) stop ("pref must be 0<=pref<=1")
  if (cry<0|cry>1) stop ("cry must be 0<=cry<=1")
  if (cu!=0&cu<=cry) stop ("cu must be zero or > cry")
  if (!fre%in%c(1,4,12)) stop("freq must be 1, 4 or 12")
  cf=cf[time(cf)<=time(nav)]
  if(length(cf)==0) {
    warning ("no cash flows on or before nav date")
    return(NULL)
  }
  if(!any(cf<0)) {
    warning("no cash flows less than zero")
    return(NULL)
  }
  #calculate fee accrual basis
  #calculate preferred return dollars
  x=pfd.return(mergesum.z(cf,nav),int=pref,freq=fre,mdate=time(nav))
  prefearned=sum(x$pref.paid)+lastinvec(x$acc.pref)
  profitpaid=max(0,sum(x$pref.paid)+sum(x$residual)-lastinvec(x$unreturned))
  #calculate the incentive fee
  excess1=max(0,profitpaid-prefearned)
  #catch up layer
  lpsharecu=gpsharecu=0
  if(cu>0&excess1>0) {
    cuagg=prefearned*cry/(cu-cry)
    lpsharecu=(1-cu)*cuagg
    gpsharecu=cu*cuagg
    if(excess1<lpsharecu) {
      cumult=lpsharecu/excess1
      lpsharecu=lpsharecu*cumult
      gpsharecu=gpsharecu*cumult
    }
  }
  #parri passu layer
  excess2=excess1-lpsharecu
  incentfee=gpsharecu+(excess2*(-1+1/(1-cry)))
  
  #calculate fee paid w european waterfall
  x2=pfd.return(cf,int=pref,freq=fre,mdate=time(nav))
  prefearned=sum(x2$pref.paid)+lastinvec(x2$acc.pref)
  profitpaid=max(0,sum(x2$pref.paid)+sum(x2$residual)-lastinvec(x2$unreturned))
  #calculate the incentive fee
  excess1=max(0,profitpaid-prefearned)
  #catch up layer
  lpsharecu=gpsharecu=0
  if(cu>0&excess1>0) {
    cuagg=prefearned*cry/(cu-cry)
    lpsharecu=(1-cu)*cuagg
    gpsharecu=cu*cuagg
    if(excess1<lpsharecu) {
      cumult=lpsharecu/excess1
      lpsharecu=lpsharecu*cumult
      gpsharecu=gpsharecu*cumult
    }
  }
  #parri passu layer
  excess2=excess1-lpsharecu
  incentfee2=gpsharecu+(excess2*(-1+1/(1-cry)))
  
  #return answer
  return (data.frame(Accrual=zoo(incentfee,time(nav)),Cash=zoo(incentfee2,time(nav))))
}
```

Let's try it out on the above simplified data, then some real data.


```r
#test with hypothetical cf and nav
ifee=incentfee.rev(nav,cf,cu=.5,cry=.2,pref=.08)
ifee
```

```
##            Accrual Cash
## 2016-12-31      25    0
```

```r
ifee/(ifee+sum(mergesum.z(cf,nav)))
```

```
##            Accrual Cash
## 2016-12-31     0.2    0
```

```r
#test on Apollo 7
# load("../private investment performance/pmedata.rdata")
# source("../basic financial.r")
# fundname="Aplo7"
# cfaplo=y.cf[[fundname]]
# nav1=y.v[[fundname]]
# nav2=y.hv[[fundname]]
# nav2=nav2[time(nav2)!=time(nav1)]
# navaplo=mergesum.z(nav1,nav2)
# infee=incentfee.rev(lastinvec(navaplo),cfaplo,cu=1,pref=.08,cry=.2)
# infee
# infee/(infee+sum(mergesum.z(cfaplo[time(cfaplo)<=time(lastinvec(navaplo))],lastinvec(navaplo))))
# #infee=sapply(as.list(navaplo),incentfee.rev,cfaplo,cu=1,pref=.08,cry=.2)
# infeevec=data.frame(Accrual=zoo(),Cash=zoo())
# for (i in 1:length(navaplo)) {
#   infeevec[i,]=incentfee.rev(navaplo[i],cfaplo,cu=1,pref=.08,cry=.2)
# }
# infeevec
# apply(infeevec,2,diff)
```


