data1=read.csv(file.choose(), header=TRUE)
data1
   Sample1 Sample2 Sample3 Sample4
1    11.55   11.62   11.91   12.02
2    11.62   11.69   11.36   12.02
3    11.52   11.59   11.75   12.05
4    11.75   11.82   11.95   12.18
5    11.90   11.97   12.14   12.11
6    11.64   11.71   11.72   12.07
7    11.80   11.87   11.61   12.05
8    12.03   12.10   11.85   11.64
9    11.94   12.01   12.16   12.39
10   11.92   11.99   11.91   11.65
11   12.13   12.20   12.12   12.11
12   12.09   12.16   11.61   11.90
13   11.93   12.00   12.21   12.22
14   12.21   12.28   11.56   11.88
15   12.32   12.39   11.95   12.03
16   11.93   12.00   12.01   12.35
17   11.85   11.92   12.06   12.09
18   11.76   11.83   11.76   11.77
19   12.16   12.23   11.82   12.20
20   11.77   11.84   12.12   11.79
21   12.00   12.07   11.60   12.30
22   12.04   12.11   11.95   12.27
23   11.98   12.05   11.96   12.29
24   12.30   12.37   12.22   12.47
25   12.18   12.25   11.75   12.03
26   11.97   12.04   11.96   12.17
27   12.17   12.24   11.95   11.94
28   11.85   11.92   11.89   11.97
29   12.30   12.37   11.88   12.23
30   12.15   12.22   11.93   12.25
 
Sample1=data1$Sample1
Sample2=data1$Sample2
Sample3=data1$Sample3
Sample4=data1$Sample4
x1bar=mean(Sample1)
x2bar=mean(Sample2)
x3bar=mean(Sample4)
x3bar=mean(Sample3)
x4bar=mean(Sample4)
#Null Hypothesis is Mu=12
#Alternative Hypothesis is Mu not =12
n=30
sigma=0.21
Mu=12
z1=(x1bar-Mu)/(sigma/sqrt(n))
z2=(x2bar-Mu)/(sigma/sqrt(n))
z3=(x3bar-Mu)/(sigma/sqrt(n))
z4=(x4bar-Mu)/(sigma/sqrt(n))
computedz=c(z1, z2, z3, z4) 
computedz
[1] -1.0780571  0.7476848 -2.8951049  2.1213382
alpha=0.01
criticalz=qnorm(1-alpha/2)
criticalz
[1] 2.575829

pvaluez1=2*(1-pnorm(abs(z1)))
pvaluez2=2*(1-pnorm(abs(z2)))
pvaluez3=2*(1-pnorm(abs(z3)))
pvaluez4=2*(1-pnorm(abs(z4)))
pvalue=c(pvaluez1, pvaluez2, pvaluez3, pvaluez4)
pvalue
[1] 0.281008276 0.454650325 0.003790318 0.033893355
sd1=sd(Sample1)
sd2=sd(Sample2)
sd3=sd(Sample3)
sd4=sd(Sample4)
SampleSds=c(sd1,sd2,sd3,sd4)
SampleSds
[1] 0.2203560 0.2203560 0.2071706 0.2061090
confidenceL1=mean(Sample1)-sd(Sample1)/sqrt(n)*criticalz
confidenceU1=mean(Sample1)+sd(Sample1)/sqrt(n)*criticalz
confidence1=c(confidenceL1, confidenceU1)
confidence1
[1] 11.85504 12.06230


