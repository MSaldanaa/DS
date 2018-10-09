mydata=read.csv(file.choose(), header=TRUE)
mydata
   Current New
1      264 277
2      261 269
3      267 263
4      272 266
5      258 262
6      283 251
7      258 262
8      266 289
9      259 286
10     270 264
11     263 274
12     264 266
13     284 262
14     263 271
15     260 260
16     283 281
17     255 250
18     272 263
19     266 278
20     268 264
21     270 272
22     287 259
23     289 264
24     280 280
25     272 274
26     275 281
27     265 276
28     260 269
29     278 268
30     275 262
31     281 283
32     274 250
33     273 253
34     263 260
35     275 270
36     267 263
37     279 261
38     274 255
39     276 263
40     262 279
Current=mydata$Current
New=mydata$New
x1bar=mean(New)
x2bar=mean(Current)
s1=sd(New)
s2=sd(Current)
pooled=((length(New)-1)*s1^2+(length(Current)-1)*s2^2)/(length(New)+length(Current)-2)
sem=sqrt(pooled*((1/length(New))+(1/length(Current))))
t=(x2bar-x1bar)/sem
t
1.328362
DF=length(New)+length(Current)-2
DF
78
alpha=0.05
critical.t=qt((1-alpha/2), df=DF)
critical.t
[1] 1.990847
critical.onetailt=qt(alpha, df=DF)
critical.onetailt
[1] -1.664625
critical.rightt=qt(1-alpha, df=DF, lower.tail=FALSE)
critical.rightt
[1] -1.664625
critical.rightt=qt(1-alpha, df=DF, lower.tail=TRUE)
critical.rightt
[1] 1.664625
pvalue=2*pt(-t, df=DF)
pvalue
[1] 0.1879323
pvalue=2*pt(t, df=DF, lower.tail=FALSE)
pvalue
[1] 0.1879323
