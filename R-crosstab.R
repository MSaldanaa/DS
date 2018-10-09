
R version 3.1.1 (2014-07-10) -- "Sock it to Me"
Copyright (C) 2014 The R Foundation for Statistical Computing
Platform: i386-w64-mingw32/i386 (32-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Previously saved workspace restored]

> mydata=read.csv(file.choose(), header=TRUE)
> mydata
    Observation       Party Paycut Lobby Term
1             1    Democrat    Yes    No   No
2             2    Democrat    Yes   Yes   No
3             3    Democrat     No   Yes   No
4             4    Democrat    Yes    No  Yes
5             5    Democrat     No    No   No
6             6    Democrat     No   Yes  Yes
7             7    Democrat    Yes    No   No
8             8    Democrat     No   Yes   No
9             9    Democrat    Yes   Yes   No
10           10    Democrat    Yes    No  Yes
11           11    Democrat    Yes    No  Yes
12           12    Democrat    Yes    No  Yes
13           13    Democrat     No    No  Yes
14           14    Democrat    Yes    No  Yes
15           15    Democrat    Yes   Yes  Yes
16           16    Democrat    Yes    No   No
17           17    Democrat    Yes   Yes   No
18           18    Democrat    Yes    No  Yes
19           19    Democrat    Yes   Yes   No
20           20    Democrat    Yes   Yes  Yes
21           21    Democrat     No    No  Yes
22           22    Democrat    Yes    No   No
23           23    Democrat     No   Yes   No
24           24    Democrat     No   Yes  Yes
25           25    Democrat     No   Yes  Yes
26           26    Democrat    Yes   Yes  Yes
27           27    Democrat    Yes   Yes   No
28           28    Democrat     No   Yes  Yes
29           29    Democrat     No    No   No
30           30    Democrat    Yes   Yes   No
31           31    Democrat    Yes    No   No
32           32    Democrat     No   Yes  Yes
33           33    Democrat    Yes   Yes   No
34           34    Democrat     No   Yes  Yes
35           35    Democrat     No   Yes   No
36           36    Democrat    Yes   Yes   No
37           37 Independent     No    No  Yes
38           38 Independent     No    No  Yes
39           39 Independent    Yes   Yes  Yes
40           40 Independent    Yes   Yes   No
41           41 Independent     No   Yes   No
42           42 Independent     No    No  Yes
43           43 Independent    Yes   Yes   No
44           44 Independent     No   Yes  Yes
45           45 Independent    Yes   Yes  Yes
46           46 Independent     No   Yes   No
47           47 Independent    Yes   Yes  Yes
48           48 Independent    Yes   Yes  Yes
49           49 Independent     No   Yes   No
50           50 Independent     No    No  Yes
51           51 Independent    Yes   Yes   No
52           52 Independent    Yes   Yes  Yes
53           53 Independent    Yes   Yes   No
54           54 Independent    Yes   Yes   No
55           55 Independent     No   Yes   No
56           56  Republican    Yes   Yes  Yes
57           57  Republican    Yes    No  Yes
58           58  Republican    Yes   Yes   No
59           59  Republican    Yes   Yes  Yes
60           60  Republican     No   Yes   No
61           61  Republican    Yes   Yes   No
62           62  Republican    Yes   Yes   No
63           63  Republican    Yes   Yes  Yes
64           64  Republican    Yes   Yes  Yes
65           65  Republican    Yes   Yes  Yes
66           66  Republican    Yes   Yes   No
67           67  Republican    Yes   Yes   No
68           68  Republican    Yes    No  Yes
69           69  Republican    Yes    No  Yes
70           70  Republican    Yes   Yes  Yes
71           71  Republican     No    No  Yes
72           72  Republican     No   Yes  Yes
73           73  Republican    Yes   Yes  Yes
74           74  Republican     No    No  Yes
75           75  Republican     No   Yes  Yes
76           76  Republican    Yes    No  Yes
77           77  Republican    Yes    No  Yes
78           78  Republican    Yes   Yes   No
79           79  Republican    Yes   Yes  Yes
80           80  Republican    Yes   Yes  Yes
81           81  Republican    Yes    No  Yes
82           82  Republican    Yes    No   No
83           83  Republican     No   Yes   No
84           84  Republican    Yes   Yes  Yes
85           85  Republican    Yes   Yes   No
86           86  Republican    Yes   Yes  Yes
87           87  Republican    Yes   Yes   No
88           88  Republican    Yes   Yes   No
89           89  Republican    Yes   Yes  Yes
90           90  Republican    Yes   Yes  Yes
91           91  Republican    Yes   Yes  Yes
92           92  Republican    Yes   Yes  Yes
93           93  Republican    Yes   Yes  Yes
94           94  Republican    Yes   Yes  Yes
95           95  Republican    Yes   Yes  Yes
96           96  Republican    Yes    No  Yes
97           97  Republican    Yes   Yes  Yes
98           98  Republican    Yes   Yes  Yes
99           99  Republican    Yes    No   No
100         100  Republican    Yes   Yes  Yes
> Party=mydata$Party
> Paycut=mydata$Paycut
> Lobby=mydata$Lobby
> Term=mydata$Term
> crosstab1=table(Party, Paycut)
> Chisq1=chisq.test(crosstab1)
> Chisq1

        Pearson's Chi-squared test

data:  crosstab1
X-squared = 10.1875, df = 2, p-value = 0.006135

> crosstab1
             Paycut
Party         No Yes
  Democrat    14  22
  Independent  9  10
  Republican   6  39
> crosstab1percent=prop.table(crosstab1)
> crosstab1percent
             Paycut
Party           No  Yes
  Democrat    0.14 0.22
  Independent 0.09 0.10
  Republican  0.06 0.39
> crosstab1rowpercent=prop.table(, 1)
Error in sweep(x, margin, margin.table(x, margin), "/", check.margin = FALSE) : 
  argument "x" is missing, with no default
> crosstab1rowpercent=prop.table(crosstab1, 1)
> crosstab1rowpercent
             Paycut
Party                No       Yes
  Democrat    0.3888889 0.6111111
  Independent 0.4736842 0.5263158
  Republican  0.1333333 0.8666667
> crosstab1colpercent=prop.table(crosstab1, 2)
> crosstab1colpercent
             Paycut
Party                No       Yes
  Democrat    0.4827586 0.3098592
  Independent 0.3103448 0.1408451
  Republican  0.2068966 0.5492958
> crosstab2=table(Party, Lobby)
> crosstab2
             Lobby
Party         No Yes
  Democrat    15  21
  Independent  4  15
  Republican  11  34
> Chisq2=chisq.test(table(crosstab2))
Warning message:
In chisq.test(table(crosstab2)) :
  Chi-squared approximation may be incorrect
> Chisq2=chisq.test(crosstab2)
> Chisq2

        Pearson's Chi-squared test

data:  crosstab2
X-squared = 3.719, df = 2, p-value = 0.1557

> 
