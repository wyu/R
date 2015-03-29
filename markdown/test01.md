Quick example of a simple QC plot for a LC-MS/MS run
========================================================

QC of proteomics data is often neglected many published studies or internal projects. This is unfortunate as the overall thesis will be shaky at the best if due dilligent is not observed.

The summary data was generated from an LC-MS/MS analysis using another program written in Java. It collects many useful statistics as the function of peptide retention time.


```r
library(lattice)
thro = read.csv(stringsAsFactors = F, row.names = NULL, "/home/wyu/Projects/R/data/sputum_thro_59881.csv")
```


The content of the summary stats is shown below.

```r
head(thro)
```

```
##       row id.prj id.expt id.run      name         desc rt.pct rt width
## 1 59881_0   1002   29687  59881 A10-07081 U2OS Ctrl 81      0  0     5
## 2 59881_0   1002   29687  59881 A10-07081 U2OS Ctrl 81      0  0     5
## 3 59881_0   1002   29687  59881 A10-07081 U2OS Ctrl 81      0  0     5
## 4 59881_0   1002   29687  59881 A10-07081 U2OS Ctrl 81      0  0     5
## 5 59881_0   1002   29687  59881 A10-07081 U2OS Ctrl 81      0  0     5
## 6 59881_0   1002   29687  59881 A10-07081 U2OS Ctrl 81      0  0     5
##   slice   stat annot val
## 1    mw i2nd.n false  44
## 2    mw i2nd.n  true   4
## 3    mw ions.n false  44
## 4    mw ions.n  true   4
## 5    mw  ppm.n  true   4
## 6    mw    z.n false  44
```



```r
# remove the row col
thro$row = NULL
# convert the columns
thro$id.run = as.factor(unique(thro$id.run))
thro$id.prj = as.factor(unique(thro$id.prj))
thro$id.expt = as.factor(unique(thro$id.expt))
thro$rt.pct = as.numeric(thro$rt.pct)
thro$rt = as.numeric(thro$rt)
thro$width = as.numeric(thro$width)
thro$val = as.numeric(thro$val)
thro$annot = as.logical(thro$annot)

junk = subset(thro, slice == "mw" & annot == TRUE & stat == "apex")
```


As an example, we can plot the MS1 intensities (apex) verus RT


```r
xyplot(val ~ rt.pct | name, data = junk, type = c("p", "l"), scales = "free")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


