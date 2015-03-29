loadCleanPartAData = function(dir,fileNames){
  
  #read data tables from NZ site preparation study
  rawData = lapply(fileNames,
                   function(fileName){
                     read.csv(paste(dir,"/",fileName,sep=""),stringsAsFactors=FALSE)
                   }
                   )
  names(rawData) = sub(".csv","",fileNames)
  
  d = cleanPartAData(rawData[[1]])
  ##modify column types for better analysis and plotting
  ##d = rawData$SiteValidation_Mock
  ##d = rawData$AMG357_NZ_Operator_QC_datadrop_reformat
  #d = rawData[[1]]
  ##d = subset(d, BCells!="n/a")
  #d$BCells = as.numeric(d$BCells)
  #d$BCells[d$BCells=="n/a"] = NA
  ## copy the anti-IgD data to a new column
  #d$antiIgDValue = d$antiIgD
  #d$antiIgDValue[d$antiIgD=="PBS"] = 0.008 / 100   # an artificial background for plotting purpose
  #d$antiIgDValue[d$antiIgD=="NPC"] = 0.008 / 10000 # an artificial background for plotting purpose
  #d$antiIgDValue = as.numeric(d$antiIgDValue)
  #d$well = sub(".fcs","",d$well,perl=FALSE)
  
  #colNamesToFactor = c("patientId", "antiIgD", "Period")

  #for(i in match(colNamesToFactor,names(d))){
  #  d[[i]] = as.factor(d[[i]])
  #}
  
  #define measure variables for main data table
  #measureVariables = c("BCells")

  # disable them for now to avoid error: "Error in if (ordered) "ordered" :   argument is not interpretable as logical"
  #d$patientId = factor(d$patientId, 
  #                     level=c("1001","1002","1003","1004","1006","1010", "A1004", "A1348"), ordered=TRUE)
  #d$antiIgD   = factor(d$antiIgD, 
  #                     level=c("NPC", "PBS", "0.008", "0.04", "0.2", "0.5", "1", "5", "10", "25"), ordered=TRUE)  
  list(rawData=rawData, d=d, measureVariables=c("BCells"))
}
