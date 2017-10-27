# debug mode handling
debug = F

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  # default input & output files
  input_file <- "ct-results.csv"
  experiment_plate_setup = "plate-setup.csv"
  output_file <- "conc-results.json"
  #experiment_plate_setup = "experiment-plate-setup.csv"
  #experiment_plate_setup = "NA"

	# additional default values needed
	omitted_unknowns = ""
	omitted_standards = "" # string of comma separated userOmitted wells
	outlier.method = "basic" # possible values:- advanced, cv, rec
	outlier.sig.level = "15,70-130" # possible values:- 15,70-130 for basic; 0.01 for advanced
	npars = "5"
	InputType = "eds"
	deprendo_5pl_script <- "deprendo-5pl-aws.r"
	output_file <- "."
	plateType <- 'RG-96'

	# debugging info. change to the local settings
	if (debug) {
		deprendo_5pl_script <- "C:/3-projects/deprendo/dev4/Deprendo-App/deprendo-svc/src/main/rscript/R_refactored/deprendo-5pl-aws.r"
		# deprendo_5pl_script <- "C:/3-projects/deprendo/tasks/task-run-rscript/deprendo-5pl-aws.r"
		plateType <- 'RG-96'
		output_file <- "C:/3-projects/deprendo/tasks/task-prism-export/david-0711/loc-proc"
	}
} else if (length(args)==1) {
  # default output file
  input_file <- args[1]
  input_plate_setup = "plate-setup.csv"
  output_file <- "conc-results.json"
} else if (length(args)==2) {
  # default output file
  input_file <- args[1]
  input_plate_setup = args[2]
  output_file <- "conc-results.json"
} else {
  # no default
 
  deprendo_5pl_script <- args[1]
  input_file <- args[2]
  #input_plate_setup = args[2]
  #* 2nd argument to read plate-setup.csv with Serum Percentage column
  experiment_plate_setup = args[3]
  #*
  #* 4th argument while invoking R should have userOmitted std wells Or NA as default
  omitted_standards = args[6]
  #*
  npars <- args[7]
  outlier.method <- args[8]
  outlier.sig.level <- args[9]
  InputType <- args[10]
  omitted_unknowns <- args[11]
  plateType <- args[12]
  output_file <- args[13]
}

setwd(output_file)
#deprendo_5pl_script <- "deprendo-5pl-aws.r"
source(deprendo_5pl_script)

ip <- as.data.frame(installed.packages()[,c(1,3:4)])
rownames(ip) <- NULL
ip <- ip[is.na(ip$Priority),1:2,drop=FALSE]
print(ip, row.names=FALSE)

packageVersion("stats")
print(packageVersion("stats"))

.Machine

####
#### Following parameters are passed from UI and read as arguments by R.
#### For local testing un-comment and initialize theses parameters here.
####
#omitted_unknowns = ""
#omitted_standards = "" # string of comma separated userOmitted wells
#outlier.method = "basic" # possible values:- advanced, cv, rec
#outlier.sig.level = "15,70-130" # possible values:- 15,70-130 for basic; 0.01 for advanced
#npars = "5"
#InputType = "eds"
# row_wise
#A1
#A2
#A3

# column_wise
#A1
#B1
#C1

# row_column
# A1,A2,A3...
# B1,B2,B3...

omittedUnkWells = unlist(strsplit(omitted_unknowns, split=","))
omittedStdWells = unlist(strsplit(omitted_standards, split=","))
npars = as.numeric(npars)

print("############## arguments list:- #################")
print(deprendo_5pl_script)
print(input_file)
print(experiment_plate_setup)
print(omittedStdWells)
print(omittedUnkWells)
print(npars)
print(outlier.method)
print(outlier.sig.level)
print(InputType)
print(output_file)

#* adding below code to resolve QS7 data analysis error
#plate_setup.df$Quantity = round(plate_setup.df$Quantity, digits = 6)
#*

#dr.df = read.csv(text = gsub(",+$", "", readLines(input_file)), header = T)
#dr.df$exp.concentration = plate_setup.df$Quantity
#dr.df$Quantity = plate_setup.df$Quantity

parse3rdPartyData <- function(input_file, InputType){
  ct_results.df <- data.frame()
  ct_input = read.csv(input_file, header = F, na.strings = c("-","NA","NaN","Undetermined"," ")) # na.strings will take care of missing values i.e. "-","NA","NaN","Undetermined"," "
  names(ct_input) <- NULL

  # check 3rd party input format
  if (all.equal(dim(ct_input),c(96,1))==TRUE){
    ct_values <- ct_input[,1]
    #do.transpose = F
  }else if (all.equal(dim(ct_input),c(1,96))==TRUE) {
    ct_values <- t.data.frame(ct_input)[,1]
    #do.transpose = T
  }else if(all.equal(dim(ct_input),c(8,12))==TRUE){
    ct_values_list <- list()
    for (i in c(1:nrow(ct_input))){
      ct_values_list <- append(ct_values_list, ct_input[i,])
    }
    ct_values <- unlist(ct_values_list)
  }
  else {
    stop("3rd party input file is not in correct format")
  }

  if (InputType == "row_wise"){
    #ct_results.df <- parse.row.wise(ct_input, do.transpose)
    ct_results.df <- parse.row.wise(ct_values)
  }else if (InputType == "column_wise") {
    #ct_results.df <- parse.col.wise(ct_input, do.transpose)
    ct_results.df <- parse.col.wise(ct_values)
  }else if (InputType == "row_column") {
    ct_results.df <- parse.row.col.mix(ct_values)
  }
  return(ct_results.df)
}


# buids ct results data frame from a file.
# the input file must be a csv file in a matrix form: row x col
build_ct_results <- function(f_ct_marix) {
  ct.m <- as.matrix(read.csv(
    f_ct_marix,
    header = F,
    na.strings = c("-", "NA", "NaN", "Undetermined", " ")
  ))

  rows <- nrow(ct.m)
  cols <- ncol(ct.m)
  ct_values <- as.vector(t(ct.m))

  row.h = LETTERS[seq(from = 1, to = rows)]
  col.h = c(1:cols)

  well.h = paste(rep(row.h, each = cols), rep(col.h, rows), sep = '')

  ct_results = data.frame(Ct = ct_values,
                          Well = well.h,
                          stringsAsFactors = F)

  return(ct_results)
}

dr.df <- data.frame()
if (InputType=="eds"){
  ct_results.df = read.csv(text = gsub(",+$", "", readLines(input_file)), header = T,na.strings = c("-","NA","NaN","Undetermined"," "))
  #ct_results.df$Ct <- round(ct_results.df$Ct, 1)
  experiment_plate_setup.df = read.csv(experiment_plate_setup, header = T, skip = 1)
  make.names(names(experiment_plate_setup.df))
  experiment_plate_setup.df$Quantity[which(experiment_plate_setup.df$Task!="STANDARD" & experiment_plate_setup.df$Quantity==0)] <- NA
  experiment_plate_setup.df$Well <- NULL
  dr.df = merge(x=experiment_plate_setup.df[which(experiment_plate_setup.df$Well.Position %in% ct_results.df$Well),], y=ct_results.df[,c("Well","Ct")], by.x="Well.Position", by.y="Well", sort=F)
  colnames(dr.df)[colnames(dr.df) == 'Sample.Name'] <- 'Sample'
  colnames(dr.df)[colnames(dr.df) == 'Well.Position'] <- 'Well'
  colnames(dr.df)[colnames(dr.df) == 'Target.Name'] <- 'Target'
  dr.df$Omit <- FALSE

}else if (InputType=="NA") {
  ct_results.df = read.csv(text = gsub(",+$", "", readLines(input_file)), header = T,na.strings = c("-","NA","NaN","Undetermined"," "))
  #ct_results.df$Ct <- round(ct_results.df$Ct, 1)
  experiment_plate_setup.df = read.csv(experiment_plate_setup, header = T, skip = 1)
  make.names(names(experiment_plate_setup.df))
  experiment_plate_setup.df$Quantity[which(experiment_plate_setup.df$Task!="STANDARD" & experiment_plate_setup.df$Quantity==0)] <- NA
  experiment_plate_setup.df$Well <- NULL
  dr.df = merge(x=experiment_plate_setup.df, y=ct_results.df[,c("Well","Ct")], by.x="Well.Position", by.y="Well", sort=F)
  colnames(dr.df)[colnames(dr.df) == 'Sample.Name'] <- 'Sample'
  colnames(dr.df)[colnames(dr.df) == 'Well.Position'] <- 'Well'
  colnames(dr.df)[colnames(dr.df) == 'Target.Name'] <- 'Target'
  dr.df$Omit <- FALSE

}else if (InputType %in% c("row_wise","column_wise","row_column")){
#  ct_results.df <- parse3rdPartyData(input_file, InputType)
  ct_results.df <- build_ct_results(input_file)
  #ct_results.df$Ct <- round(ct_results.df$Ct, 1)
  write.csv(ct_results.df, file = 'ct-results.csv', row.names=F)
  # add a copy to the input dir, as ct-results.csv is read from here.
  write.csv(ct_results.df, file = paste(dirname(input_file), 'ct-results.csv', sep = '/'), row.names=F)
  experiment_plate_setup.df = read.csv(experiment_plate_setup, header = T, skip = 1)
  make.names(names(experiment_plate_setup.df))
  experiment_plate_setup.df$Quantity[which(experiment_plate_setup.df$Quantity==0)] <- NA
  experiment_plate_setup.df$Well <- NULL
  dr.df = merge(x=experiment_plate_setup.df, y=ct_results.df, by.x="Well.Position", by.y="Well", sort=F)
  colnames(dr.df)[colnames(dr.df) == 'Sample.Name'] <- 'Sample'
  colnames(dr.df)[colnames(dr.df) == 'Well.Position'] <- 'Well'
  colnames(dr.df)[colnames(dr.df) == 'Target.Name'] <- 'Target'
  dr.df$Omit <- FALSE
}


print("dr.df: ")
print(dr.df)
#*

head(dr.df)
dim(dr.df)
# masn. insert end. --------------------------------------------------------


#### npars = 5 is the default but can also run the 4-PL by setting npars = 4
#* passing all arguments to the function like npars, outlier_method, outlier.sig.level, omitted_wells #*

ll.test <- log.logistic.lma(data = dr.df, npars, use.log = TRUE, outlier.method, outlier.sig.level, omittedStdWells, omittedUnkWells)

results <- list(Number.of.parameters = ll.test$numPar, parameter.estimates = ll.test$estPar, yObs = ll.test$yObs, yFit = ll.test$yFit,
                yObs.mean = ll.test$yObs.mean, yFit.mean = ll.test$yFit.mean, xExt = ll.test$x.ext, yExt = ll.test$y.ext, inflection.point = ll.test$infl,
                regression.weights = ll.test$reg.Weights, model.standard.dev = ll.test$resid.stdev,  model.standard.error = ll.test$resid.stdErr,
                Pseudo.R.squared = ll.test$goodness, median.CV.standards = ll.test$median.CV.standards, linear.range.log10 = ll.test$linear.range.log10,
                bendL.conc.linear = ll.test$bendL.conc.linear, bendH.conc.linear = ll.test$bendH.conc.linear, LOD.conc = ll.test$LOD.conc,
                outlier.df = ll.test$outlier.df, outlier.Flag = ll.test$outlier.Flag,
                gradient = ll.test$gradient, hessian = ll.test$hessian, convergence.code = ll.test$code, iterations = ll.test$iterations,
                standard.exp.conc = ll.test$standard.exp.conc, standard.meas.conc = ll.test$standard.meas.conc, max.Ct = ll.test$max.Ct,
                results.df = ll.test$results.df)


# prepare data frame for group comparison; consider rows with Biogroup.Name values in c("UNKNOWN","UNKNOWN-2","UNKNOWN-3") and userOmit!=True
sample.conc.df <- results$results.df[(results$results.df$Biogroup.Name %in% c("UNKNOWN","UNKNOWN-2","UNKNOWN-3") & results$results.df$userOmit!=TRUE),
                                     c("Well", "Sample", "Biogroup.Name", "Target", "measured.concentration","serum.conc")]
head(sample.conc.df)
dim(sample.conc.df)


if (nrow(sample.conc.df) == 0) {
  sample.conc.df.agg = sample.conc.df
  sample.conc.df.agg.2 = sample.conc.df.agg
} else {
  sample.conc.df.agg <-
    aggregate(
      sample.conc.df[, c("measured.concentration", "serum.conc")],
      by = list(sample.conc.df[, "Sample"]),
      mean,
      na.rm = T,
      simplify = TRUE
    )

  colnames(sample.conc.df.agg)[1] <- "Sample"

  sample.conc.df.agg.2 <-
    merge(unique(sample.conc.df[, c("Sample", "Biogroup.Name", "Target")]), sample.conc.df.agg)
}

head(sample.conc.df.agg.2)
dim(sample.conc.df.agg.2)


if(dim(sample.conc.df.agg.2)[1]>0){
  rownames(sample.conc.df.agg.2) <- c(1:dim(sample.conc.df.agg.2)[1])
  groupComparisonResults <- GroupComparison(sample.conc.df.agg.2)
  mstat.results_Pairs = c("mstat.results_grps12","mstat.results_grps23","mstat.results_grps13")
  pValuePairs = c(NA,NA,NA)

  for(i in (1:length(groupComparisonResults))){
    #print(i)
    if("P-Value" %in% rownames(groupComparisonResults[[i]])){
      pValuePairs[i] <- as.numeric(groupComparisonResults[[i]]["P-Value",])
    }
  }
  pValueGrp12 <- pValuePairs[1]
  pValueGrp23 <- pValuePairs[2]
  pValueGrp13 <- pValuePairs[3]
} else{
  pValueGrp12 <- NA
  pValueGrp23 <- NA
  pValueGrp13 <- NA
}


####
#### extract results. only relevant ones.
####
results.conc <- mapply(list,
                       well = results$results.df$Well,
		       ct = results$results.df$Ct,
                       dct = results$results.df$delta.Ct,
                       conc = results$results.df$measured.concentration,
                       serumConc = results$results.df$serum.conc,
                       quant = results$results.df$Quantity,
                       #* adding target and task info
                       t = results$results.df$Target,
                       tk = results$results.df$Task,
                       #*
                       recov = results$results.df$standards.recovery,
                       cv = results$results.df$Coeff.Var,
                       outlier = results$results.df$outlier.Flag,
                       #* adding userOmit flag -> True/False
                       userOmit = results$results.df$userOmit,
                       #*
                       SIMPLIFY = F)



avgPlotData.df <- average.plot.data(results$results.df)
avgPlotData <- mapply(list,
                      id = as.factor(avgPlotData.df$ID),
                      x_mean = avgPlotData.df$x.mean,
                      conc = avgPlotData.df$conc,
                      y_mean = avgPlotData.df$delta.Ct.mean,
                      y_sd = avgPlotData.df$delta.Ct.sd,
                      well = avgPlotData.df$Well,
                      t = avgPlotData.df$Target,
                      tk = avgPlotData.df$Task,
                      cv = avgPlotData.df$cv,
                      recov = avgPlotData.df$recov,
                      serumConc = avgPlotData.df$serum.conc,
                      SIMPLIFY = F)

#*


# output to files
#library(rjson)
library(jsonlite)

#* We are writing more data to output file i.e. conc-results.json file later, so commented this line
#cat(toJSON(results.conc, auto_unbox = T, pretty = TRUE), file = output_file)
#*


####
#### Extract 5pl parameters
####
cat(toJSON(list(
  bottom = results$parameter$bottom,
  top = results$parameter$top,
  ec50 = results$parameter$ec50,
  slope = results$parameter$slope,
  asym = results$parameter$asym,
  linearRangeL = results$bendL.conc.linear,
  linearRangeH = results$bendH.conc.linear,
  lod = results$LOD.conc,
  linearRangeLog10 = results$linear.range.log10,
  noOfParams=npars,
  outlierMethod = outlier.method,
  outlierParams = outlier.sig.level,
  pValueGrp12 = pValueGrp12,
  pValueGrp23 = pValueGrp23,
  pValueGrp13 = pValueGrp13),
  digits = NA,
  auto_unbox = T,
  pretty = TRUE),
  file = "5pl-params.json")


#* storing 500 5pl curve points to draw 5pl curve on UI side
####
#### Extract 5pl standard curve 200 points
####
stdCurveData <- mapply(list,
                       x = 2^(results$xExt), # writing x points in linear scale
                       y = results$yExt,
                       SIMPLIFY = F)
#*


#* writing all results to conc-results.json file
cat(toJSON(list(plateType=plateType, stdCurve=stdCurveData, avgPlotData=avgPlotData, replicatesPlotdata=results.conc), digits = NA, auto_unbox = T, pretty = TRUE), file ="conc-results.json")
#*


#if (debug) {

  results.setup <- mapply(list,
                          well = dr.df$Well,
                          sample = dr.df$Sample,
                          task = dr.df$Task,
                          quant  = dr.df$Quantity,
                          SIMPLIFY = F)

  results.raw <- data.frame(
    well = as.character(results$results.df$Well),
    dct = results$results.df$delta.Ct,
    conc = results$results.df$measured.concentration,
    quant = dr.df$Quantity,
    recov = results$results.df$standards.recovery,
    cv = results$results.df$Coeff.Var,
    outlier = results$results.df$outlier.Flag)

#  cat(toJSON(results.setup), file = 'plate-setup.json')
  write.csv(results.raw, file = 'conc-results-raw.csv', row.names=F)

  # echo Ct.max to console
  ll.test$max.Ct
#}
# masn. insert end. --------------------------------------------------------

# export conc in Prism format
inDir = dirname(deprendo_5pl_script)
source(paste(inDir, 'prism-export.R', sep = '/'))
# exportToPrism(output_file, paste(shome, 'conc-results.tml', sep = '/'))
exportToPrism(inDir, output_file)
