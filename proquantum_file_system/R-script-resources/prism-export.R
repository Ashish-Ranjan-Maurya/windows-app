library(plyr)

# consts and globals
# expected group IDs. this is internal reprensentation of the groups. const
UNKNOWN_GROUPS <- c('UNKNOWN', 'UNKNOWN-2', 'UNKNOWN-3')
# display name for the groups. must match the internal rep (UNKNOWN_GROUPS). const
UNKNOWN_GROUP_NAMES <- c('Group 1', 'Group 2', 'Group 3')
# template for unknown groups YColmn construct
GROUP_TML =
  '<YColumn Width="204" Decimals="9" Subcolumns="$UNKNOWN-REP$">
<Title>$GROUP-NAME$</Title>
$UNKNOWN-CONC$
</YColumn>'
# number of dummy rows for the next group. 0 for the 1st group. global.
dummy_rows <- 0

read.text = function(pathname)
{
  return (paste(readLines(pathname), collapse = "\n"))
}

# eg: <Subcolumn><d>1.0</d><d>2.0</d></Subcolumn> for vector x=1.0, 2.0
markupSubcolumn <- function(x) {
  return(paste (
    '<Subcolumn>',
    paste('\n<d>', x, '</d>', sep = '', collapse = ''),
    '\n</Subcolumn>',
    sep = ''
  ))
}


# eg: <Subcolumn><d>1.0</d><d>2.0</d></Subcolumn> for vector x=1.0, 2.0
markupSubd <- function(x) {
  return(
    paste('\n<d>', x, '</d>', sep = '', collapse = ''))
}

# maps group id (UNKNOWN_GROUPS) to display name
to_group_name <- function(id) {
  name = if (is.na(id)) {
    'Group'
  } else if (id == UNKNOWN_GROUPS[1]) {
    UNKNOWN_GROUP_NAMES[1]
  } else if (id == UNKNOWN_GROUPS[2]) {
    UNKNOWN_GROUP_NAMES[2]
  } else if (id == UNKNOWN_GROUPS[3]) {
    UNKNOWN_GROUP_NAMES[3]
  }
  else {
    'Group'
  }
  
  return (name)
  
}

# build a <YColumn>. one for each group of unknowns
build_ycol <- function(conc_xml, Group) {
  # combine
  conc = paste(conc_xml, sep = '', collapse = '')
  
  # add dummy rows for unknown groups: UNKNOWN_GROUPS
  # all values in the Group are the same. use the 1st one.
  if (Group[1] %in% UNKNOWN_GROUPS) {
    dummy_fill = paste(rep('<d></d>', dummy_rows),
                       sep = '',
                       collapse = '\n')
    conc = gsub('<Subcolumn>',
                paste('<Subcolumn >', dummy_fill, sep = '\n'),
                conc,
                fixed = T)
    
    # update dummy rows count. for use in the next one.
    # conc_xml contains rows of data in xml. the markup is <d>.
    # number of rows is the number of splits by <d> minus 1.
    dummy_rows <<-
      dummy_rows + (length(strsplit(conc_xml[1], '<d>', fixed = T)[[1]]) - 1)
  }
  
  group_tml = gsub('$UNKNOWN-REP$', length(conc_xml), GROUP_TML, fixed = T)
  group_tml = gsub('$GROUP-NAME$', to_group_name (Group[1]), group_tml, fixed = T)
  group_tml = gsub('$UNKNOWN-CONC$', conc, group_tml, fixed = T)
  
  return (group_tml)
  
}

buildPrismReplicateXml <- function(model.df) {
  rtn <-
    ddply(
      model.df,
      c('Task', 'Group', 'ssid'),
      summarise,
      Ct_xml = markupSubcolumn(Ct),

      # 9/1
      # Sample_xml = markupSubcolumn(Sample),
      Sample_xml = markupSubd(sid),
      conc_xml = markupSubcolumn(conc)
    )
  
  # grouping
  rtn <-
    ddply(
      rtn,
      c('Task', 'Group'),
      summarise,
      Ct_xml1 = paste(Ct_xml, sep = '', collapse = ''),
      # 9/1
      # Sample_xml1 = paste(Sample_xml, sep = '', collapse = ''),
      Sample_xml1 = unique(Sample_xml),
      conc_xml1 = build_ycol(conc_xml, Group)
    )
  
  rtn <-
    ddply(
      rtn,
      c('Task'),
      summarise,
      Ct_xml2 = paste(Ct_xml1, sep = '', collapse = ''),
      # 9/1
      # Sample_xml2 = paste(Sample_xml1, sep = '', collapse = ''),
      Sample_xml2 = paste("<Subcolumn>", paste(Sample_xml1, sep = '', collapse = ''), "\n</Subcolumn>", sep = '', collapse = ''),
      conc_xml2 = paste(conc_xml1, sep = '', collapse = '')
    )
  
  return(rtn)
}

buildPrismReplicateData <- function(model.df) {
  ct_results = buildPrismReplicateXml(model.df)
  
  rtn <- list(
    ntc = (ct_results[ct_results$Task == 'NTC', 'Ct_xml2']),
    standard = (ct_results[ct_results$Task == 'STANDARD', 'Ct_xml2']),
    unknown = (ct_results[ct_results$Task == 'UNKNOWN', 'Ct_xml2']),
    unknown.name = (ct_results[ct_results$Task == 'UNKNOWN', 'Sample_xml2']),
    standard.name = (ct_results[ct_results$Task == 'STANDARD', 'Sample_xml2']),
    conc = (ct_results[ct_results$Task == 'UNKNOWN', 'conc_xml2'])
  )
  
  return (rtn)
}

buildPrismNames <-  function(model.df) {
  sample <- subset(model.df, !duplicated(sid))
  
  # 8/31
  # sort on groups
  sample <-
    sample[order(sample$Group, sample$Task, sample$wid), ]
  
  sample <-
    ddply(sample, 'Task', summarise , name = markupSubcolumn(sid))
  
  return (list(std =
                 sample[sample$Task == 'STANDARD', 'name'], unk = sample[sample$Task == 'UNKNOWN', 'name']))
}

# build a complete model of domain
# currently, it has  the following columns
# Well	Sample	Task	Ct	Quantity	Group	wid	conc	sid	ssid
# A1		STANDARD	18.12802315	5000	STANDARD	1	6356.319474	5000	1
# A2		STANDARD	18.36406898	5000	STANDARD	2	5237.598867	5000	2
# note
# wid - well index. 1 based;
# sid - sample id. it's either the quant (standard) or sample name (unknown & ntc)
# ssid - replicate id in a sample group. serial #
buildModel <-
  function(pathToInputCt.csv,
           pathToPlateSetup.csv,
           pathToConc.csv) {
    # extract data
    # pathToInputCt.csv= 'ct-results.csv'
    ct.df <-
      read.csv(pathToInputCt.csv,
               header = T,
               stringsAsFactors  = FALSE)[, c("Well", "Ct")]
    
    # extract data
    # pathToPlateSetup.csv <- "plate-setup.csv"
    setup.df <-
      read.csv(
        pathToPlateSetup.csv,
        header = T,
        skip = 1,
        stringsAsFactors = FALSE
      ) [, c('Well',
             'Well.Position',
             'Sample.Name',
             'Task',
             'Quantity',
             'Biogroup.Name')]
    
    setup.df$Quantity[which(is.na(setup.df$Quantity))] <- ''
    colnames(setup.df) <-
      c('wid', 'Well', 'Sample', 'Task', 'Quantity', 'Group')
    
    # pathToConc.csv = "conc-results-raw.csv"
    conc.df <-
      read.csv(pathToConc.csv,
               header = TRUE,
               stringsAsFactors = FALSE)
    
    # working copy. contains the complete data model for the operations.
    # working.df = ct.df  %>%
    #   inner_join(setup.df, by = "Well") %>%
    #   inner_join(conc.df, by = c("Well" = "well"))
    working.df <-  merge(ct.df, setup.df, by = "Well")
    working.df <-
      merge(working.df, conc.df, by.x = "Well", by.y = "well")
    
    # separate additional manipulation for clarity
    working.df = working.df[, c('Well',
                                'Sample',
                                'Task',
                                'Ct',
                                'Quantity',
                                'Group',
                                'wid',
                                'conc')]

    working.df <-
      working.df[order(working.df$Task, working.df$Sample), ]
    
    # construct the sample id: quant for std; sample name for others
    working.df$sid <-
      ifelse(working.df$Task == 'STANDARD',
             working.df$Quantity,
             working.df$Sample)
    
    # ssid used to separate replicates in a rep-group
    working.df <-
      ddply(working.df, c('Task', 'sid'), mutate, ssid = seq_along(sid))
    
    # working.df <-  working.df[order(working.df$wid), ]
    # working.df <-  working.df[order(as.numeric(working.df$Quantity), working.df$wid, decreasing = c(TRUE, FALSE)), ]
    working.df <-
      working.df[order(as.numeric(working.df$Quantity), decreasing = TRUE), ]
    
    return (working.df)
  }

getReplicateCounts <- function(model.df) {
  tmp <-
    ddply(model.df, c('Task', 'sid'), summarise, nsid = length(Well))
  replicates <- ddply(tmp, c('Task'), summarise, ntask = max(nsid))
  
  
  counts = list(std = replicates[replicates$Task == 'STANDARD', 'ntask'], unk = replicates[replicates$Task == 'UNKNOWN', 'ntask'])
  
  return(counts)
}

templating <- function(pathToPrismTemplateFile,
                       # snames ,
                       ctInfo ,
                       repcounts) {
  # get the template contents
  rtn = read.text(pathToPrismTemplateFile)
  
  # update the template with data
  # 9/1
  # rtn = gsub('$STANDARD-CONC$', snames$std, rtn, fixed = T)
  rtn = gsub('$STANDARD-CONC$', ctInfo$standard.name, rtn, fixed = T)
  rtn = gsub('$STANDARD-CT$', ctInfo$standard, rtn, fixed = T)
  rtn = gsub('$STANDARD-REP$', repcounts$std, rtn, fixed = T)
  # 9/1
  # if (length(snames$unk) > 0) {
  if (length(ctInfo$unknown.name) > 0) {
    # 9/1
    # rtn = gsub('$UNKNOWN-NAME$', snames$unk, rtn, fixed = T)
    rtn = gsub('$UNKNOWN-NAME$', ctInfo$unknown.name, rtn, fixed = T)
    rtn = gsub('$UNKNOWN-REP$', repcounts$unk, rtn, fixed = T)
    rtn = gsub('$UNKNOWN-GROUPS$', ctInfo$conc, rtn, fixed = T)
  }
  
  return(rtn)
}

# keys used for content substitutions
# $STANDARD-CONC$ - known concentration for the standards
# $STANDARD-CT$   - measured Ct for the standards
# $STANDARD-REP$ - replicate numbers for standards
# $UNKNOWN-NAME$  - names/labels for the unknown samples
# $UNKNOWN-CONC$  - calculated concentration for the unknown samples
# $UNKNOWN-REP$ - replicate numbers for unknown samples
# $UNKNOWN-GROUPS$ - whole section of concentrations (conc) for the unknown samples arranged in groups.
genPrismFile <-
  function(pathToInputCt.csv,
           pathToPlateSetup.csv,
           pathToConc.csv,
           pathToOutputPrismFile,
           pathToPrismTemplateFile) {
    # build a global model
    model.df = buildModel(pathToInputCt.csv,
                          pathToPlateSetup.csv,
                          pathToConc.csv)
    
    # debugging state. working model goes to the output dir - same dir as pathToOutputPrismFile
    # write.csv(
    #   model.df,
    #   row.names = FALSE,
    #   file = paste(dirname(pathToOutputPrismFile), '/working.df.csv', sep = '')
    # )
    #              file = 'C:/3-projects/deprendo/dev4/Deprendo-App/deprendo-svc/src/main/rscript/prism/working.df.csv')
    #            file = 'C:/3-projects/deprendo/tasks/task-prism-export/out-il5-linked/working.df.csv')
    
    # 9/1
    # snames = buildPrismNames(model.df)
    
    ctInfo = buildPrismReplicateData(model.df)
    
    repcounts = getReplicateCounts(model.df)
    
    # replace key in the template
    rtn = templating(pathToPrismTemplateFile,
                     # snames ,
                     ctInfo ,
                     repcounts)
    
    # export results
    write(rtn, pathToOutputPrismFile)
  }

exportToPrism <-
  function(inputDir, outputDir) {
    genPrismFile(
      paste(inputDir, 'ct-results.csv', sep = '/'),
      paste(inputDir, 'plate-setup.csv', sep = '/'),
      paste(outputDir, 'conc-results-raw.csv', sep = '/'),
      paste(outputDir, 'conc-results.pzfx', sep = '/'),
      paste(inputDir, 'conc-results.tml', sep = '/')
    )
    
    print('Done.')
  }

###### main entry here ######
# dios = 'C:/3-projects/deprendo/tasks/task-prism-export/out-il5-linked'
# dios = "C:/3-projects/deprendo/bugs/prism export with 3rd-party case"
# dios = "C:/3-projects/deprendo/bugs/prism export with 3rd-party case/14a517f7-1334-4f91-8b59-fced8566fb78/14a517f7-1334-4f91-8b59-fced8566fb78"
# dios = "C:/3-projects/deprendo/bugs/thao-0810-384/out-updated"
# dios <- "C:/3-projects/deprendo/bugs/david-0821/out/rstudio"
# dios <- "C:/3-projects/deprendo/bugs/david-0821/out/rstudio-m1"
# dios <- "C:/3-projects/deprendo/bugs/daa-1084-prism-export"
# dios <- "C:/3-projects/deprendo/bugs/daa-1084-prism-export/with-0901-config"
# 
# exportToPrism(dios, dios)
# 'C:/3-projects/deprendo/tasks/task-prism-export/out-il5-linked', 'C:/3-projects/deprendo/tasks/task-prism-export/out-il5-linked')
# need to copy the template file (.tml) into the input folder
# exportToPrism(
#   'C:/3-projects/deprendo/tasks/task-prism-export/043511cc-778e-426a-8d61-494df16e87da-working',
#   'C:/3-projects/deprendo/tasks/task-prism-export/043511cc-778e-426a-8d61-494df16e87da-working'
# )


# usage example. uncomment to enable. update file paths before run.
# genPrismFile('c:/tmp/prism/ct-results.csv', 'c:/tmp/prism/plate-setup.csv', 'c:/tmp/prism/output.pzfx', 'c:/tmp/prism/template.pzfx')
# genPrismFile(
#   # 'C:/3-projects/deprendo/dev4/Deprendo-App/deprendo-svc/src/main/rscript/prism/ct-results-2s.csv',
#   # 'C:/3-projects/deprendo/dev4/Deprendo-App/deprendo-svc/src/main/rscript/prism/plate-setup.csv',
#   # 'C:/3-projects/deprendo/dev4/Deprendo-App/deprendo-svc/src/main/rscript/prism/conc-results-raw.csv',
#   # 'C:/3-projects/deprendo/dev4/Deprendo-App/deprendo-svc/src/main/rscript/prism/conc-results.pzfx',
#   # 'C:/3-projects/deprendo/dev4/Deprendo-App/deprendo-svc/src/main/rscript/prism/conc-results.tml'
#   'C:/3-projects/deprendo/tasks/task-prism-export/out-il5-linked/ct-results.csv',
#   'C:/3-projects/deprendo/tasks/task-prism-export/out-il5-linked/plate-setup.csv',
#   'C:/3-projects/deprendo/tasks/task-prism-export/out-il5-linked/conc-results-raw.csv',
#   'C:/3-projects/deprendo/tasks/task-prism-export/out-il5-linked/conc-results.pzfx',
#   'C:/3-projects/deprendo/dev4/Deprendo-App/deprendo-svc/src/main/rscript/prism/conc-results.tml'
# )
