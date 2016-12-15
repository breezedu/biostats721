
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721| Topic 7 Part 2: Reproducible Research in R ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create Simulation Results --------------------------------------------------------

rm(list=ls())

# Name Working Directories: 
# - Point R to the directory where the simulation output 
#   files are stored 
# - You should change this to where you stored these files
# - Make sure to use \\ or / in the file path and not \
in_dir <- ''        # Set to location of .RData file from Topic 6 Part 3
out_dir <- ''       # Set to location where you want to save report doc

# Load the R Workspace with the simulation results
# - Should sit the fit objects listed in the workspace
#   after running the load() command
load(paste(in_dir,'sim_allns_fitobjects.RData',sep=''))

# Develop Code to Create Table ---------------------------------------------------

# A few notes about rounding in R ... 
# - For a numeric vector in R, the round() function will work as expected ... 
a <- c(1.2, 2, -0.46, 1.345)
round(a,2)
# - For a integer vector in R, the round() function will not ... 
b <- 1:5
round(b,2)
# - To get around this formatting issue for table generation, use the formatC() 
#   function which "forces" the format by converting the output to character mode
formatC(b,digits=2,format='f')

# Recall the structure of the stored simulation output: 
names(fit.n50)
fit.n50$bias.t1

# - Step 1: Develop code to create single cell in the table shell 
# - Start with the n=50 and prop of outliers = 0 for the mean cell 
fit.object <- fit.n50
prop <- 0
which.prop <- which(seq(0,0.2,by=0.01)==prop)
cell <- paste(formatC(fit.object$bias.t1[which.prop],digits=3,format='f'),
              ' (',
              formatC(fit.object$mse.t1[which.prop],digits=3,format='f'),
              ')',sep='')
cell

# - Step 2: Develop code to create a single row in the table shell 
# - Start with the n=50 and prop of outliers = 0 row (i.e. all estimators)
fit.object <- fit.n50
prop <- 0
which.prop <- which(seq(0,0.2,by=0.01)==prop)
cell1 <- paste(formatC(fit.object$bias.t1[which.prop],digits=3,format='f'),
               ' (',
               formatC(fit.object$mse.t1[which.prop],digits=3,format='f'),
               ')',sep='')
cell2 <- paste(formatC(fit.object$bias.t2[which.prop],digits=3,format='f'),
               ' (',
               formatC(fit.object$mse.t2[which.prop],digits=3,format='f'),
               ')',sep='')
cell3 <- paste(formatC(fit.object$bias.t3[which.prop],digits=3,format='f'),
               ' (',
               formatC(fit.object$mse.t3[which.prop],digits=3,format='f'),
               ')',sep='')
row <- c(cell1,cell2,cell3)
row

# - Step 3: Functional-ize the code to create a single row and re-run for all 
#           rows in the table shell
make.row <- function(fit.object,prop){
  which.prop <- which(seq(0,0.2,by=0.01)==prop)
  cell1 <- paste(formatC(fit.object$bias.t1[which.prop],digits=3,format='f'),
                 ' (',
                 formatC(fit.object$mse.t1[which.prop],digits=3,format='f'),
                 ')',sep='')
  cell2 <- paste(formatC(fit.object$bias.t2[which.prop],digits=3,format='f'),
                 ' (',
                 formatC(fit.object$mse.t2[which.prop],digits=3,format='f'),
                 ')',sep='')
  cell3 <- paste(formatC(fit.object$bias.t3[which.prop],digits=3,format='f'),
                 ' (',
                 formatC(fit.object$mse.t3[which.prop],digits=3,format='f'),
                 ')',sep='')
  row <- c(cell1,cell2,cell3)
  return(list(row=row))}

make.row(fit.n50,0)$row         # Same row used in developement
make.row(fit.n50,0.05)$row      # For n=50 and prop=0.05 ... 
make.row(fit.n100,0.1)$row      # For n=100 and prop=0.10 ... 


# Exporting Table ----------------------------------------------------------------

# Option 1: By "hand"
# - Run make.row fuction, print results, and copy-and-paste into table shell 

make.row(fit.n50,0)$row
make.row(fit.n50,0.05)$row
make.row(fit.n50,0.10)$row

make.row(fit.n100,0)$row
make.row(fit.n100,0.05)$row
make.row(fit.n100,0.10)$row

make.row(fit.n150,0)$row
make.row(fit.n150,0.05)$row
make.row(fit.n150,0.10)$row

# Option 2: Create a data object that mirrors table shell and export to .csv file

# Step 1: Recreate the table shell row-by-row and stack row objectss to create 
#         a single data object that exactly mirrors the format of the table shell 

table.object <- NULL 

sampsize.n50 <- rep(NA,3)            # Section 1 of Table Shell 
table.object <- rbind(table.object,sampsize.n50)

prop.out.00 <- make.row(fit.n50,0)$row
table.object <- rbind(table.object,prop.out.00)

prop.out.05 <- make.row(fit.n50,0.05)$row
table.object <- rbind(table.object,prop.out.05)

prop.out.10 <- make.row(fit.n50,0.10)$row
table.object <- rbind(table.object,prop.out.10)

sampsize.n100 <- rep(NA,3)           # Section 2 of Table Shell 
table.object <- rbind(table.object,sampsize.n100)

prop.out.00 <- make.row(fit.n100,0)$row
table.object <- rbind(table.object,prop.out.00)

prop.out.05 <- make.row(fit.n100,0.05)$row
table.object <- rbind(table.object,prop.out.05)

prop.out.10 <- make.row(fit.n100,0.10)$row
table.object <- rbind(table.object,prop.out.10)

sampsize.n150 <- rep(NA,3)           # Section 3 of Table Shell 
table.object <- rbind(table.object,sampsize.n150)

prop.out.00 <- make.row(fit.n150,0)$row
table.object <- rbind(table.object,prop.out.00)

prop.out.05 <- make.row(fit.n150,0.05)$row
table.object <- rbind(table.object,prop.out.05)

prop.out.10 <- make.row(fit.n150,0.10)$row
table.object <- rbind(table.object,prop.out.10)

colnames(table.object) <- c('Mean','20% Trimmed Mean','Median')
table.object 

# Step 2: Export table data object to a .csv file 
write.csv(table.object,
          paste(out_dir,'table2.csv',sep=''),
          quote=FALSE, na='')

# Option 3: Create a table and export directly to .docx file using RMarkdown 
# - Compile code in table_option3.Rmd

# --------------------------------------------------------------------------------
# End of Program 


