

## Generalized Synthetic Control Method
## (Causal Inferece with Interactive Fixed-effect Models)

## Author: Yiqing Xu (yiqingxu@ucsd.edu)
## Data: Jan 17, 2018 

## Install from CRAN
install.packages("panelView")
install.packages("gsynth", type = "source")
install.packages("haven")
install.packages("readstata13")


## ## Install from Github (development version)
install.packages('devtools', repos = 'http://cran.us.r-project.org') # if not already installed
devtools::install_github('xuyiqing/gsynth')


###################################################
##  First Example (Simulated Data)
###################################################


library(gsynth)
library(panelView)
library(haven)
library(readstata13)

## load data
rm(list = ls())
setwd("/Users/Qiong/Box Sync/Student Writing Group/no loan policy/working data")
data <- read.dta13("noloan_control_2.dta")

ls()
names(data)

panelView(par_q1_age20 ~ noloan_year, data = data,  index = c("super_opeid_col","ac_year")) 
panelView(par_q1_age20 ~ noloan_year, data = data,  index = c("super_opeid_col","ac_year"),type = "missing") 


system.time(
        out <- gsynth(par_q1_age20 ~ noloan_year+ftftfyug_race00_th, data = data,
                      index=c("super_opeid_col","ac_year"), inference="parametric",
                      se = TRUE, nboots = 500, r = c(0, 5), CV = TRUE,
                      force = "two-way", parallel = TRUE, cores = 4)
)


## print out estimates
print(out)

## or
out$est.att
out$est.avg
out$est.beta

## gap plot
plot(out) # by default

plot(out,type="gap", ylim=c(-3,12), main="")

## raw plot
plot(out,type="raw")

plot(out,type="raw", legendOff=TRUE, ylim=c(-10,40), main="")

## counterfactual plot
plot(out,type="counterfactual")

plot(out,type="counterfactual", raw = "band", xlab="Time",
     ylim=c(-5,35), legendOff=TRUE)

plot(out,type="counterfactual", raw = "none", main="")

plot(out,type="counterfactual", raw = "all")

## (individual plots)
plot(out,type="counterfactual", id = 102)

plot(out,type="counterfactual", id = 104, raw = "none", ylim = c(-10, 30))

plot(out,type="counterfactual", id = 105, raw = "all", legendOff = TRUE)


## factors
plot(out,type="factors", xlab="Time")

plot(out,type="factors", legendOff = TRUE, ylim = c(-4,4), main = "")

## loadings
plot(out,type="loadings")

## EM Method
system.time(
        out <- gsynth(Y ~ D + X1 + X2, data = simdata, EM = TRUE,
                      index=c("id","time"), inference="parametric",
                      se = TRUE, nboots = 500, r = c(0, 5), CV = TRUE,
                      force = "two-way", parallel = TRUE, cores = 4)
)

plot(out)

################################################################
##  Second Example (Election-Day Registration on Voter Turnout)
################################################################

data(gsynth)
names(turnout)

## data structure
panelView(turnout ~ policy_edr, data = turnout,  index = c("abb","year")) 

## estimation
out <- gsynth(turnout ~ policy_edr + policy_mail_in + policy_motor,
              data = turnout,  index=c("abb","year"), se = TRUE,
              r = c(0, 5), CV = TRUE, force = "two-way", nboots=500,
              seed = 02139)


## gap plot
plot(out,type="gap")

plot(out,type="gap", xlim = c(-10, 5), ylim=c(-3,10))

plot(out,type="gap", id = "WI", main = "Wisconsin")


## raw plot
plot(out,type="raw",xlab="Year",ylab="Turnout")

## counterfactual plot
plot(out,type="counterfactual")

plot(out,type="counterfactual", raw = "all")

## (individual plots)
plot(out,type="counterfactual", id = "CT")

plot(out,type="counterfactual", id = "WY", raw = "all", legendOff = TRUE)

plot(out,type="counterfactual", id = "WI", raw = "none")


## factors
plot(out,type="factors",xlab="Year")

## loadings
plot(out,type="loadings")


##### Unbalanced Panel ######

set.seed(123456)
turnout.ub <- turnout[-c(sample(1:dim(turnout)[1],50,replace=FALSE)),]

## quick missing plot
panelView(turnout ~ policy_edr + policy_mail_in + policy_motor, data = turnout.ub,  index = c("abb","year")) 



out <- gsynth(turnout ~ policy_edr + policy_mail_in + policy_motor, parallel = TRUE,
              data = turnout.ub,  index = c("abb","year"), se = TRUE, inference = "parametric",
              r = c(0, 5), CV = TRUE, force = "two-way", nboots = 1000, seed = 02139)

plot(out, type = "missing", xlab = "Year", ylab = "State", main = "Treatment Status")

plot(out, type = "missing", xlab = "Year", ylab = "State", main = "Treatment Status",
     id = out$id.tr, xlim = c(1920,2010),  axis.adjust=TRUE)

plot(out, type = "gap")

