# testing GEE api in R 
# attempt to put everything in R instead of having JN too 
# main reason: hit a roadblock when exporting GEE images from JN and reading them in to R --> for some reason the values are wrong 

# following tutorial: https://www.css.cornell.edu/faculty/dgr2/_static/files/R_html/ex_rgee.html


####################################################################
############################# set-up ###############################
####################################################################

# NOTE: installations only have to be done ONCE in the computer (not every time restart computer)

# install reticulate package if not already installed then load package 
if(!('reticulate') %in% installed.packages()){install.packages('reticulate')}
#library(reticulate)

# ONLY ONCE: specify which python version to use  
use_python(Sys.which('python3'))

# install package containing functions to use GEE
if(!('rgee') %in% installed.packages()){install.packages('rgee')}
#library(rgee)

# ONLY ONCE: install required Python pacakges for GEE (possibly in a new python environment? although can't find it)
rgee::ee_install()
rgee::ee_install_set_pyenv(py_path = "/usr/bin/python3", py_env="rgee")

# import any additional Python packages --> might have to move this to next section
reticulate::import('pandas')


####################################################################
####################### installation ###############################
####################################################################

# NOTE: every time that open the session

# load libraries 
library(reticulate)
library(rgee)

ee_check()
ee_Initialize()

