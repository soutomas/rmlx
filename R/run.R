#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Filename : run.R
# Use      : Convenient Functions for Monolix Runs 
# Author   : Tomas Sou (souto1)
# Created  : 2025-10-30
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Notes 
# - na
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Updates
# - na 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Global variables 
utils::globalVariables(c(
))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Run Monolix model file using command line 
#'
#' @param mlx_tran `<chr>` File name of path of the Monolix file.
#' @param mlx_dir `<chr>` Location of the Monolix file.
#' @param wait `<lgl>` `TRUE` to wait for the run to finish. 
#' @returns Job ID of the run on the cluster. 
#' @export
#' @examples
#' \dontrun{
#' run_mlx("r01_model.mlxtran")
#' }
run_mlx = function(mlx_tran,mlx_dir,wait=F){
  # Options 
  mlx_opt = "; mlxbsub -V 2023 -N 4 -n 12 -p "
  cmd = paste0("module purge; cd ",mlx_dir,mlx_opt,mlx_tran)
  out = system(cmd, intern=T)
  tstart = Sys.time()
  jobID = trimws(sub("bjobs", "", out[6]))
  print(out)
  # Wait 
  if(wait){
    # Wait until finish - max 4320 min (72 hr)
    jobID_done = paste0("ended(",jobID,")")
    cmd = paste0('bwait -t 4320 -w "',paste(jobID_done, collapse="&&"),'" ')
    print(cmd)
    system(cmd, intern = TRUE)
    tend = Sys.time()
    trun = difftime(tend, tstart, units="secs") |> as.double() |> round(2)
    cat("Job done! [sec]:", trun)  
  }
  # Return 
  return(jobID)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Run covariate model building using Monolix v2023
#'
#' @param mlx_tran `<chr>` File name of path of the Monolix file.
#' @param mlx_dir `<chr>` Location of the Monolix file.
#' @param mlx_tool `<chr>` Tool for model building.
#' @returns Job ID of the run on the cluster. 
#' @export
#' @examples
#' \dontrun{
#' run_cov2023("r01_model.mlxtran","cossac",".")
#' }
run_cov2023 = function(mlx_dir,mlx_tran,mlx_tool){
  # COSSAC/covSAMBA using Monolix v2023
  if(mlx_tool=="cossac") mlx_user = paste0(' -U "-t modelBuilding -s cossac')  
  if(mlx_tool=="covSamba") mlx_user = paste0(' -U "-t modelBuilding -s covSamba')  
  mlx_opt = '; mlxbsub -V 2023 -N 1 -n 16 -W 7200 -p '
  cmd = paste0("module purge; cd ",mlx_dir,mlx_opt,mlx_tran,mlx_user)
  out = system(cmd, intern=FALSE)
  tstart = Sys.time()
  jobID = trimws(sub("bjobs", "", out[6]))
  print(out)
  return(jobID)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Run covariate model building using Monolix v2024
#'
#' @param mlx_tran `<chr>` File name of path of the Monolix file.
#' @param mlx_dir `<chr>` Location of the Monolix file.
#' @param mlx_tool `<chr>` Tool for model building with reference to the config file.
#' @returns Job ID of the run on the cluster. 
#' @export
#' @examples
#' \dontrun{
#' mlx_tran = "r01_model.mlxtran"
#' mlx_tool = paste0(' -U "-t modelBuilding --config r20_config_cossac.txt"')
#' run_cov2024(mlx_tran,mlx_tool,".")
#' }
run_cov2024 = function(mlx_dir,mlx_tran,mlx_tool){
  # COSSAC/covSAMBA using Monolix v2024 with config file 
  mlx_opt = '; mlxbsub -V 2024 -N 1 -n 16 -W 7200 -p '
  cmd = paste0("module purge; cd ",mlx_dir,mlx_opt,mlx_tran,mlx_tool)
  out = system(cmd, intern=FALSE)
  tstart = Sys.time()
  jobID = trimws(sub("bjobs", "", out[6]))
  print(out)
  return(jobID)
}
