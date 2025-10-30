#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Filename : run.R
# Use      : Convenient Functions for Monolix Runs 
# Author   : Tomas Sou (souto1)
# Created  : 2025-10-30
# Updated  : 2025-10-30
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
#' run_mlx_cmd("r01_model.mlxtran")
#' }
run_mlx_cmd = function(mlx_tran,mlx_dir=NULL,wait=TRUE){
  mlx_opt = "; mlxbsub -V 2023 -N 2 -n 12 -p "
  cmd = paste0("module purge; cd ",mlx_dir,mlx_opt,mlx_tran)
  out = system(cmd, intern=T)
  tstart = Sys.time()
  jobID = trimws(sub("bjobs", "", out[6]))
  print(out)
  if(wait){
    # Wait until finish - max 72 hr / 4320 min 
    jobID_done = paste0("ended(",jobID,")")
    cmd = paste0('bwait -t 4320 -w "',paste(jobID_done, collapse="&&"),'" ')
    print(cmd)
    system(cmd, intern = TRUE)
    tend = Sys.time()
    trun = difftime(tend, tstart, units="secs") |> as.double() |> round(2)
    cat("Job done! [sec]:", trun)  
  }
  return(jobID)
}
