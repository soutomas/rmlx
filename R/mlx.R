#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Filename : mlx.R
# Use      : Convenient Functions for Processing of Monolix Results 
# Author   : Tomas Sou (souto1)
# Created  : 2025-10-16
# Updated  : 2025-10-29
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Notes 
# - na
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Updates
# - na 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Global variables 
utils::globalVariables(c(
  "Values","Value","value",
  "CV","PARA","OFV","AIC","BIC","BICc",
  "X2","X3","parameter"
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

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Search for Monolix model files 
#'
#' @param path `<chr>` Path to the directory of the model files. 
#' @param ifall `<lgl>` `TRUE` to return all model files in the directory. 
#' @return A character vector containing the names of the model files.
#' @export
#' @examples
#' \dontrun{
#' mlx_path = "CLOU064C1/mas/mas_1/model/pgm_001/Task_02_REMODEL_Sim_PopPK_plan/runs" 
#' get_mlx(mlx_path)
#' }
get_mlx = function(path=".",ifall=TRUE){
  if(!ifall) {
    mlxruns = select.list(dir(path=path,pattern=".mlxtran$",full.names=T), multiple=T) 
  } else {
    mlxruns = dir(path=path,pattern=".mlxtran$",full.names=T)
  }
  mlxruns = gsub(".mlxtran","",mlxruns)
  return(mlxruns)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Get population parameter file names of a Monolix model 
#'
#' @param mlxrun `<chr>` A Monolix file name. 
#' @return The file name of the population parameters of the model run. 
#' @export
#' @examples
#' \dontrun{
#' get_parafname("r01_model.mlxtran")
#' }
get_parafname = function(mlxrun){
  fdirname = gsub(".mlxtran","",mlxrun) 
  out = file.path(fdirname,"populationParameters.txt")
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Get summary file names of a Monolix model 
#'
#' @param mlxrun `<chr>` A Monolix file name. 
#' @return The file name of the summary file of the model run. 
#' @export
#' @examples
#' \dontrun{
#' get_summfname("r01_model.mlxtran")
#' }
get_summfname = function(mlxrun){
  fdirname = gsub(".mlxtran","",mlxrun) 
  out = file.path(fdirname,"summary.txt")
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Get summary file names of a Monolix model 
#'
#' @param mlxrun `<chr>` A Monolix file name. 
#' @return The file name of the objective function values of the model run. 
#' @export
#' @examples
#' \dontrun{
#' get_ofv("r01_model.mlxtran")
#' }
get_ofv = function(mlxrun){
  fdirname = gsub(".mlxtran","",mlxrun) 
  fname = get_summfname(mlxrun)
  if(!file.exists(fname)){
    ofvs = tibble::tibble(
      RUN = mlxrun,
      OFV = NA,
      AIC = NA,
      BIC = NA,
      BICc = NA,
    ) 
    return(ofvs)
  } 
  ofvs = readr::read_fwf(fname,readr::fwf_widths(c(41,14,9)),show_col_types=F) |>
    dplyr::filter(grepl("OFV|AIC|BICc|BIC",X2)) |> 
    dplyr::mutate(X3 = as.numeric(X3)) |> 
    # mutate(X3 = tibble::num(X3,digits=2)) |> 
    dplyr::mutate(across(where(is.double), ~round(.x, digits=2))) |> 
    dplyr::select(X2,X3) |> 
    tidyr::pivot_wider(names_from=X2,values_from=X3) |> 
    dplyr::rename(OFV=1,AIC=2,BICc=3,BIC=4) |>
    dplyr::mutate(RUN = basename(fdirname), .before=1) 
  return(ofvs)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Get parameter results of a Monolix model 
#'
#' @param mlxrun `<chr>` A Monolix file name. 
#' @return A data frame containing the parameter values of the model run.
#' @export
#' @examples
#' \dontrun{
#' get_para("r01_model.mlxtran")
#' }
get_para = function(mlxrun){
  fname = get_parafname(mlxrun) 
  if(!file.exists(fname)) return(paste0("No results for: ",basename(mlxrun),"\n"))
  para = readr::read_csv(fname) |> 
    dplyr::select(
      PARA = parameter, 
      Value = value, 
      CV, 
      RSEsa = dplyr::any_of("rse_sa"), 
      RSElin = dplyr::any_of("rse_lin")
    ) |> 
    # dplyr::mutate(CV = ifelse(grepl("^omega",PARA),Value*100,NA)) |> # omega on SD scale
    dplyr::mutate(across(where(is.double), ~signif(.x, digits=3))) |> 
    dplyr::mutate(across(everything(), ~replace(.x, is.nan(.x), "NaN")))    
  # dplyr::mutate(RUN = mlxrun, .before=PARA)
  return(para) 
} 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Get objective function values of all model runs 
#'
#' @param mlxruns `<chr>` A vector containing the model file names. 
#' @param sortAIC `<lgl>` `TRUE` to sort results by AIC values.
#' @returns A data frame containing the objective function values of the models. 
#' @export
#' @examples
#' \dontrun{
#' get_allofv("r01_model.mlxtran")
#'}
get_allofv = function(mlxruns, sortAIC=FALSE){
  allofv = purrr::map_df(mlxruns, get_ofv) |> 
    dplyr::mutate(dOFV=OFV-dplyr::lag(OFV), .after=OFV) |>
    dplyr::mutate(dAIC=AIC-dplyr::lag(AIC), .after=AIC) |>
    dplyr::mutate(dBICc=BICc-dplyr::lag(BICc), .after=BICc) |>
    dplyr::mutate(dBIC=BIC-dplyr::lag(BIC), .after=BIC) 
  if(sortAIC) allofv = allofv |> dplyr::arrange(AIC) 
  allofv = allofv |> dplyr::mutate(ROW = dplyr::row_number(), .before=1) 
  return(allofv)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Get parameters of all runs 
#'
#' @param mlxruns `<chr>` A vector containing the model file names. 
#' @returns All parameter values 
#' @examples
#' \dontrun{
#' get_allpara("r01_model.mlxtran")
#'}
get_allpara = function(mlxruns){
  allpara = purrr::map(mlxruns,get_para)
  names(allpara) = basename(mlxruns)
  return(allpara)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Compare parameters of two runs 
#'
#' @param addto `<dfr>` Output from previous run to add to if any. 
#' @param run1,run2 `<chr>` Partial model names to by matched by regular expression 
#' @param rse `<lgl>` `TRUE` to return RSE.
#' @param cv `<lgl>` `TRUE` to return CV.
#' @param mlxruns `<chr>` A vector containing the model file names. 
#' @returns A data frame containing all parameter values.
#' @export
#' @examples
#' \dontrun{
#' compare_para(mlxruns=mlxruns) 
#' }
compare_para = function(addto=NULL,run1=NA,run2=NA,rse=TRUE,cv=FALSE,mlxruns){
  run1n = grep(paste0("/",run1),mlxruns,value=TRUE) 
  run2n = grep(paste0("/",run2),mlxruns,value=TRUE) 
  runs = run1n 
  if(!is.na(run2)) runs = c(run1n,run2n) 
  if(!is.null(addto)) runs = c(runs,"+others") 
  # print(runs) # show full run names for debugging 
  valname = paste0(run1,".val")
  rsename = paste0(run1,".rse")
  rsename2 = paste0(run1,".rseLin")
  cvname = paste0(run1,".cv")
  para1 = get_para(run1n) 
  if(!is.data.frame(para1)) {cat(para1); return(addto)}
  if(is.data.frame(para1)) para1 = para1 |> 
    dplyr::rename(
      !!valname := Value,
      !!cvname := CV,
      !!rsename := dplyr::any_of("RSEsa"),
      !!rsename2 := dplyr::any_of("RSElin")
    )
  out = para1 
  if(!is.na(run2)) valname = paste0(run2,".val") 
  if(!is.na(run2)) rsename = paste0(run2,".rse")
  if(!is.na(run2)) rsename2 = paste0(run2,".rseLin")
  if(!is.na(run2)) cvname = paste0(run2,".cv")
  if(!is.na(run2)) para2 = get_para(run2n)
  if(!is.na(run2) && !is.data.frame(para2)) {cat(para2); return(out)}
  if(!is.na(run2) && is.data.frame(para2)) para2 = para2 |> 
    dplyr::rename(
      !!valname := Value,
      !!cvname := CV,
      !!rsename := dplyr::any_of("RSEsa"),
      !!rsename2 := dplyr::any_of("RSElin")
    )
  if(!is.na(run2)) out = dplyr::full_join(para1,para2,by="PARA") 
  if(!is.null(addto)) out = dplyr::full_join(addto,out,by="PARA")
  if(!rse) out = out |> dplyr::select(-ends_with("rse"))
  if(!cv) out = out |> dplyr::select(-ends_with("cv"))
  pop = out |> dplyr::filter(grepl("pop$",PARA),!grepl("^beta",PARA)) 
  omg = out |> dplyr::filter(grepl("^omega",PARA)) 
  cov = out |> dplyr::filter(grepl("^beta",PARA)) 
  corr = out |> dplyr::filter(grepl("^corr",PARA)) 
  res = out |> dplyr::filter(!grepl("pop$|^omega|^beta|^corr",PARA))
  out = dplyr::bind_rows(pop,cov,omg,corr,res)
  return(out) 
} 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Compare parameters of all runs 
#'
#' @param mlxruns `<chr>` A vector containing the model file names. 
#' @param rse `<lgl>` `TRUE` to return RSE.
#' @param cv `<lgl>` `TRUE` to return CV.
#' @returns A data frame containing the model parameters.
#' @export
#' @examples
#' \dontrun{
#' compare_allpara("r01_model.mlxtran") 
#' }
compare_allpara = function(mlxruns,rse=TRUE,cv=FALSE){
  # extract run number 
  # runs = substr(mlxruns,1,3) 
  runs = 
    stringr::str_extract(basename(mlxruns),"^.*?[_]") |> 
    stringr::str_sort(numeric=TRUE) |> 
    stringr::str_remove("_")
  out = compare_para(run1=runs[1],rse=rse,cv=cv,mlxruns=mlxruns) 
  if(length(runs)==1) return(out = out |> tibble::rowid_to_column("ROW"))
  out = compare_para(run1=runs[1],run2=runs[2],rse=rse,cv=cv,mlxruns=mlxruns)
  if(length(runs)==2) return(out = out |> tibble::rowid_to_column("ROW"))
  if(length(runs)>2){ 
    for(i in runs[-(1:2)]){
      out = compare_para(addto=out,run1=i,rse=rse,cv=cv,mlxruns=mlxruns) 
    }
    return(out = out |> tibble::rowid_to_column("ROW"))
  }
} 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' Examine objective function and parameter values of selected runs 
#'
#' @param runnums `<chr>` Partial model file names of the model files
#' @param path `<chr>` Path to model directory for [get_mlx]
#' @param ifOFV `<lgl>` `TRUE` to return objective function values 
#' @param ifParam `<lgl>` `TRUE` to return parameter values 
#'
#' @returns A list containing the OFV and parameter values of all models 
#' @export
#' @examples
#' \dontrun{
#' # Model files are matched by regular expression 
#' exam_runs("/r01|/r02") # model names include "/r01" and "/r02"
#' exam_runs("^r01|^r02") # model names starting with "r01" and "r02"
#' }
exam_runs = function(runnums,path=".",ifOFV=TRUE,ifParam=TRUE){
  # Get all runs in the directory 
  mlxruns = get_mlx(path) 
  if(length(mlxruns)==0) stop("No models found!")
  # Select runs matched by regular expression 
  runs = grep(runnums,mlxruns,value=TRUE) |> stringr::str_sort(numeric=TRUE) 
  # Get all OFVs 
  ofvs = NULL
  if(ifOFV) ofvs = get_allofv(runs,sortAIC=FALSE) 
  # Get all parameters 
  paras = NULL
  if(ifParam) paras = compare_allpara(runs,rse=TRUE)
  out = list(ofvs,paras) 
  return(out)
} 
