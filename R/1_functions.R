dir_out_reset <- function(dir_out) {
  if(dir.exists(dir_out)) fs::dir_delete(dir_out)
  fs::dir_create(dir_out)
}

#gets info about the system, R, and the lidar related packages/software
system_info <- function(lastools_path) {
  
  system_details <- 
    benchmarkme::get_sys_details(byte_compiler = F, 
                                 linear_algebra = F, 
                                 machine = F)
  
  pkgs <- system_details$installed_packages 
  
  #get lidR, lasR, and lastools versions (if installed)
  lidR_version <- lasR_version <- lastools_version <- NA
  
  if("lidR" %in% pkgs[,1]) {
    lidR_version <- pkgs[which(pkgs[,1]=="lidR"),3]
  } 
  
  if("lasR" %in% pkgs[,1]) {
    lasR_version <- pkgs[which(pkgs[,1]=="lasR"),3]
  } 
  
  # lastools_path <- "C:/LAStools/bin"
  if(dir.exists(lastools_path)) {
    a <- system(paste0(lastools_path,"/las2las.exe -version"), intern = T)  
    lastools_version <- as.numeric(gsub("\\D", "", a))
  }
  
  cpu_info <- benchmarkme::get_cpu()
  
  output <- list(
    nodename = system_details$sys_info$nodename,
    sysname = system_details$sys_info$sysname,
    release = system_details$sys_info$release,
    cpu = cpu_info$model_name,
    cpu_threads = cpu_info$no_of_cores,
    ram = benchmarkme::get_ram(),
    Rversion = R.version.string,
    lidR_version = lidR_version,
    lasR_version = lasR_version,
    lastools_version=lastools_version
  )
  
  return(output)
}


prepare_input <- function(dir_in, dir_out) {
  
  ctg <- catalog(dir_in)
  
  opt_output_files(ctg) <- paste0(dir_out, "/{ORIGINALFILENAME}")
  opt_laz_compression(ctg) <- TRUE
  opt_progress(ctg) <- TRUE
  opt_stop_early(ctg) <- FALSE
  
  return(ctg)
  
}

# get lidar data info
benchmark_ALS_info <- function(x, dir_in) {
  
  info <- capture.output(summary(x)) # x is a lidR catalog object
  total_point_count <- parse_number( info[5] )
  density_points <- parse_number( info[6] )
  density_pulses <- parse_number( info[7] )
  
  file_count <- nrow(x@data)
  
  input_data_size <- sum(fs::dir_info(dir_in)$size)  # total ctg size in GB
  
  return(list(
    total_point_count=total_point_count, 
    density_pulses=density_pulses, 
    density_points=density_points, 
    file_count=file_count, 
    input_data_size=input_data_size)) 
  
  
}

check_results <- function(path) {
  
  #count number of files and total size
  file_count <- length(list.files(path))
  
  output_size <- sum(fs::dir_info(path)$size) 
  
  return(
    list(
      output_file_count = file_count,
      output_total_size = output_size
    )
  )
  
}

benchmark_run <- function(task_id, 
                          run_id,
                          run_lidR = T, 
                          run_lasR = T, 
                          run_lastools = T, 
                          benchmark_tasks, 
                          benchmark_runs) {
  
  cli::cli_alert_info("Benchmark run started: {task_id}, {run_id}")
  
  #get task calls based on task id
  benchmark_tasks_current  <- benchmark_tasks[benchmark_tasks$task_id==task_id,]
  
  #get run info based on run_id
  benchmark_runs_current <- benchmark_runs[benchmark_runs$ID == run_id,]
  
  cores <- benchmark_runs_current$cores
  
  # dir_in required to run the task depends on the data_type (normalized or original)
  if(benchmark_tasks_current$data_type == "datasets_org") dir_in <- benchmark_runs_current$dir_in_org
  if(benchmark_tasks_current$data_type == "datasets_norm") dir_in <- benchmark_runs_current$dir_in_norm
  
  dir_out <- benchmark_runs_current$dir_out
  
  #system info
  sys <- system_info(lastools_path = lastools_path)
  
  # prepare input
  ctg <- prepare_input(dir_in = dir_in, dir_out = dir_out)
  f <- ctg@data$filename #file list for lasR
  f_lastools <- paste(f, collapse = " ") #file list for lastools
  
  #point cloud info
  d_info <- benchmark_ALS_info(ctg, dir_in = dir_in)
  
  #output file names
  output_fname_lidR <- glue("{workstation_id}_{task_id}_{run_id}_lidR")
  output_fname_lasR <- glue("{workstation_id}_{task_id}_{run_id}_lasR")
  output_fname_lastools <- glue("{workstation_id}_{task_id}_{run_id}_lastools")
  
  # function to run lidR call
  benchmark_run_lidR <- function(task_call_lidR) {
    
    cli::cli_h1("Running lidR")
    
    #reset output dir
    dir_out_reset(dir_out)
    
    #setup future multisession 
    x <- paste0("future::plan(future::multisession(workers = ", cores, "))")
    eval(parse(text = x))
    
    #filename for system monitoring log
    # output_fname1 <- glue("{workstation_id}_{task_id}_{run_id}_lidR")
    
    #start system resource monitoring (windows only, a powershell script run in the background)
    MonitoringProcess1 <- 
      callr::r_bg( function(x,y)  system2("powershell", args=c("-file",  "./cpu_ram_usage.ps1", x, y)), args = list(output_fname_lidR, system_monitoring_path)  )  
    
    
    #record start time
    start_time_lidR <- Sys.time()
    
    result_lidR <- tryCatch(expr = {
      eval(parse(text = task_call_lidR))
    },
    error=function(e) { 
      message('An Error Occurred')
      print(e)
      return("ERROR")
    }
    )
    
    #record finish time
    finish_time_lidR <- Sys.time()
    
    #display time elapsed
    cli::cli_alert_success("Finished in {round(as.numeric(finish_time_lidR-start_time_lidR))} sec.")
    
    # record if run was successful
    is_successful_lidR <- 1
    if(is.character(result_lidR) & length(result_lidR)==1) {
      if(result_lidR == "ERROR") is_successful_lidR <- 0
    }
    
    #stop system monitoring
    MonitoringProcess1$kill()
    
    #reset workers
    future::plan(future::sequential)
    
    check_results_lidR <- check_results(dir_out)
    
    return(
      c(
        list(
          tool = "lidR",
          success=is_successful_lidR,
          start_time =  start_time_lidR,
          finish_time= finish_time_lidR
        ),
        check_results_lidR
      )
    )
    
    Sys.sleep(5)
    
  }
  
  
  
  #function to run lasR 
  benchmark_run_lasR <- function(task_call_lasR) {
    
    cli::cli_h1("Running lasR")
    
    #reset output dir
    dir_out_reset(dir_out)
    
    #set parallel strategy (note: more strategies need to be tested)
    set_parallel_strategy(concurrent_files(ncores = cores))
    
    #filename for system monitoring log
    output_fname2 <- glue("{workstation_id}_{task_id}_{run_id}_lasR")
    
    #start system resource monitoring (windows only, a powershell script run in the background)
    MonitoringProcess2 <- 
      callr::r_bg( function(x,y)  system2("powershell", args=c("-file",  "./cpu_ram_usage.ps1", x, y)), args = list(output_fname2, system_monitoring_path)  )  
    
    #wildcard output path for lasR
    fout <- paste0(dir_out, "/*.",benchmark_tasks_current$output_format)
    
    start_time_lasR <- Sys.time()
    
    result_lasR <- tryCatch(expr = {
      eval(parse(text = task_call_lasR))
    },
    error=function(e) {
      message('An Error Occurred')
      print(e)
      return("ERROR")
    }
    )
    
    finish_time_lasR <- Sys.time()
    
    #stop system monitoring
    MonitoringProcess2$kill()
    
    cli::cli_alert_success("Finished in {round(as.numeric(finish_time_lasR-start_time_lasR))} sec.")
    
    is_successful_lasR <- 1
    if(is.character(result_lasR) & length(result_lasR)==1) {
      if(result_lasR == "ERROR") is_successful_lasR <- 0
    }
    
    check_results_lasR <- check_results(dir_out)
    
    return(
      c(
        list(
          tool = "lasR",
          success=is_successful_lasR,
          start_time =  start_time_lasR,
          finish_time= finish_time_lasR
        ),
        check_results_lasR
      )
    )
    
    
    Sys.sleep(5)
  }
  
  
  
  #function to run lastools
  benchmark_run_lastools <- function(task_call_lastools) {
    
    cli::cli_h1("Running lastools")
    
    #filename for system monitoring log
    output_fname3 <- glue("{workstation_id}_{task_id}_{run_id}_lastools")
    
    # fout_results_lastools <- file.path(reports_path, paste0(output_fname3,".csv"))
    # 
    # if(!file.exists(fout_results_lastools)) {
    
    #reset output dir
    dir_out_reset(dir_out)
    
    #lastools command to execute 
    command <- paste0(lastools_path, "/", task_call_lastools)
    
    #start system resource monitoring (windows only, a powershell script run in the background)
    MonitoringProcess3 <- 
      callr::r_bg( function(x,y)  system2("powershell", args=c("-file",  "./cpu_ram_usage.ps1", x, y)), args = list(output_fname3, system_monitoring_path)  )  
    
    start_time_lastools <- Sys.time()
    
    result_lastools <- tryCatch(expr = {
      system(glue::glue(command)) #glue is crucial here because of the {} inside the call
    },
    error=function(e) {
      message('An Error Occurred')
      print(e)
      return("ERROR")
    }
    )
    
    finish_time_lastools <- Sys.time()
    
    #stop system monitoring
    MonitoringProcess3$kill()
    
    cli::cli_alert_success("Finished in {round(as.numeric(finish_time_lastools-start_time_lastools))} sec.")
    
    is_successful_lastools <- 1
    if(is.character(result_lastools) & length(result_lastools)==1) {
      if(result_lastools == "ERROR") is_successful_lastools <- 0
    }
    
    check_results_lastools <- check_results(dir_out)
    
    O <-     
      c(
        list(
          tool = "lastools",
          success = is_successful_lastools,
          start_time =  start_time_lastools,
          finish_time= finish_time_lastools
        ),
        check_results_lastools
      )
    
    
    return(O)
    
    
    Sys.sleep(5)
  }
  
  ######
  
  benchmark_lidR_result <- benchmark_lasR_result <- benchmark_lastools_result <- NA
  
  common_output <- 
    tibble(
    task_id=task_id,
    run_id = run_id,
    cores = cores) %>%
    bind_cols(
          as_tibble(sys),
          as_tibble(d_info)
    )
  
  if(run_lidR) {
    f_results <- glue::glue("{reports_path}/{output_fname_lidR}.csv")
    if(!file.exists(f_results)) {
      benchmark_lidR_result <- benchmark_run_lidR(task_call_lidR = benchmark_tasks_current$task_call_lidR)  
      R <- bind_cols(benchmark_lidR_result, common_output)
      write.csv(R, file = f_results)
      
    } else {
      cli::cli_warn(glue::glue("Output already exist: {f_results}"))
    }
  }
  
  if(run_lasR) {
    f_results <- glue::glue("{reports_path}/{output_fname_lasR}.csv")
    if(!file.exists(f_results)) {
      benchmark_lasR_result <- benchmark_run_lasR(task_call_lasR = benchmark_tasks_current$task_call_lasR) 
      R <- bind_cols(benchmark_lasR_result, common_output)
      write.csv(R, file = f_results)
      
    } else {
      cli::cli_warn(glue::glue("Output already exist: {f_results}"))
    }
  }
  
  if(run_lastools) {
    f_results <- glue::glue("{reports_path}/{output_fname_lastools}.csv")
    if(!file.exists(f_results)) {
      benchmark_lastools_result <- benchmark_run_lastools(task_call_lastools = benchmark_tasks_current$task_call_lastools)
      R <- bind_cols(benchmark_lastools_result, common_output)
      write.csv(R, file = f_results)
      
    } else {
      cli::cli_warn(glue::glue("Output already exist: {f_results}"))
    }
  }
  
  
  # if(run_lasR) benchmark_lasR_result <- benchmark_run_lasR(task_call_lasR = benchmark_tasks_current$task_call_lasR)
  # if(run_lastools) benchmark_lastools_result <- benchmark_run_lastools(task_call_lastools = benchmark_tasks_current$task_call_lastools)
  
  # # return(benchmark_lidR_result)
  # output <- 
  #   bind_rows(
  #     as_tibble(benchmark_lidR_result),
  #     as_tibble(benchmark_lasR_result),
  #     as_tibble(benchmark_lastools_result)
  #   ) %>% 
  #   mutate(task_id=task_id,
  #          run_id = run_id,
  #          cores = cores) %>%
  #   bind_cols(
  #     as_tibble(sys),
  #     as_tibble(d_info)
  #     
  #   )
  # 
  # return(output)
  
}






