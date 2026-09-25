what_u_need <- c("lubridate", "tidyverse", "readxl")
cran_packages <- what_u_need[!(what_u_need %in% installed.packages())]
if(length(cran_packages) != 0) {
  lapply(cran_packages, install.packages, dependencies = TRUE)
}
lapply(what_u_need, library, character.only = TRUE)

merge_coding_sheets <- function(specific, 
                                general, 
                                analysis_order_only = FALSE,
                                export_output = TRUE,
                                output_filename = NULL,
                                output_path = NULL
                                ) {
  # sanity checks
  if(export_output && is.null(output_filename)) {
    stop("Please provide a name for the output file to be exported")
  } else {
    if(is.null(output_path)) {
      stop("Please provide a valid path to the directory where you want to export the output")
    } else{
      if(!dir.exists(output_path)) {
        dir.create(output_path)
      }
    }
  }
  # ----------------- #
  #
  #### Merging two datafiles (general to specific) ####
  #
  # ----------------- #
  
  
  #you might need to change certain columns to characters
  specific <- specific %>%
    mutate(Analysis_order = as.character(Analysis_order))
  general <- general %>%
    mutate(Analysis_order = as.character(Analysis_order))
  
  #Sometimes, column names contain hidden characters like non-breaking spaces (\u00A0) or Unicode issues
  #Look for this in column headers \u00A0
  #dput(colnames(specific))
  colnames(specific) <- gsub("\\s+", "", colnames(specific))
  colnames(general) <- gsub("\\s+", "", colnames(general))
  
  # Check if the they have named the common columns the same in the specific sheet
  if(!all(c("Analysis_order", "Target", "Pressure..level.2.") %in% names(specific))) {
    stop("Please check that chapter specific coding sheet has the following column:\n\t'Analysis_order'\n\t'Target',\n\t'Pressure..level.2.'")
  } else {
    ### Now lets left join combine the two sheets
    # By creating Target.GENERAL and Pressure..level.2..GENERAL directly in the general dataframe
    # we ensure that these columns will always exist after the left_join, regardless of whether there are matching rows.
    general <- general %>%
      mutate(Target.GENERAL = Target,
             Pressure..level.2..GENERAL = Pressure..level.2.
             )
    
    # IF Target, Presure, Species, & EBVs will not be used by chapter co-ordinators,
    # we just need to align by Analysis Order
    #if its just Analysis order (no need for Target and Pressure)
    if(analysis_order_only) {
      message("Merge by 'Analysis_order'")
      specific <- specific %>%
        left_join(general, 
                  by = trimws("Analysis_order"), 
                  suffix = c(".CH", ".GEN")
                  )
    } else {
      message("Merge by 'Analysis order', 'Target' and 'Pressure Level 2'")
      # perform the left join with the three columns (placing a suffix of .CH and .GEN when columns are the same between the two sheets - i.e Short_title)
      # otherwise we will lose these
      # additionally it helps to trimws since some spreadsheets may have these errors
      specific <- specific %>%
        left_join(general, 
                  by = c(trimws("Analysis_order"), 
                         trimws("Target"), 
                         trimws("Pressure..level.2.")
                         ), 
                  suffix = c(".CH", ".GEN")
                  )
      
      # Finally we want to place the Target.GENERAL and Pressure..level.2..GENERAL back where they belong
      specific <- specific %>% 
        relocate(Target.GENERAL, .after = Socio.ecological.system..SES..study.WITHOUT.impact.on.biodiversity) %>%
        relocate(Pressure..level.2..GENERAL, .before = Comment..if..Other...1)
    }
    # And then create a merged_status column whereby we identify if there was a merging error 
    # (as a result of differences in the three column combinations)
    # CH co-ordinators would need to see why these differ and fix accordingly
    specific <- specific     %>%
      mutate(
        match_status = if_else(
          (is.na(Target.GENERAL) | Target.GENERAL == "") & (is.na(Pressure..level.2..GENERAL)  | Pressure..level.2..GENERAL == ""),
          "merge_error",
          "merged"
        )
      )
    # get error counts
    error_nb <- specific %>% 
      pull(match_status) %>%
      str_count("merged") %>%
      sum()
    message(paste0("Please check merge status in output. There were ", error_nb, " rows\nthat were not linked to the general coding sheet.\n\t Check the 'match_status' column in output"))
    
    # Write the file as a csv in your folder
    if(export_output) {
      specific %>%
        write_excel_csv(., paste0(output_path, "/", output_filename,"_GeneralCoding_merged.csv"))
    }
  }
  return(specific)
}

##### Unesting the OWF metadata by OWF for articles
unnest_owf_data <- function(merged_df,
                            varnames = c("Name.of.OWF.s.",
                                         "Consented.Area.Length..km.",
                                         "OWF.Centroidal.Latitude",
                                         "OWF.Centroidal.Longitude",
                                         "Consented.Area..km2.",
                                         "Distance.to.Coast..km.",
                                         "OWF.Type",
                                         "Foundation.Type",
                                         "Installation.Method",
                                         "Substrate.Type",
                                         "Min.Depth..m.",
                                         "Max.Depth..m.",
                                         "Production.Capacity..MW.",
                                         "Turbine.Number",
                                         "Exploitation.start.date",
                                         "Exploitation.phase",
                                         "EUNIS.Habitats",
                                         "IUCN.Protected.Area.Type",
                                         "IUCN.Protected.Area.Category",
                                         "IUCN.Protected.Areas.overlap.with.OWF..km.",
                                         "IUCN.Protected.Areas.overlap.with.OWF....",
                                         "match_status"
                                         ),
                            export_output = TRUE,
                            output_filename = NULL,
                            output_path = NULL
                            ) {
  # sanity checks
  if(export_output && is.null(output_filename)) {
    stop("Please provide a name for the output file to be exported")
  } else {
    if(is.null(output_path)) {
      stop("Please provide a valid path to the directory where you want to export the output")
    } else{
      if(!dir.exists(output_path)) {
        dir.create(output_path)
      }
    }
  }

  get_col <- which(names(merged_df) %in% varnames)
  
  # Split the data into 'fixed' columns (before OWF metadata) and 'nested' columns (from Name of OWF(s) onward)
  
  df_fixed <- merged_df[, -get_col] #this will become duplicates based on number of splitted nested columns
  df_nested <- merged_df[, get_col] #this will be unnested
  
  #Combine all nested columns into a single column of list-columns
  df_nested_long <- df_nested %>%
    mutate(row_id = row_number()) %>%
    pivot_longer(-row_id, names_to = "variable", values_to = "value") %>%
    mutate(value = str_split(value, ";\\s*")) %>%
    unnest(value) %>%
    group_by(row_id, variable) %>%
    mutate(col_instance = row_number()) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    ungroup()
  
  # Now match each row_id and col_instance with the corresponding duplicated fixed data
  df_fixed_long <- df_fixed %>%
    mutate(row_id = row_number()) %>%
    right_join(
      df_nested_long %>% select(row_id, col_instance) %>% distinct(),
      by = "row_id"
    )
  
  # Combine the fixed (before owf metadata) and unnested data (owf metadata) and rename
  final_df <- bind_cols(df_fixed_long, df_nested_long %>% select(-row_id))
  
  #If we want to unnest OWF metadata for the merged data file (specifc and general coding merge)
  if(export_output) {
    final_df %>%
      write_excel_csv(., paste0(output_path, "/", output_filename,"_GeneralCoding_merged_OWFUnnested.csv"))
  }
  return(final_df)
}
