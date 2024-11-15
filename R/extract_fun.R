#' Extract Gridded Data from NetCDF Based on Shapefile
#'
#' This function extracts data from a NetCDF file based on the regions defined in a shapefile.
#' The user provides the names of the columns in the shapefile that represent state and district.
#' The function can apply an aggregation function (e.g., mean, sum) to the extracted data.
#'
#' @param shapefile_path The path to the shapefile containing region boundaries.
#' @param netcdf_path The path to the NetCDF file containing gridded data.
#' @param varname The name of the variable to extract from the NetCDF file.
#' @param state_field The column name in the shapefile that contains state or region names.
#' @param district_field The column name in the shapefile that contains district or subregion names.
#' @param fun The function to apply to the extracted data (default is mean).
#' @param na.rm Whether to remove NA values (default is TRUE).
#' @param output_csv Optional; path to save the results as a CSV file.
#'
#' @return A data frame with the extracted and aggregated data, and state/district information.
#' @export
extractgridded <- function(shapefile_path,
                           netcdf_path,
                           varname,
                           state_field,
                           district_field,
                           fun = mean,
                           na.rm = TRUE,
                           output_csv = NULL) {

  # Read the shapefile
  shp <- shapefile(shapefile_path)

  # Check if the state_field and district_field exist in the shapefile
  if (!state_field %in% names(shp)) {
    stop(paste("State field", state_field, "does not exist in the shapefile."))
  }
  if (!district_field %in% names(shp)) {
    stop(paste("District field", district_field, "does not exist in the shapefile."))
  }

  # Read the NetCDF file
  nc_data <- nc_open(netcdf_path)
  print(nc_data)  # Optional: print information about the NetCDF file

  # Extract the relevant variable (e.g., temperature) from the NetCDF file
  gridded_data <- brick(netcdf_path, varname = varname)

  # Extract data from the gridded dataset based on the shapefile
  extracted_data <- extract(gridded_data, shp, fun = fun, na.rm = na.rm, df = TRUE)

  # Convert the extracted data to a data frame
  extracted_df <- data.frame(extracted_data)

  # Combine the shapefile's state and district information with the extracted data
  result_df <- data.frame(state = shp[[state_field]],
                          district = shp[[district_field]],
                          extracted_df)

  # View the resulting data (optional)
  # View(result_df)

  # Write the output to CSV if specified
  if (!is.null(output_csv)) {
    write.csv(result_df, output_csv, row.names = FALSE)
  }

  # Return the resulting data frame
  return(result_df)
}
