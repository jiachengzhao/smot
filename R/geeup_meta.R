#' Write the Metadata file for uploading images to Google Earth Engine (GEE) Using geeup
#'
#' \code{geeup_meta} writes a .csv metadata file for images to upload to GEE.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @importFrom data.table fwrite
#' @importFrom lubridate as_datetime force_tz
#'
#' @param nc_path the path that stores the nc files.
#' @param tif_path the path that stores the tif files.
#' @param ori date origin (e.g., '2000-01-01').
#'
#' @examples
#' ### not run
#' # geeup_meta(nc_path = 'C:/Users/jzhao/Downloads/nc/tas_Paris_UrbClim_2015_07_v1.0.nc', tif_path = 'C:/Users/jzhao/Downloads/tif', '2015-01-01')
#'
#' @export
#'

# this function accounts for the nc data formatting its time in local time zone
# geeup_meta = function(nc_path, tif_path, ori, tz) {
#   # nc
#   nc = nc_open(nc_path)
#   # nc time
#   nc.time = ncvar_get(nc, 'time')
#   nc_close(nc)
#   # start time
#   start = as_datetime(nc.time[1]*3600, origin = ori)
#   # end time
#   end = as_datetime(nc.time[length(nc.time)]*3600, origin = ori)
#   # timeseries in local time zone
#   ts.tz = force_tz(seq(start, end, by = '1 hour'), tz)
#   # timeseries in UTC
#   ts.utc = as_datetime(ts.tz, tz = 'UTC')
#   # timeseries in unix time
#   ts.unix = as.numeric(ts.utc)
#   # image name
#   image.name = file_path_sans_ext(list.files(tif_path, pattern = '\\.tif$'))
#   # return metadata.csv
#   fwrite(
#     data.table(
#       id_no = image.name,
#       system_time_start = ts.unix
#     ), paste0(tif_path, '/metadata.csv')
#   )
# }

# this function accounts for the nc data formatting its time in UTC
geeup_meta = function(nc_path, tif_path, ori) {
  # nc
  nc = nc_open(nc_path)
  # nc time
  nc.time = ncvar_get(nc, 'time')
  nc_close(nc)
  # start time
  start = as_datetime(nc.time[1]*3600, origin = ori)
  # end time
  end = as_datetime(nc.time[length(nc.time)]*3600, origin = ori)
  # timeseries in UTC
  ts.utc = seq(start, end, by = '1 hour')
  # timeseries in unix time
  ts.unix = as.numeric(ts.utc)
  # image name
  image.name = file_path_sans_ext(list.files(tif_path, pattern = '\\.tif$'))
  # return metadata.csv
  fwrite(
    data.table(
      id_no = image.name,
      system_time_start = ts.unix
    ), paste0(tif_path, '/metadata.csv')
  )
}
