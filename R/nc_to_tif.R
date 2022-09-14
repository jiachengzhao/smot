#' Convert NetCDF to GeoTIFF
#'
#' \code{nc_to_tif} converts a NetCDF file to a projected GeoTIFF file.
#'
#' @import tools
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @importFrom raster raster brick crs flip writeRaster
#'
#' @param path_from the path that stores the nc files.
#' @param path_to the path to store the processed .tif files.
#' @param epsg epsg to assign.
#'
#' @examples
#' ### not run
#' # nc_to_tif(path_from = 'C:/Users/jzhao/Downloads/nc_from', path_to = 'C:/Users/jzhao/Downloads/nc_to', epsg = 3035)
#'
#' @export
#'

nc_to_tif = function(path_from, path_to, epsg = 4326) {
  # require(ncdf4)
  # require(raster)
  setwd(path_from)
  f = list.files(pattern = '*.nc')
  k = 1
  for (i in 1:length(f)) {
    nc = nc_open(f[i])
    b = brick(f[i])
    lon = ncvar_get(nc, 'x')
    lat = ncvar_get(nc, 'y')
    value = ncvar_get(nc, names(nc$var)[3])
    for (j in 1:dim(value)[3]) {
      r = raster(t(value[, , j]), xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat))
      crs(r) = paste0('EPSG:', epsg)
      r = flip(r, direction = 'y')
      r.name = paste0(file_path_sans_ext(f[i]), '_', gsub("\\.", "_", names(b)[j]), '.tif')
      writeRaster(r, paste0(path_to, '/', r.name), overwrite = T)
      total = dim(value)[3] * length(f)
      cat(k, 'of', total, 'nc files processed!\n')
      k = k + 1
    }
    nc_close(nc)
  }
}
