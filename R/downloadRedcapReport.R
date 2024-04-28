#' Download Data Report from RedCap
#'
#' This function retrieves a data report from a RedCap project using API credentials stored
#' in the user's environment file. It requires the name of the environment variable holding
#' the API token, the RedCap API URL, and the report ID.
#'
#' @param redcapTokenName The name of the environment variable containing the RedCap API token.
#' @param redcapUrl The URL to the RedCap API endpoint.
#' @param redcapReportId The ID of the RedCap report to retrieve.
#'
#' @return A tibble containing the data from the RedCap report.
#'
#' @examples
#' # Set up in .REnviron: REDCAP_API_TOKEN="6189879441F5C29A25245880677488BF"
#' downloadRedcapReport("REDCAP_API_TOKEN", "https://redcap.emory.edu/api/", "46524")
#'
#' @importFrom httr POST content
#' @importFrom tibble as_tibble
#' @export
downloadRedcapReport = function(redcapTokenName, redcapUrl, redcapReportId) {
  require(httr)
  require(tibble)

  token = Sys.getenv(redcapTokenName)
  if (token == "") {
    stop("API token is not set in the environment. Check your .REnviron file.")
  }

  formData = list(token = token,
                   content = 'report',
                   format = 'csv',
                   report_id = redcapReportId,
                   csvDelimiter = '',
                   rawOrLabel = 'raw',
                   rawOrLabelHeaders = 'raw',
                   exportCheckboxLabel = 'false',
                   returnFormat = 'csv')

  response = POST(redcapUrl, body = formData, encode = "form")
  if (http_error(response)) {
    stop("Failed to retrieve data: ", http_status(response)$message)
  }

  data = content(response, as = "text", encoding = "UTF-8")
  data = read.csv(text = data)

  return(as_tibble(data))
}
