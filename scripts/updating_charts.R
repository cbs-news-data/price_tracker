library(DatawRappr)

# Retrieve API key from env
api_key <- Sys.getenv("DATAWRAPPER_API_KEY")
# Check if API key is present
if (api_key == "") {
  stop("Datawrapper API key not found. Please set the DATAWRAPPER_API_KEY environment variable.")
}
# Authenticate with Datawrapper
datawrapper_auth(api_key)

# Publish the charts related to average item prices
dw_publish_chart(chart_id = 'YK0KE')
dw_publish_chart(chart_id = 'aVy9W')
dw_publish_chart(chart_id = 'LY7fa')

# Publish the charts related to building prices
dw_publish_chart(chart_id = 'apXeE')
dw_publish_chart(chart_id = '9S6vc')

# Publish the charts related to oil prices
dw_publish_chart(chart_id = 'apXeE')
dw_publish_chart(chart_id = '9S6vc')

# Publish the charts related to gasoline prices
dw_publish_chart(chart_id = 'apXeE')
dw_publish_chart(chart_id = '9S6vc')

# Publish the charts related to rent and home prices
dw_publish_chart(chart_id = 'apXeE')
dw_publish_chart(chart_id = '9S6vc')




