library(rmarkdown)
# Render page
rmarkdown::render('scripts/cbs_news_price_tracker.Rmd', 
                  output_dir = "docs",
                  output_file = 'cbs_news_price_tracker.html')