# wineR
scraping, extracting, and exploring wine reviews (in R)

I scraped ~160k wine reviews from a popular wine magazine to answer a few personal questions about how wine is described. Here's an [initial analysis](http://sadacca.github.io/wineR/) of these data, along with a paired [Shiny app](https://sadacca.shinyapps.io/wine-findr/) to explore the dataset further.

currently includes

- R Notebooks: a web scraper to compile a wine review database, "wine_text_analysis_and_classification.rmd", which houses the code used to generate the above analysis, along with other r-notebooks playing with other analyses

- related content: a helper function multiplot.r, and a csv .txt file of common terms used to describe wine taste

- code for the associated [Shiny app](https://sadacca.shinyapps.io/wine-findr/) lives [next-door](www.github.com/sadacca/wine-findR)

- a HTML rendered version of the analysis linked above and hosted at [github.io](http://sadacca.github.io/wineR/) can also be found in the [docs folder](www.github.com/sadacca/wineR/docs)
