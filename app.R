library(dash)
library(ggplot2)
library(plotly)
library(dashHtmlComponents)
library(dplyr)
library(readr)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

gapminder <- readr::read_csv("data/processed/gapminder_processed.csv")

app$layout(
  dbcContainer(
    list(
      htmlH1("Gapminder: top 10 countries"),
      dccDropdown(
        id = "target",
        options = list(
          list(label = "Income", value = "income"),
          list(label = "Life Expectancy", value = "life_expectancy"),
          list(label = "Children per Woman", value = "children_per_woman"),
          list(label = "Child Mortality", value = "child_mortality"),
          list(label = "Population Density", value = "pop_density"),
          list(label = "CO2 Per Capita", value = "co2_per_capita"),
          list(label = "Years in school (Men)", value = "years_in_school_men"),
          list(label = "Years in school (Women)", value = "years_in_school_women")
        ),
        value = "life_expectancy",
        style = list("width" = "350px", "margin-bottom" = "10px")
      ),
      dccDropdown(
        id = "region",
        options = list(
          list(label = "Africa", value = "Africa"),
          list(label = "Asia", value = "Asia"),
          list(label = "Americas", value = "Americas"),
          list(label = "Europe", value = "Europe"),
          list(label = "Oceania", value = "Oceania")
        ),
        value = "Africa",
        style = list("width" = "350px", "margin-bottom" = "10px")
      ),
      dccDropdown(
        id = "year",
        options = list(
          list(label = "2007", value = "2007"),
          list(label = "2008", value = "2008"),
          list(label = "2009", value = "2009"),
          list(label = "2010", value = "2010"),
          list(label = "2011", value = "2011"),
          list(label = "2012", value = "2012"),
          list(label = "2013", value = "2013"),
          list(label = "2014", value = "2014")
        ),
        value = "2014",
        style = list("width" = "350px", "margin-bottom" = "10px")
      ),
      dbcContainer(
        list(
          dccGraph(id='bar-chart')
        )
      )
    ),
    style = list("padding" = "20px")
  )
)

app$callback(
  output('bar-chart', 'figure'),
  list(input('target', 'value'),
       input('region', 'value'),
       input('year', 'value')),
  function(target, region_f, year_f) {
    gm_target <- gapminder %>%
      filter(region == region_f, year == year_f) %>%
      select(region, country, year, target)
    
    colnames(gm_target)[4] <- "target"
    
    gm_target <- gm_target[order(-gm_target$target),][1:10,]
    
    
    bar_chart_top10 <- ggplot(gm_target, aes(
      x = target, y = reorder(country, target))) +
      geom_col(show.legend = FALSE, fill = "lightblue") +
      labs(y = "Country", x = target,
           title = "Top 10 countries")
    
    ggplotly(bar_chart_top10)
    
  }
)

app$run_server(host = '0.0.0.0')