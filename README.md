
# 1. lucidum

lucidum is an open source R Shiny app to help users build and communicate GLMs and GBMs without writing code.

To see lucidum in action visit [ShinyConf 2023 - lucidum app showcase](https://www.youtube.com/watch?v=AryLn4rVeHc).

lucidum works with standard R data.frames and data.tables and is designed to make model building more interactive,  visual and insightful.

I originally wrote lucidum to automate the repetitive tasks involved when building the types of regression models common to UK personal lines insurers.  More recently, I have used it as a tool to help insurers move from GLMs to GBMs (specifically [LightGBM](https://github.com/microsoft/LightGBM)), using SHAP values to understand and communicate model features and interaction effects.

lucidum's functionality includes:

* **Collection of metadata to support a modelling exercise**
  - define modelling KPIs (e.g. frequency by claim peril)
  - filters to apply to charts and maps (e.g. new business vs renewals)
  - base levels and bandings to apply when tabulating a GLM
  - setup feature scenarios for inclusion in a GBM  
* **Interactive charting**
  - actual vs expected charting by rating factor, with easy access to filters and training vs test views
  - plot several models' predictions (GLMs and GBMs) simultaneously
  - user-defined banding for continuous features - no "pre-banding" required
  - overlay "single profile" lines for GLMs to understand the underlying model effect
  - overlay SHAP value ribbons for GBMs to understand the underlying model effect

* **Interactive mapping of data at UK Postcode Area, Sector resolution**  
  - uses the [leaflet](https://github.com/Leaflet/Leaflet) library to draw choropleth maps for Postcode Area and Sector
  - uses open source shapefiles for Area and Sector

* **Support a GLM build**  
  - "formula helper" to make the job of building an R GLM formula much faster
  - convert GLMs to tabular format ("ratebooks") with user-defined bandings and base levels
  - export tabulated GLMs as Excel workbooks  

* **Support a GBM build**  
  - provide a simple user interface for GBM feature selection
  - provide a simple user interface to the most common LightGBM parameters
  - build GA2M models (1D+2D GBMs) to support interaction detection
  - use feature interaction constraints to build indices for high cardinality features like postcode
  - 1D SHAP plots to interpret the model's main effects
  - 2D SHAP plots to  interpret interaction effects
  - convert GBMs to tabular format ("ratebooks") with user-defined bandings and base levels

## 2. Installation

You can install the development version of lucidum from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SpeckledJim2/lucidum")
```

## 3. Load a dataset into lucidum

``` r
library(lucidum)
lucidum(your_dataframe_name)
```

To load the demo dataset into lucidum use:

``` r
library(lucidum)
lucidum(insurance, starting_response = 'price')
```

## 4. Training and test rows

To separate training and test rows in your dataset, include a numerical column called "train_test" with value 0 for training and 1 for test.

## 5. Specification files

Specification files make lucidum more useful by specifying metadata to make model building faster.  

Specification files are .csv files which can be created within lucidum itself or in a text editor.
You don’t have to use specification files, but they make life easier if you are going to be working with a dataset on a regular basis.

There are three types of specification files:  

1. KPI specification: the metrics you want to access quickly in the app’s sidebar  
2. Filter specification: formulae that define filters you want to apply to charts and maps  
3. Feature specification: quicker access to features in ChartaR and feature scenarios that you want to use in your models

