mathematica-kplots
==================

Academically perfected plots for Mathematica! Plots are optimised for PDF exports for embeding in LaTeX documments. 

## Examples

### ListPlot  

    << KPlots`
    ListPlot[
      Sin /@ Range[0, 4 Pi, 0.3],
      FrameLabel -> { "angle", "amplitude"},
      PlotMarkers -> CustomMarkers[6, 8, Orange],
      KPlotsTheme
    ]

![demo_listplot](https://cloud.githubusercontent.com/assets/4820843/5002438/de12279a-69f8-11e4-8a52-ddb2af94a829.png)

## Usage


## Requrements

 - Mathematica 10 or later

## Contributions 

`CustomTicks.m` library was adapted from [Mark Caprio](http://physics.nd.edu/people/faculty/mark-caprio/) code. Thanks!

`xkcdify[plot_]` functions was adapted from [stackexchange discussion](http://mathematica.stackexchange.com/questions/11350/xkcd-style-graphs)
