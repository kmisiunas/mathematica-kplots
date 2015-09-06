mathematica-kplots
==================

Academically perfected plots for Mathematica! Plots are optimised for PDF exports for embeding in LaTeX documments. 

## Examples

### ListPlot  

    << KPlots`
    ListPlot[
      Sin /@ Range[0, 4 Pi, 0.3],
      FrameLabel -> {"Phase (deg)", "Amplitude (mV)"},
      PlotMarkers -> CustomMarkers[6, 8, Orange],
      KPlotsTheme
    ]

<img width="366" alt="screen shot 2015-09-06 at 14 50 44" src="https://cloud.githubusercontent.com/assets/4820843/9704671/c79151dc-54a6-11e5-8b7b-136ed0c48cf6.png">

    Export["list_plot.pdf", %]

<img width="411" alt="screen shot 2015-09-06 at 14 55 41" src="https://cloud.githubusercontent.com/assets/4820843/9704692/6ef482f0-54a7-11e5-944d-845fbcb06340.png">

## Usage


## Requrements

 - Mathematica 10 or later

## Contributions 

`CustomTicks.m` library was adapted from [Mark Caprio code](http://scidraw.nd.edu/). Thanks!

`xkcdify[plot_]` functions was adapted from [stackexchange discussion](http://mathematica.stackexchange.com/questions/11350/xkcd-style-graphs)

`FastListPlot[]` function was adapted form [stackexchange discussion](http://mathematica.stackexchange.com/questions/140/listplot-plotting-large-data-fast)
