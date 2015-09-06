mathematica-kplots
==================

Academically perfected plots for Mathematica! Plots are optimised for PDF exports for embedding in LaTeX documents. 

## Examples

### ListPlot  

    << KPlots`
    ListPlot[
      Sin /@ Range[0, 4 Pi, 0.3],
      FrameLabel -> {"Phase (deg)", "Amplitude (mV)"},
      PlotMarkers -> CustomMarkers[6, 8, Orange],
      PlotTheme -> "KPlots"
    ]

<img width="366" alt="screen shot 2015-09-06 at 14 50 44" src="https://cloud.githubusercontent.com/assets/4820843/9704671/c79151dc-54a6-11e5-8b7b-136ed0c48cf6.png">

    Export["list_plot.pdf", %]

<img width="411" alt="screen shot 2015-09-06 at 14 55 41" src="https://cloud.githubusercontent.com/assets/4820843/9704692/6ef482f0-54a7-11e5-944d-845fbcb06340.png">

### OListPlot

After Origin Pro plots, clean style with optional error bar support.

    OListPlot[
     {#, Sin[#]} & /@ Range[0, 4 Pi, 0.3],
     ErrorBars -> RandomReal[0.3, 42],
     FrameLabel -> {"Phase (rad)", "Amplitude (mV)"},
     PlotRange -> {-1.4, 1.4}
    ]
    
<img width="353" alt="screen shot 2015-09-06 at 15 29 56" src="https://cloud.githubusercontent.com/assets/4820843/9704875/6461cf8c-54ac-11e5-9113-594ddfb70180.png">

## Guidelines

`ImageSize -> 350` is the standard width and it was designed to correspond to 8cm on smaller plots and 10cm for wider ones. 

The theme formatting can be invoked by calling `PlotTheme -> "KPlots"` or adding `KPlotsTheme` at the end of the plot specification. 


### Other methods

`FastListPlot[data_, options_]` for faster ListPlot when you have a lot of data.

`KColor[int_]` provides nice colors for overlapping data plots

`RoundPlotTicks[plot_]` a method for rounding tick endings for perfectionists. 

`xkcdify[plot_]` makes plot look like it was from XKCD comics. Just for fun.


## Requrements

 - Mathematica 10 or later

## Contributions 

`CustomTicks.m` library was adapted from [Mark Caprio code](http://scidraw.nd.edu/). Thanks!

`xkcdify[plot_]` functions was adapted from [stackexchange discussion](http://mathematica.stackexchange.com/questions/11350/xkcd-style-graphs)

`FastListPlot[]` function was adapted form [stackexchange discussion](http://mathematica.stackexchange.com/questions/140/listplot-plotting-large-data-fast)
