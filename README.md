mathematica-kplots
==================

Customised plotting functions for Mathematica

## Requrements

 - Mathematica 10

## Examples

### ListPlot  

    << KPlots`
    ListPlot[
      Sin /@ Range[0, 4 Pi, 0.3],
      FrameLabel -> { "angle", "amplitude"},
      PlotMarkers -> CustomMarkers[6, 8, Orange],
      KPlotsTheme
    ]

![demo_listplot](https://cloud.githubusercontent.com/assets/4820843/5002404/7344da66-69f8-11e4-8a52-893289802983.png)
