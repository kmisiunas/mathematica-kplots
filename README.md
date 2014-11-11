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

![demo_listplot](https://cloud.githubusercontent.com/assets/4820843/5002438/de12279a-69f8-11e4-8a52-ddb2af94a829.png)
