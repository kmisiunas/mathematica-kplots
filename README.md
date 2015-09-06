mathematica-kplots
==================

Academically perfected plots for Mathematica!

## Requrements

 - Mathematica 10 or later

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




## Contributions 

`CustomTicks.m` library was adapted from [Mark Caprio](http://physics.nd.edu/people/faculty/mark-caprio/) code. Thanks!
