mathematica-kplots
==================

Academically perfected plots for Mathematica! Plots are optimised for PDF exports for embedding in LaTeX documents. 

## Methods

##ListPlotErrors[]  

    << KPlots`

    x = Range[10];
    y = Sin[x/2];
    data = Transpose[{x, y}];
    err = RandomReal[0.25, {10, 2}];
    
    ListPlotErrors[ data, ErrorBars -> err]

<img width="371" alt="screen shot 2018-06-12 at 12 18 08" src="https://user-images.githubusercontent.com/4820843/41287504-cf8384d6-6e3a-11e8-8eca-c5bb879ff578.png">


Alternative way of specifying the errors:

    ListPlotErrors[Join[data, err, 2]]

### ScienceTheme[]

After Origin Pro plots, clean style with optional error bar support.

    ListPlotErrors[ 
      data, 
      ErrorBars -> err,
      ScienceTheme[]
    ]
    
<img width="353" alt="screen shot 2018-06-12 at 12 44 38" src="https://user-images.githubusercontent.com/4820843/41288555-a1abc6dc-6e3e-11e8-8cd9-8af72b52f4af.png">


Stable under Frame labels 
    
    ListPlotErrors[ 
      data, 
      ErrorBars -> err,
      FrameLabel -> {"X axis", "Y (impact)"},
      ScienceTheme[]
    ]
    
<img width="372" alt="screen shot 2018-06-12 at 12 45 26" src="https://user-images.githubusercontent.com/4820843/41288567-aba13348-6e3e-11e8-9f14-e8065abd13c9.png">

Options: 
    
    "Size" -> 8.0,    (* output plot size in cm *)
	"Labels" -> True, (* print tick labels *)
	"xticks" -> Automatic, (* Y minor ticks *)
	"yticks" -> Automatic, (* Y major ticks *)
	"Trim" -> False, (* Trim the figure tightly *)
	FontSize -> 8,
	"Scaling" -> 0.6 (* Mathematica to Export scaling to Illustrator *)";


### ThesisTheme

Simpler theme specification 




### Other methods

`FastListPlot[data_, options_]` for faster ListPlot when you have a lot of data.

`KColor[int_]` provides nice colors for overlapping data plots

`RoundPlotTicks[plot_]` a method for rounding tick endings for perfectionists. 



## Requrements

 - Mathematica 10 or later

## Contributions 

`CustomTicks.m` library was adapted from [Mark Caprio code](http://scidraw.nd.edu/). Thanks!

`FastListPlot[]` function was adapted form [stackexchange discussion](http://mathematica.stackexchange.com/questions/140/listplot-plotting-large-data-fast)
