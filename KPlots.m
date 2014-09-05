(* ::Package:: *)

(* 	Plots that meet Keyser Lab standards for plots.
	---
	There are two types:
	OListPlot - plots mimicking OriginPro style with perfect error bars
	KListPlot - Modern visualisation by Karolis

	Versions:
	1.0 - initial release. 
	2.0b1 - 2014-09-05 - Replaced KTicks with modified version of CustomTicks
	2.0b2 - 2014-09-05 - Added KPlot theme that captures most important style elements
	2.0b3 - 2014-09-05 - Added RoundPlotTicks for rounding tick ends
	2.0b3 - 2014-09-05 - Removed dependency on Wolframs ErrorBarPlots`
	2.0 - 2014-09-05 - Updated OListPlot and KListPlot
 *)

BeginPackage["KPlots`", {"CustomTicks`"}]

KListPlot::usage = "KListPlot[data_, ErrorBars->..., opt->...] list plot for data";
OListPlot::usage = "OListPlot[data_, ErrorBars->..., opt->...] list plot for data in Origin style. Supports all sorts of markers and standard error-bars";
FastListPlot::usage = "FastListPlot[data_] will perform fast plotting for large data sets";

KPlotsTheme::usage = "KPlotsTheme can be used to pass as list of options or can be applied as plot theme: PlotTheme -> \"KPlots\" ";

ErrorBars::usage = "Option for providing Error Bars directly into KListPlot or OListPlot";
ErrorBarPlotStyle::usage = "Option for specifying error bar style";
ErrorBarPlotStyleFilling::usage = "Option for specifying error bar filling for KListPlot";
ErrorBarInterpolationOrder::usage = "Option for specifying interpolation order for connected error bars";
ErrorBarStyle::usage = "Option for choosing a style for drawing error bars";
ErrorBarWidth::usage = "Option for error bar hat width"

KErrorBarFunction::usage = "Custom function for drawing error bars - lines";
KColor::usage = "KColor[int] gives a colour from selection of nice shades";

xkcdify::usage = "converts plot into xkcd styled plot: "; 
RoundPlotTicks::usage = "Rounds[plot] rounds ticks and shows final look of the plot";

(*error messages*)
KPlots::errSameLength = "plotted data must be the same length as error array";


(* Implementations *)
Begin["`Private`"]


(* ==============================  BEAUTIFUL PLOT THEME ================================ *)

SetOptions[ LinTicks,
	MinorTickLength -> 0.009,
	MajorTickLength -> 0.015,
	NumberOfMinorTicks -> 1
];

KPlotsTheme := { 
	Frame -> True,
	FrameStyle -> Directive[Black, 13, FontFamily -> "Helvetica Neue", AbsoluteThickness[1.5]],
	FrameTicks ->
    	{{LinTicks, LinTicks[#1, #2, ShowTickLabels -> False] &},
     	 {LinTicks, LinTicks[#1, #2, ShowTickLabels -> False] &}},
	GridLines -> None,
	ImageSize -> 350
};

(* add a theme compatible with in-built ones *)
System`PlotThemeDump`resolvePlotTheme["KPlots", "Plot" | "ListPlot"] :=
	Themes`SetWeight[ KPlotsTheme, System`PlotThemeDump`$ComponentWeight];



(* ==============================  ERROR BAR DRAWING FUNCTION ================================ *)

(*specialisd function for preparing error bars*)
FormatErrorBars[x_]:= Print["KPots error: unrecognised error bar: "~~ToString@x];
FormatErrorBars[x : ErrorBar[_?NumberQ]] := 
	{{0,0} , {x[[1]], -x[[1]]}};
FormatErrorBars[x : ErrorBar[_?NumberQ, _?NumberQ ]] := 
	{{x[[1]], -x[[1]]} , {x[[2]], -x[[2]]}};
FormatErrorBars[x : ErrorBar[{_?NumberQ, _?NumberQ }]] := 
	{{0,0} , {x[[1,1]], x[[1,2]]}};
FormatErrorBars[x : ErrorBar[{_?NumberQ, _?NumberQ }, {_?NumberQ, _?NumberQ }]] := 
	{{x[[1,1]], x[[1,2]]} , {x[[2,1]], -x[[2,2]]}};

(* KErrorBarFunction := Function[{coords, errs}, {Opacity[0.2], 
    Rectangle[coords + {errs[[1, 1]], errs[[2, 1]]}, 
     coords + {errs[[1, 2]], errs[[2, 2]]}]}] *)
KErrorBarFunction[size_] := Function[ 
	{coords, errs} ,
	Module[ {xline, yline, x, y, xmin, xmax, ymin, ymax},
		x = coords[[1]];
		y = coords[[2]];
		(* Print@{coords, errs}; *)
		{{xmin, xmax}, {ymin,ymax}} = FormatErrorBars@errs;
		(* Print@{x,y};
		Print@{{xmin, xmax}, {ymin,ymax}} ; *)
		If[ xmin===0 && xmax===0,
			xline={},
			xline={
				Line[{{x+xmax, y}, {x+xmin, y}}],
				Line[{Offset[{0,size}, {x+xmax, y}], Offset[{0,-size},{x+xmax, y}]}],
				Line[{Offset[{0,size}, {x+xmin, y}], Offset[{0,-size}, {x+xmin, y}]}]}
		];
		If[ ymin===0 && ymax===0,
			yline={}, 
			yline={
				Line[{{x, y+ymax}, {x, y+ymin}}],
				Line[{Offset[{size,0}, {x, y+ymax}], Offset[{-size,0}, {x, y+ymax}]}],
				Line[{Offset[{size,0}, {x, y+ymin}], Offset[{-size,0}, {x, y+ymin}]}]
			}
		];
		Join[xline, yline]
	]
]

(* ==============================  ORIGIN PLOT WITH ERRORS ================================ *)

Options[OListPlot] = Normal@Join[
	(* Association@Options[ListPlot], *)
	Association@{
		MaxPlotPoints->Infinity,
		PlotMarkers -> {Graphics`PlotMarkers[][[2, 1]], 13} , 
		Mesh -> All,
		Joined -> False,
		PlotStyle -> Directive[Black],
		PlotTheme -> "KPlots",
		ErrorBars -> None,
		ErrorBarWidth -> 1.4,
		ErrorBarStyle -> AbsoluteThickness[1.2]
	}	
];

OListPlot[data_, opts: OptionsPattern[OListPlot]] := Module[
	{
		fullOpts = Join[ Association@Options[OListPlot], Association@{opts} ],
		errorsPlot, dataPlot
	},

	If[ !MatrixQ@data, Print["KPlots error: OListPlot data must be a matrix"]];
	dataPlot = ListPlot[
		data ,
		Evaluate@FilterRules[ Normal@fullOpts, Options[ListPlot]]
	];

	errorsPlot = If[ ! VectorQ @ fullOpts[ErrorBars] , {} ,
		If[Length @ fullOpts[ErrorBars] != Length @ data, Message[KPlots::errSameLength]];
		errorData = Table[{ data[[i]] , ErrorBar[ fullOpts[ErrorBars][[i]] ]}, {i, Length[data]}];
		Graphics[ Flatten[ Join[ 
			{fullOpts[PlotStyle]},
			{fullOpts[ErrorBarStyle]},
  		KErrorBarFunction[fullOpts[ErrorBarWidth]][#[[1]], #[[2]]] & /@ errorData
  		] , 1]   ]
	];
  
	Show[
		dataPlot,
		errorsPlot
		(* Evaluate@FilterRules[ Normal@fullOpts, Options[Graphics]] (*does not work with PlotTheme*) *)
	]
];


(* ==============================  MODERN PLOT WITH ERRORS ================================ *)

Options[KListPlot] = Normal@Join[
	(* Association@Options[ListPlot], *)
	Association@{
		MaxPlotPoints->Infinity,
		PlotMarkers -> {Graphics`PlotMarkers[][[2, 1]], 14},
		MeshShading -> {Red,Blue},
		Mesh -> All,
		Joined -> True,
		PlotStyle -> Directive[KColor[5]],
		PlotTheme -> "KPlots",
		ErrorBars -> None,
		ErrorBarPlotStyle -> Directive[Orange, Thin],
		ErrorBarPlotStyleFilling -> Directive[Orange, Opacity[0.1]],
		ErrorBarInterpolationOrder -> 2
	}	
];


KListPlot[data_, opts: OptionsPattern[]] := Module[ 
	{
		fullOpts = Join[ Association@Options[KListPlot], Association@{opts} ],
		errorData, errorsPlot
	},
  
	errorsPlot = If[ ! VectorQ @ fullOpts[ErrorBars] , {},
		If[Length @ fullOpts[ErrorBars] != Length @ data, Message[KPlots::errSameLength]];
		errorData = Transpose @ {data[[;; , 1]] , data[[;; , 2]] - fullOpts[ErrorBars], data[[;; , 2]] + fullOpts[ErrorBars]} ;
		ListPlot[{errorData[[;; , {1, 2}]], errorData[[;; , {1, 3}]]},
			Filling -> {1 -> {2}},
			Joined -> True ,
			InterpolationOrder -> fullOpts[ErrorBarInterpolationOrder],
			FillingStyle -> fullOpts[ErrorBarPlotStyleFilling],
			PlotStyle -> fullOpts[ErrorBarPlotStyle]
		]
	];

  	Show[
		errorsPlot,
		ListPlot[data, 
			Evaluate@FilterRules[ Normal@fullOpts, Options[ListPlot]]
		],
		KPlotsTheme,
		Evaluate@FilterRules[ Normal@fullOpts,  Options[Graphics]]
  ]
]


(* ==============================  FAST LIST PLOT  ================================ *)

(* from: http://mathematica.stackexchange.com/questions/140/listplot-plotting-large-data-fast *)
FastListPlot[data_, opts:OptionsPattern[]] := Module[
    {interp},
    interp=Interpolation[data];
    Plot[interp[x],{x,interp[[1,1,1]],interp[[1,1,2]]},opts]
]

(* ==============================  COLORS ================================ *)

KColor[i_Integer] := Switch[ i, 
	1, RGBColor[ 54/255,  54/255,  54/255],
	2, RGBColor[  5/255, 153/255, 176/255],
	3, RGBColor[164/255, 189/255,  10/255],
	4, RGBColor[255/255, 166/255,  21/255],
	5, RGBColor[255/255,  46/255,   0/255],
	_, RGBColor[0, 0, 0]
];


(* ==============================  COLORS ================================ *)

(* function rounds plots ticks and also makes it loo like the final PDF plot *)
(* from http://mathematica.stackexchange.com/questions/58866/how-to-round-tick-line-ends-in-the-plots *)
RoundPlotTicks[plot_] := 
	First@ImportString[ExportString[plot, "PDF"], "PDF"] /. 
		JoinedCurve[{{{0, 2, 0}}}, x_, 
			CurveClosed -> {0}] :> {CapForm["Round"], JoinedCurve[{{{0, 2, 0}}}, x, CurveClosed -> {0}]};


(* helper methods *)

RectMarker[size_] := Rectangle[{{-size/2,-size/2},{size/2,size/2}}];



(* ==============================  xkcd PLOT - for fun ================================ *)

(* xkcd plots from : http://mathematica.stackexchange.com/questions/11350/xkcd-style-graphs*)
(*Thanks to belisarius& J.M.for refactoring*)

split[{a_, b_}] := If[ a == b, 
	{b}, 
	With[{n = Ceiling[3 Norm[a - b]]}, Array[{n - #, #}/n &, n].{a, b}]
];

partition[{x_, y__}] := Partition[{x, x, y}, 2, 1];

nudge[L : {a_, b_}, d_] := Mean@L + d Cross[a - b];

gap = {style__, x_BSplineCurve} :> {{White, AbsoluteThickness[10], x},
     style, AbsoluteThickness[2], x};

wiggle[pts : {{_, _} ..}, 
  d_: {-0.15, 0.15}] := ## &[#~nudge~RandomReal@d, #[[2]]] & /@ 
  partition[Join @@ split /@ partition@pts]

xkcdify[plot_Graphics] := 
 Show[FullGraphics@plot, 
    TextStyle -> {17, FontFamily -> "Humor Sans"}] /. 
   Line[pts_] :> {AbsoluteThickness[2], BSplineCurve@wiggle@pts} // 
  MapAt[# /. gap &, #, {1, 1}] &

End[ ]

EndPackage[ ]
