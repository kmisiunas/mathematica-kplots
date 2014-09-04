(* ::Package:: *)

(* 	Plots that meet Keyser Lab standarts for plots.
	---
	There are two types:
	K* - ones favored by Karolis 
	O* - ones mimic OriginPro style, favored by Ulrich

	Vesrions:
	1.0 - intial realease. 
	2.0b1 - 2014-09-05 - Replaced KTicks with modified version of CustomTicks
 *)

BeginPackage["KPlots`", {"ErrorBarPlots`", "CustomTicks`"}]

KListPlot::usage = 
	"KListPlot[data_, Errors->..., opt->...] list plot for data"

OListPlot::usage = 
	"OListPlot[data_, Errors->..., opt->...] list plot for data in Origin style!"

KPlotStyle::usage = 
	"gives the style that makes plot look in Karolis style"

OPlotStyle::usage = 
	"gives the style that makes plot look in OriginPro style"

FastListPlot::usage = 
	"FastListPlot[data_] will perform fast ploting for large data sets"

KErrorBarFunction::usage = 
	"Custom function for drawing error bars - lines"

KColor::usage = 
	"KColor[int] gives a color from selecton of nice shades"

xkcdify::usage = 
	"converts plot into xkcd styled plot: "

(*error messages*)

KPlots::errSameLength = "plotted data must be the same length as error array"


Options[KTicks] = {Size -> 0.015, Labels -> True, Extra -> {}, 
   Period -> Automatic, StartAt -> Automatic, At -> Automatic};



Begin["`Private`"]

(* Implementations *)

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


KTicks[OptionsPattern[]][min_, max_] := 
	Module[{size, startAt, period, at, mid},
		size = OptionValue[Size];
		period = If[ OptionValue[Period] === Automatic,
			(max - min)/4,
			OptionValue[Period]
		];
		startAt = If[OptionValue[StartAt] === Automatic, 
			Ceiling[min, period],
			OptionValue[StartAt]
		];
		at = If[ OptionValue[At] === Automatic,
			Range[startAt, Floor[max - startAt, period] + startAt, period],
			OptionValue[At]
		];
		mid = (at[[2 ;;]] + at[[;; -2]])/2;
		Join[
			Table[{i, If[OptionValue[Labels], i], {size, 0}}, {i, at}],
			Table[{j, Null, {size*0.6, 0}}, {j, mid}],
			{#, #, {size, 0}} & /@ OptionValue[Extra]
		]
]

(* KErrorBarFunction := Function[{coords, errs}, {Opacity[0.2], 
    Rectangle[coords + {errs[[1, 1]], errs[[2, 1]]}, 
     coords + {errs[[1, 2]], errs[[2, 2]]}]}] *)
KErrorBarFunction[size_] := 
Function[ {coords, errs} ,
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


KPlotStyle := {
	"Errors" -> None,
	"ErrorPlotStyle" -> Directive[Orange, Thin],
	"ErrorFillingStyle" -> Directive[Orange, Opacity[0.1]],
	"ErrorInterpolationOrder" -> 2,
	"ShadowFrom" -> None,
	PlotStyle -> Directive[Opacity[1]],
	PlotMarkers -> {Graphics`PlotMarkers[][[2, 1]], 14},
	FrameStyle -> Directive[FontSize ->13],
	Frame -> True,
	MeshShading -> {Red,Blue}
}

KListPlot[data_, opt : OptionsPattern[{KPlotStyle,ListPlot}]] := Module[
	{errorData, errorsPlot, shadowRect},
  
	errorsPlot = If[ ! VectorQ @ OptionValue["Errors"] , {},
		If[Length @ OptionValue["Errors"] != Length @ data, Message[KPlots::errSameLength]];
		errorData = Transpose @ {data[[;; , 1]] , data[[;; , 2]] - OptionValue["Errors"], data[[;; , 2]] + OptionValue["Errors"]} ;
		ListPlot[{errorData[[;; , {1, 2}]], errorData[[;; , {1, 3}]]},
			Filling -> {1 -> {2}},
			Joined -> True ,
			InterpolationOrder -> OptionValue["ErrorInterpolationOrder"],
			FillingStyle -> OptionValue["ErrorFillingStyle"],
			PlotStyle -> OptionValue["ErrorPlotStyle"]
		]
	];
  
	shadowRect = If[ ! NumberQ @ OptionValue["ShadowFrom"] , {},
		Directive[ {rangeY, rangeX, errMax},
			rangeX = {Min@data[[;; , 1]] , Max@data[[;; , 1]]} ;
			rangeY = {Min@data[[;; , 2]] , Max@data[[;; , 2]]} ;
			errMax = 1.02 Max @ {Max@error , 0} ;
   		If[ rangeX[[2]] <= OptionValue["ShadowFrom"] , {},
		    Graphics[{
		      White, Opacity[0.7],
		      Rectangle[{OptionValue["ShadowFrom"], 
		        rangeY[[1]] - errMax}, {rangeX[[2]]*1.01, 
		        rangeY[[2]] + errMax}]
		    }] 
	    ]
	  ]
  ]; 

  Show[
  	errorsPlot,
   
		ListPlot[data, 
			Evaluate@FilterRules[ {opt}, Options[ListPlot]],
			Evaluate@FilterRules[ KPlotStyle, Options[ListPlot]],
			Joined -> True,
			Mesh -> All
		],

		shadowRect,

		Evaluate@FilterRules[ {opt}, Options[Graphics]],
		Evaluate@FilterRules[ KPlotStyle, Options[Graphics]]
  ]
]


(* ---- Origin Pro style ---- *)

OPlotStyle := {
	"Errors" -> None,
	"ShadowFrom" -> None,
	PlotStyle -> Directive[Opacity[1], Black],
	PlotMarkers -> {Graphics`PlotMarkers[][[2, 1]], 13}, 
	FrameStyle -> Directive[14, Black, Thickness[0.0035], FontFamily -> "Helvetica"],
	Mesh -> All,
	Joined -> False,
	Frame -> True,
	"ErrorBarWidth" -> 1.2,
	"ErrorBarStyle" -> Thickness[0.0045]
}

OListPlot[data_, opt : OptionsPattern[{OPlotStyle,ListPlot}]] := Module[
	{errorData, errorsPlot, dataPlot},

	If[ !MatrixQ@data, Print["KPlots error: OListPlot data must be a matrix"]];
	dataPlot = ListPlot[
		data ,
		Evaluate@FilterRules[ {opt}, Options[ListPlot]],
		Evaluate@FilterRules[ OPlotStyle, Options[ListPlot]]
	];

	errorsPlot = If[ ! VectorQ @ OptionValue["Errors"] , {} ,
		If[Length @ OptionValue["Errors"] != Length @ data, Message[KPlots::errSameLength]];
		errorData = Table[{ data[[i]] , ErrorBar[ OptionValue["Errors"][[i]] ]}, {i, Length[data]}];
		Graphics[ Flatten[ Join[ 
			{PlotStyle /. {opt}/. OPlotStyle}  ,
			{"ErrorBarStyle" /. {opt} /. OPlotStyle}  ,
  		KErrorBarFunction[OptionValue["ErrorBarWidth"]][#[[1]], #[[2]]] & /@ errorData
  	] , 1]]
	];
  
	Show[
		dataPlot,
		errorsPlot,

		Evaluate@FilterRules[ {opt}, Options[Graphics]],
		Evaluate@FilterRules[ OPlotStyle, Options[Graphics]]
	]
]



(* from: http://mathematica.stackexchange.com/questions/140/listplot-plotting-large-data-fast *)
FastListPlot[data_,opts:OptionsPattern[]] := Module[
    {interp},

    interp=Interpolation[data];
    Plot[interp[x],{x,interp[[1,1,1]],interp[[1,1,2]]},opts]
]

KColor[i_Integer] := {
	RGBColor[ 54/255,  54/255, 54/255],
	RGBColor[  5/255, 153/255, 176/255],
	RGBColor[164/255, 189/255, 10/255],
	RGBColor[255/255, 166/255, 21/255],
	RGBColor[255/255, 46/255, 0/255]
}[[i]]


(* helper methods *)

RectMarker[size_] := Rectangle[{{-size/2,-size/2},{size/2,size/2}}]

(* xkcd plots from : http://mathematica.stackexchange.com/questions/11350/xkcd-style-graphs*)

(*Thanks to belisarius& J.M.for refactoring*)
split[{a_, b_}] := 
 If[a == b, {b}, 
  With[{n = Ceiling[3 Norm[a - b]]}, Array[{n - #, #}/n &, n].{a, b}]]

partition[{x_, y__}] := Partition[{x, x, y}, 2, 1]

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
