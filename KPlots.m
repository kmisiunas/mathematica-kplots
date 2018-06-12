(* ::Package:: *)

(* 	=== KPlots package ===

    Scientific plots by Karolis Misiunas

    Aim for consistent and clear scientific plots. Initially developed 
    to meet the standards of Keyser Lab in Cambridge, but now available
    for wider usage. 
	
	---

	Versions:
	1.0 - initial release. 
	2.0b1 - 2014-09-05 - Replaced KTicks with modified version of CustomTicks
	2.0b2 - 2014-09-05 - Added KPlot theme that captures most important style elements
	2.0b3 - 2014-09-05 - Added RoundPlotTicks for rounding tick ends
	2.0b3 - 2014-09-05 - Removed dependency on Wolframs ErrorBarPlots`
	2.0   - 2014-09-05 - Updated OListPlot and KListPlot
	2.1   - 2014-09-05 - Added CustomMarkers that survive PDF export/import
	2.2   - 2015-09-06 - Modified base of KPlotTheme to thinner lines and smaller font
	2.3   - 2015-09-06 - ErrorBars option now suppots {x,y} or ErrorBar specifications
	2.4   - 2018-04-05 - Helvetica Neue -> Helvetica for wider compatibility
	3.0   - 2018-06-11 - Introduced ListPlotErrors[] and ScienceTheme[]. 
						 Removed: KListPlot, KPlot, KPlotsTheme, xkcdify
 *)

BeginPackage["KPlots`", {"CustomTicks`", "CustomMarkers`"}]



(* Important methods *)

ListPlotErrors::usage = "ListPlotErrors[data, ErrorBars ->{...} ]"

ScienceTheme::usage = "ScienceTheme[options] returns commonly used theme. Options:
		\"Size\" -> 8.0,    (* output plot size in cm *)
		\"Labels\" -> True, (* print tick labels *)
		\"xticks\" -> Automatic, (* Y minor ticks *)
		\"yticks\" -> Automatic, (* Y major ticks *)
		\"Trim\" -> False, (* Trim the figure tightly *)
		FontSize -> 8,
		\"Scaling\" -> 0.6 (* Mathematica to Export scaling to Illustrator *)";
ScienceThemeSmall::usage = "same as ScienceTheme[], but smaller";

FastListPlot::usage = "FastListPlot[data_] will perform fast plotting for large data sets";


(* Helper methods *)

ErrorBar::usage = "Overwritten Mathematica's ErrorBar container. Sorry.";
ErrorBars::usage = "Option for providing Error Bars directly into KListPlot or OListPlot";
ErrorBarPlotStyle::usage = "Option for specifying error bar style";
ErrorBarPlotStyleFilling::usage = "Option for specifying error bar filling for KListPlot";
ErrorBarInterpolationOrder::usage = "Option for specifying interpolation order for connected error bars";
ErrorBarStyle::usage = "Option for choosing a style for drawing error bars";
ErrorBarWidth::usage = "Option for error bar hat width"

KErrorBarFunction::usage = "Custom function for drawing error bars - lines";
KColor::usage = "KColor[int] gives a colour from selection of nice shades";

RoundPlotTicks::usage = "Rounds[plot] rounds ticks and shows final look of the plot";
StabiliseForPDF::usage = "Exports and reimports PDF to stabilise the image under export.";
ToBlackBackground::usage = "converts an image plot to black background"


(*error messages*)
ListPlotErrors::imputNotMatrix = "Error: input should be a vector or a matrix";
ListPlotErrors::oddInputShape = "Warning: input shape is not fully supported: `1`";
ListPlotErrors::errorsNotMatrix = "Error: ErrorBars should be a vector or a matrix";
ListPlotErrors::errSameLength = "Error: please supply the same number of errors (N=`1`) as data points(N=`2`)";


(* deprecated: remove at some point *)

ThesisTheme::usage = "Special theme for thesis. Scale it by 60% once imported to illustrator ";
ThesisThemeSmall::usage = "Special theme for thesis. Scale it by 60% once imported to illustrator. Fits 2 fig in 13cm";
OListPlot::usage = "OListPlot[data_, ErrorBars->..., opt->...] list plot for data in Origin style. Supports all sorts of markers and standard error-bars";
OListPlot::deprecated = "OListPlot[] will be deprecated; please use ListPlotErrors[]."

(* Implementations *)
Begin["`Private`"]


(* ==============================  BEAUTIFUL PLOT THEME ================================ *)

SetOptions[ LinTicks,
	MinorTickLength -> 0.009,
	MajorTickLength -> 0.015,
	NumberOfMinorTicks -> 1
];


(*designed to be imported to Illustrator and scaled by 60% *)
ThesisTheme := {
  Frame -> True,
  FrameStyle -> Directive[Black, 8/0.6, FontFamily -> "Helvetica", AbsoluteThickness[1.0]],
  FrameTicks ->
      {{LinTicks, LinTicks[#1, #2, ShowTickLabels -> False] &},
        {LinTicks, LinTicks[#1, #2, ShowTickLabels -> False] &}},
  GridLines -> None,
  ImageSize -> 350
};

(*designed to be imported to Illustrator and scaled by 60% *)
ThesisThemeSmall := {
  Frame -> True,
  FrameStyle -> Directive[Black, 8/0.6, FontFamily -> "Helvetica", AbsoluteThickness[1.0]],
  FrameTicks ->
      {{LinTicks, LinTicks[#1, #2, ShowTickLabels -> False] &},
        {LinTicks, LinTicks[#1, #2, ShowTickLabels -> False] &}},
  GridLines -> None,
  ImageSize -> 270
};

(*todo ???*)
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
];

(*Helper function for interpretting error bars*)
InterpretErrorBarList[list_]:= Print["Could not interpret error bar: ",list];
InterpretErrorBarList[list : {ErrorBar[_]...}] := list;
InterpretErrorBarList[list : {ErrorBar[_,_]...}] := list;
InterpretErrorBarList[list_List/;VectorQ[list,NumberQ]] := ErrorBar /@ list;
InterpretErrorBarList[list : {{_, _}...}] := ErrorBar[ #[[1]] , #[[2]] ] &/@ list;



(* ==============================  ListPlotErrors[] ================================ *)

Options[ListPlotErrors] = Normal@Join[
	Association@Options[ListPlot], 
	Association@{
		MaxPlotPoints->Infinity,
		PlotMarkers -> {Graphics`PlotMarkers[][[2, 1]], 13} , 
		Mesh -> All,
		Joined -> False,
		PlotStyle -> Directive[Black],
		ErrorBars -> None,
		"Errors" -> None,
		ErrorBarWidth -> 1.4,
		ErrorBarStyle -> AbsoluteThickness[1.2]
	}	
];


(* Main plotting method for everything *)
ListPlotErrors[input_, opts: OptionsPattern[ListPlotErrors]] := Module[
	{
		data, errors,
		fullOpts = Join[ Association@Options[ListPlotErrors], Association@{opts} ],
		errorsPlot, dataPlot
	},

	{data, errors} = helperPrepareInput[input, opts];	
	(* Print[data]; Print[errors]; *)

	dataPlot = ListPlot[
		FilterWithinPlotRegion[ data , Lookup[fullOpts, PlotRange, None] ], 
		Evaluate@FilterRules[ Normal@fullOpts, Options[ListPlot]]
	];

 	errorsPlot = If[ errors === None, {} ,
		errorData = Transpose@{ data , InterpretErrorBarList[errors]};
		Graphics[ Flatten[ Join[ 
			{OptionValue[PlotStyle]},
			{OptionValue[ErrorBarStyle]},
  			KErrorBarFunction[OptionValue[ErrorBarWidth]][#[[1]], #[[2]]] & /@ errorData
  		] , 1]   ]
	]; 
  
	Show[
        (* ListPlot[{}, Evaluate@FilterRules[ Normal@fullOpts, Options[Graphics]]], *)
        errorsPlot,
		dataPlot,
		Evaluate@FilterRules[ Normal@fullOpts, Options[ListPlot]]
	]
];


(*function sorts data and and errors. Returns: {data, errors} *)
helperPrepareInput[input_, opts: OptionsPattern[ListPlotErrors]] := Module[
	{
		data = Normal[input],
		errors = None,
		points
	},

	If[ !(MatrixQ[data] || VectorQ[data]), Message[ListPlotErrors::imputNotMatrix]; Abort[]];
	If[ MatrixQ[data] && Dimensions[data][[2]] > 4 , Message[ListPlotErrors::oddInputShape, Dimensions[data]]];
	points = If[ MatrixQ[data], data[[All, {1,2}]] , data];

	errors = If[errors === None, OptionValue[ ErrorBars], errors];
	errors = If[errors === None, OptionValue["Errors"], errors];
	errors = If[errors === None, getErrorsFromInput[data],     errors];
	If[ errors =!= None && !(MatrixQ[errors] || VectorQ[errors]), 
		Message[ListPlotErrors::errorsNotMatrix]; Abort[]];

	If[errors =!= None && Length[points] =!= Length[errors],
		Message[ListPlotErrors::errSameLength, Length[errors], Length[points]]; Abort[] ];

	Return[	{points, errors} ]
];


(* returns errors if they were passed with data and None otherwise *)
getErrorsFromInput[data_]:= Which[ 
	Length[ Dimensions[data] ] == 2 && Dimensions[data][[2]] == 3 ,
		(* if there are [[All,3]] interpret them as errors *)
		data[[All, 3]],
	Length[ Dimensions[data] ] == 2 &&  Dimensions[data][[2]] == 4,
		(* if there are [[All,4]] interpret them as errors *)
		data[[All, {3,4}]],
    True,(*else return None*)
		None
];



(* ==============================  ScienceTheme  ================================ *)

Options[ScienceTheme] = {
		"Size" -> 8.0,
		"Labels" -> True,
		"xticks" -> Automatic,
		"yticks" -> Automatic,
		"Trim" -> False,
		FontSize -> 8,
		"Scaling" -> 0.6 (*Mathematica to Export scaling*)
	};


(* make it viewable in Mathematica and print in CM *)
ScienceTheme[opts: OptionsPattern[ScienceTheme]] := {
  Frame -> True,
  FrameStyle -> Directive[  Black, 
  							FontSize -> OptionValue[FontSize]/OptionValue["Scaling"], 
  							FontFamily -> "Helvetica", 
  							AbsoluteThickness[1.0]
  						 ],
  FrameTicks -> generateTicks[OptionValue["Size"] , OptionValue["Labels"], OptionValue["xticks"], OptionValue["yticks"]],
  ImagePadding -> If[ OptionValue["Trim"],
  	{{1 , 1}, {1, 1}},
  	{{66*OptionValue[FontSize]/8 , 10}, {40*OptionValue[FontSize]/8, 7}}
  ],
  Axes -> False,
  GridLines -> None,
  ImageSize -> 380*OptionValue["Size"]/8.0
};


ScienceThemeSmall[opts: OptionsPattern[ScienceTheme]] := ScienceTheme["Size" -> 4.5, FontSize -> 7, opts];


generateTicks[size_, labels_, Automatic, Automatic] := 
	{{LinTicks[#1, #2, ShowTickLabels -> labels, TickLengthScale -> 0.9/size*8.0] &  ,
	  LinTicks[#1, #2, ShowTickLabels -> False, TickLengthScale ->  0.9/size*8.0] & },
     {LinTicks[#1, #2, ShowTickLabels -> labels, TickLengthScale -> 0.9/size*8.0] &  ,
	  LinTicks[#1, #2, ShowTickLabels -> False, TickLengthScale ->  0.9/size*8.0] & } 
};

generateTicks[size_, labels_, xticks_, yticks_] := 
	{{LinTicks[yticks, ShowTickLabels -> labels, TickLengthScale -> 0.9/size*8.0] &  ,
	  LinTicks[yticks, ShowTickLabels -> False, TickLengthScale ->  0.9/size*8.0] & },
     {LinTicks[xticks, ShowTickLabels -> labels, TickLengthScale -> 0.9/size*8.0] &  ,
	  LinTicks[xticks, ShowTickLabels -> False, TickLengthScale ->  0.9/size*8.0] & } 
};

generateTicks[size_, labels_, Automatic, yticks_] := 
	{{LinTicks[yticks, ShowTickLabels -> labels, TickLengthScale -> 0.9/size*8.0] &  ,
	  LinTicks[yticks, ShowTickLabels -> False, TickLengthScale ->  0.9/size*8.0] & },
     {LinTicks[#1, #2, ShowTickLabels -> labels, TickLengthScale -> 0.9/size*8.0] &  ,
	  LinTicks[#1, #2, ShowTickLabels -> False, TickLengthScale ->  0.9/size*8.0] & } 
};

generateTicks[size_, labels_, xticks_, Automatic] := 
	{{LinTicks[#1, #2, ShowTickLabels -> labels, TickLengthScale -> 0.9/size*8.0] &  ,
	  LinTicks[#1, #2, ShowTickLabels -> False, TickLengthScale ->  0.9/size*8.0] & },
     {LinTicks[xticks, ShowTickLabels -> labels, TickLengthScale -> 0.9/size*8.0] &  ,
	  LinTicks[xticks, ShowTickLabels -> False, TickLengthScale ->  0.9/size*8.0] & } 
};




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


(* ==============================  PDF functions ================================ *)

(* function rounds plots ticks and also makes it loo like the final PDF plot *)
(* from http://mathematica.stackexchange.com/questions/58866/how-to-round-tick-line-ends-in-the-plots *)

RoundPlotTicks[plot_] := 
	First@ImportString[ExportString[plot, "PDF"], "PDF"] /. 
		JoinedCurve[{{{0, 2, 0}}}, x_, 
			CurveClosed -> {0}] :> {CapForm["Round"], JoinedCurve[{{{0, 2, 0}}}, x, CurveClosed -> {0}]};

(*exports and reimports - makes it stable under illustrator importing Stabilise *)
StabiliseForPDF[plot_] := First@ImportString[ExportString[plot, "PDF"], "PDF"];


(* ==============================  ??? ================================ *)

RectMarker[size_] := Rectangle[{{-size/2,-size/2},{size/2,size/2}}];

(* remove elements outside plotting range *)
FilterWithinPlotRegion[data_, _ ] = data;
FilterWithinPlotRegion[data_?MatrixQ, region_] := Block[ 
	{ within = IsWithinPlotRegion[region] },
  	Select[data, within[#[[1]], #[[2]]] &]
];

IsWithinPlotRegion[ _ ] := Function[{x, y}, True ];
IsWithinPlotRegion[{{xMin_?NumericQ, xMax_?NumericQ}, {yMin_?NumericQ, yMax_?NumericQ}}, padding_?NumericQ] := Module[
	{
		padSizeX = If[ Infinity === Max@Abs@{xMin, xMax} , 0, (xMax - xMin)*padding ], 
		padSizeY = If[ Infinity === Max@Abs@{yMin, yMax} , 0, (yMax - yMin)*padding ]
	},
	Function[{x, y}, (yMin+padSizeY <= y <= yMax-padSizeY) && (xMin+padSizeX <= x <= xMax-padSizeX)]
]
IsWithinPlotRegion[{{xMin_?NumericQ, xMax_?NumericQ}, {yMin_?NumericQ, yMax_?NumericQ}}] :=
	IsWithinPlotRegion[{{xMin, xMax}, {yMin, yMax}}, 0.01]; 
IsWithinPlotRegion[ {yMin_?NumericQ, yMax_?NumericQ}] := 
	IsWithinPlotRegion[{{-Infinity, Infinity}, {yMin, yMax}}]


(* =========================== BACKWARDS COMPATIBILITY ============================= *)

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

(*deprecated*)
OListPlot[data_, opts: OptionsPattern[OListPlot]] := Module[
	{
		fullOpts = Join[ Association@Options[OListPlot], Association@{opts} ],
		errorsPlot, dataPlot
	},
	Message[OListPlot::deprecated];

	If[ !MatrixQ@data, Print["KPlots error: OListPlot data must be a matrix"]];
	dataPlot = ListPlot[
		FilterWithinPlotRegion[ data , Lookup[fullOpts, PlotRange, None] ], 
		Evaluate@FilterRules[ Normal@fullOpts, Options[ListPlot]]
	];

	errorsPlot = If[ ! ListQ @ fullOpts[ErrorBars] , {} ,
		If[Length @ fullOpts[ErrorBars] != Length @ data, Message[KPlots::errSameLength]];
		errorData = Transpose@{ data , InterpretErrorBarList[fullOpts[ErrorBars]]};
		Graphics[ Flatten[ Join[ 
			{fullOpts[PlotStyle]},
			{fullOpts[ErrorBarStyle]},
  			KErrorBarFunction[fullOpts[ErrorBarWidth]][#[[1]], #[[2]]] & /@ errorData
  		] , 1]   ]
	];
  
	Show[
		dataPlot,
		errorsPlot,
		dataPlot
		(* Evaluate@FilterRules[ Normal@fullOpts, Options[Graphics]] (*does not work with PlotTheme*) *)
	]
];


End[ ]


EndPackage[ ]
