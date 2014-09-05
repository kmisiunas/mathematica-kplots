(* ::Package:: *)

(* 	Package containing custom Markers that survive PDF export/import

  Author: Karolis Misiunas k.misiunas@gmail.com

	Versions:
	1.0 - 2014-09-06 -initial release. 
 *)

BeginPackage["CustomMarkers`"]

CustomMarkers::usage = "CustomMarkers[id, size_Integer: 10, color_: Black] gives a marker. See them all by calling CustomMarkers[].";


(* Implementations *)
Begin["`Private`"]


triangleUp[size_Integer: 10, color_: Black] := 
 Graphics[{color, Polygon[{{1, 0}, {0, Sqrt[3]}, {-1, 0}}]}, 
  ImageSize -> size]

triangleDown[size_Integer: 10, color_: Black] := 
 Graphics[{color, 
   Rotate[Polygon[{{1, 0}, {0, Sqrt[3]}, {-1, 0}}], 180 Degree]}, 
  ImageSize -> size]

square[size_Integer: 10, color_: Black] := 
 Graphics[{color, Rectangle[]}, ImageSize -> size]

disk[size_Integer: 10, color_: Black] := 
 Graphics[{color, Disk[]}, ImageSize -> size]

rhomb[size_Integer: 10, color_: Black] := 
 Graphics[{color, Rotate[Rectangle[], 45 Degree]}, ImageSize -> size]


triangleUpEdge[size_Integer: 10, color_: Black] := 
 Graphics[{EdgeForm[Directive[AbsoluteThickness[1], color]], White, 
   Polygon[{{1, 0}, {0, Sqrt[3]}, {-1, 0}}]}, ImageSize -> size]

triangleDownEdge[size_Integer: 10, color_: Black] := 
 Graphics[{EdgeForm[Directive[AbsoluteThickness[1], color]], White, 
   Rotate[Polygon[{{1, 0}, {0, Sqrt[3]}, {-1, 0}}], 180 Degree]}, 
  ImageSize -> size]

squareEdge[size_Integer: 10, color_: Black] := 
 Graphics[{EdgeForm[Directive[AbsoluteThickness[1], color]], White, 
   Rectangle[]}, ImageSize -> size]

diskEdge[size_Integer: 10, color_: Black] := 
 Graphics[{EdgeForm[Directive[AbsoluteThickness[1], color]], White, 
   Disk[]}, ImageSize -> size]

rhombEdge[size_Integer: 10, color_: Black] := 
 Graphics[{EdgeForm[Directive[AbsoluteThickness[1], color]], White, 
   Rotate[Rectangle[], 45 Degree]}, ImageSize -> size]


CustomMarkers[id_Integer, size_Integer: 10, color_: Black] := 
  Switch[ id,
    1, square,
    2, squareEdge,
    3, disk,
    4, diskEdge,
    5, rhomb, 
    6, rhombEdge,
    7, triangleUp,
    8, triangleUpEdge,
    9, triangleDown,
    10, triangleDownEdge,
    _, square][size, color]; 

CustomMarkers[] := <|
	1-> square[],
    2-> squareEdge[],
    3-> disk[],
    4-> diskEdge[],
    5-> rhomb[], 
    6-> rhombEdge[],
    7-> triangleUp[],
    8-> triangleUpEdge[],
    9-> triangleDown[],
    10-> triangleDownEdge[]
   |>


End[ ]

EndPackage[ ]
