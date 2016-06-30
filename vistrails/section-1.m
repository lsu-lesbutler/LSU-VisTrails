(* ::Package:: *)

(* ::Section:: *)
(*Step 1:  Initialization*)


(* ::Subsection:: *)
(*1.a Image size, Paths*)


(* ::Input:: *)
(**)


ClearAll["Global`*"]

(* Get pathRoot from commandline *)
eval = ToExpression[##]&;
ARGV = Drop[$CommandLine, 3]; 
Print[ARGV]
(* Print[Length[ARGV]] *)
pathRoot = ARGV[[1]];


(* ::Input:: *)
(**)


NX =256; NY=256; 


(* ::Input:: *)
(**)


(*NotebookDirectory[]*)
$OperatingSystem
$HomeDirectory


(* ::Input:: *)
(**)


DirectoryQ[pathRawData =  pathRoot<>"raw_bullet_images/"]
DirectoryQ[pathFigures =  pathRoot<>"figures/"]
DirectoryQ[pathAttenuation =  pathRoot<>"attenuation/"]
DirectoryQ[pathSinograms =  pathRoot<>"sinograms/"]
DirectoryQ[pathSlices=  pathRoot<>"slices/"]
DirectoryQ[pathVolumes=  pathRoot<>"volumes/"]
DirectoryQ[pathFigures =  pathRoot<>"figures/"]


(* ::Subsection::Closed:: *)
(*1.b Define image processing functions*)


(* ::Text:: *)
(*funcFilterRawImages[raw_]*)
(*In a high radiation environment, even if shielded from the main radiation beam, a detector can be damaged at the the pixel level. ONe strategy is to replace the bad pixel with an average of its neighbors.  *)


(* ::Input:: *)
(**)


funcFilterRawImages[raw_]:=Module[{anyZeros,filtered,r,c,medianFiltered},
filtered=raw;
anyZeros= Position[raw, p_?(#<=0 &)];
medianFiltered = MedianFilter[raw,1];
If[Length[anyZeros]>0,Module[{}, 
For[i=1,i<=Length[anyZeros],i++,
{r,c}=anyZeros[[i]];
filtered[[r,c]] = medianFiltered[[r,c]];  ]]];
filtered];


(* ::Input:: *)
(**)


funcFilterAttenuation[data_,upperLimit_]:=Module[{anyZeros,anyZingers,filtered,r,c,medianFiltered},
filtered=data;
anyZeros= Position[data, p_?(#<=0 &)];
medianFiltered = MedianFilter[data,1];
If[Length[anyZeros]>0,Module[{}, 
For[i=1,i<=Length[anyZeros],i++,
{r,c}=anyZeros[[i]];
filtered[[r,c]] = medianFiltered[[r,c]];  ]]];
anyZingers= Position[data, p_?(#>upperLimit &)];
If[Length[anyZingers]>0,Module[{}, 
For[i=1,i<=Length[anyZingers],i++,
{r,c}=anyZingers[[i]];
filtered[[r,c]] = medianFiltered[[r,c]];  ]]];
filtered];


(* ::Input:: *)
(**)


funcShiftSinogram[sinogram_,shift_,numberColumns_,numberOfAngles_]:=Module[{rInitial,cInitial,rFinal,cFinal,sinoShifted},
{rInitial,cInitial}=Dimensions[sinogram];
If[shift>0,
sinoShifted = Transpose[PadRight[Take[Transpose[sinogram],All,{shift,numberColumns}],{numberOfAngles,numberColumns},0] ]];
If[shift<0,
sinoShifted = Transpose[PadLeft[Take[Transpose[sinogram],All,{1,numberColumns+shift}],{numberOfAngles,numberColumns},0] ]];
If[shift==0,sinoShifted =sinogram ];
{rFinal,cFinal}=Dimensions[sinoShifted];
error=If[{rInitial,cInitial}=={rFinal,cFinal},0,1];
sinoShifted  ];


(* ::Input:: *)
(**)


funcStreakRemovalFFT[sinogram_,bandWidth_]:=Module[{numberRays,numberOfAngles,streaks,streaksHighPassFilter},
{numberRays,numberOfAngles}=Dimensions[sinogram];
streaks = Total[sinogram,{2}]/numberOfAngles;
streaksHighPassFilter = HighpassFilter[streaks,bandWidth];
sinogram-streaksHighPassFilter ];


(* ::Input:: *)
(**)


funcUseAirRegionForOffsetSinogram[data_,airRegionWidth_]:=Module[{numberRays,numberOfAngles,airTop,airBottom,tempSinogram,indexAngle,sinogram},
sinogram = data;
{numberRays,numberOfAngles}=Dimensions[data];
For[indexAngle =1, indexAngle<=numberOfAngles,indexAngle++,
airTop = Median[Flatten[Take[sinogram, {1,airRegionWidth},{indexAngle}]]];
airBottom =  Median[Flatten[Take[sinogram, {numberRays-airRegionWidth+1,numberRays},{indexAngle}]]];
sinogram[[All,indexAngle]]=sinogram[[All,indexAngle]]-Mean[{airTop,airBottom}];
];
sinogram];


(* ::Subsection:: *)
(*1.c DumpSave*)


(* ::Input:: *)
(**)


DumpSave[pathRoot<>"bullet_jge_run_step_1.mx",{NX, NY, pathAttenuation,pathFigures,pathRawData,pathSinograms,pathSlices,pathVolumes, funcFilterRawImages,funcFilterAttenuation,funcShiftSinogram,funcStreakRemovalFFT,funcUseAirRegionForOffsetSinogram }]
ClearAll["Global`*"]
