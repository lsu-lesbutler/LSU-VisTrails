(* Get pathRoot from commandline *)
eval = ToExpression[##]&;
ARGV = Drop[$CommandLine, 3]; 
Print[ARGV]
(* Print[Length[ARGV]] *)
pathRoot = ARGV[[1]];
shiftRange = eval[ARGV[[2]]]

Get[pathRoot<>"bullet_jge_run_step_1.mx"]
Get[pathRoot<>"bullet_jge_run_step_2.mx"]
Get[pathRoot<>"bullet_jge_run_step_3.mx"]

(* change pathFigures to stage folder for job download*)
If[DirectoryQ["figures"], Null, CreateDirectory["figures"]];
pathFigures = "figures/"


(* split of section 4 ::Subsection:: section 4.a to 4.b + dumpsave *)
(*4.a  Read all attenuation FITS and make allSinograms *)


(* ::Input:: *)
filenameFITS = FileNames["attenuation_#*.fits",pathAttenuation];
Length[filenameFITS]


(* ::Input:: *)
NZ = Length[filenameFITS]-1;
allSinograms = ConstantArray[0,{NY,NX,NZ}];
Dimensions[allSinograms]
For[projection =1,projection<= NZ,projection++,
attenuation = Import[filenameFITS[[projection]],"RawData" ][[1]];
allSinograms[[All,All,projection]]=attenuation;
];
Dimensions[allSinograms]


(* ::Input:: *)
indexSlice = 165;
gSino=ArrayPlot[ allSinograms[[indexSlice,All,All]], 
PlotRange->{All,All,grayScalePlotLimits},ColorFunction->"TemperatureMap",ClippingStyle->{Black,White},Frame->True,
FrameLabel->{{"\[Theta], degrees","slice = "<>ToString[indexSlice]},{"columns",""}},
PlotLegends->{Placed[BarLegend[{"TemperatureMap",grayScalePlotLimits},LegendMarkerSize->150],Right]}];
lineProbe =allSinograms[[indexSlice,{30,120},All]];
gLineProbe=ListPlot[lineProbe, PlotRange->grayScalePlotLimits, AxesOrigin->{0,0},
Frame->True,FrameLabel->{{"attenuation",""},{"\[Theta], degrees","columns 20, 120"}},
GridLines->Automatic
];
gAll = GraphicsRow[{gSino,gLineProbe},ImageSize->700];
Print["Export sino_lineprobe.png to job stage directory for download"]
Export[pathFigures<>"sino_lineprobe.png",gAll,"png"];

(* ::Subsection::Closed:: *)
(*4.b. Find best center of rotation*)


(* ::Input:: *)
index0deg=First[Flatten[Position[angleList,p_?(#>=0&)]]];
index180deg=Last[Flatten[Position[angleList,p_?(#<=180&)]]];
filenameFITS = FileNames["attenuation_#*.fits",pathAttenuation];
attenuation0Deg = ImageAdjust[Image[Import[filenameFITS[[index0deg]],"RawData" ][[1]],"Real"]];
attenuation180Deg = ImageAdjust[Image[Import[filenameFITS[[index180deg]],"RawData" ][[1]],"Real"]];
transformationFunction = FindGeometricTransform[attenuation0Deg,ImageReflect[attenuation180Deg,Left->Right],TransformationClass->"Translation"]
estimatedCenteringShift = Round[transformationFunction[[2,1,1,3]]/2]
Print["estimatedCenteringShift="<>ToString[estimatedCenteringShift]]

(* ::Input:: *)
sinogram= allSinograms[[indexSlice,All,All]];

Print["export separate shift test image for download"]

gCenteringTest = Table[Module[{},
imageSlice = InverseRadon[Image[funcShiftSinogram[sinogram,shift,NY,NZ],"Real"]];
g1=ImageAdjust[ ImageResize[imageSlice,400]];

g = Show[g1,Epilog->Inset[Text[Style["shift = "<>ToString[shift],20,Background->White]],Scaled[{0.8,0.8}]]];

Export[pathFigures<>"shiftcentertest_"<>IntegerString[shift,10,3]<>".png", g, "PNG"]

],{shift,estimatedCenteringShift+Range[-shiftRange,shiftRange,1]}];

(*
gAll = GraphicsColumn[gCenteringTest];

Print["export overall shiftcentertest.png for download"]
Export[pathFigures<>"shiftcentertest.png", gAll, "PNG"]
*)

(*dumpsave*)
DumpSave[pathRoot<>"bullet_jge_run_step_4.mx",{estimatedCenteringShift}];
ClearAll["Global`*"]