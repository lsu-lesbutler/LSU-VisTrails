(* Get pathRoot from commandline *)
eval = ToExpression[##]&;
ARGV = Drop[$CommandLine, 3]; 
Print[ARGV]
(* Print[Length[ARGV]] *)
pathRoot = ARGV[[1]];
bestShift = eval[ARGV[[2]]]
sliceStart = eval[ARGV[[3]]]
sliceStep = eval[ARGV[[4]]]


Get[pathRoot<>"bullet_jge_run_step_1.mx"]
Get[pathRoot<>"bullet_jge_run_step_2.mx"]
Get[pathRoot<>"bullet_jge_run_step_3.mx"]
Get[pathRoot<>"bullet_jge_run_step_4.mx"]

(* change pathFigures to stage folder for job download*)
If[DirectoryQ["figures"], Null, CreateDirectory["figures"]];
pathFigures = "figures/"


(* split of section 4 ::Subsection:: section 4.a, 4.c to 4.d + dumpsave *)
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


(*4.c  Shift all sinograms and plot a few reconstructions*)


(* ::Input:: *)
(*bestShift = estimatedCenteringShift;*)

(* ::Input:: *)
Dimensions[allSinograms]
For[indexSlice =1, indexSlice<=NX,indexSlice++,
sinogram= allSinograms[[indexSlice,All,All]];
sinogram=funcStreakRemovalFFT[sinogram,0.5];
sinogram= funcUseAirRegionForOffsetSinogram[sinogram,5];
sinogram =funcShiftSinogram[sinogram,bestShift ,NY,NZ];
allSinograms[[indexSlice,All,All]]=sinogram;
];


(* ::Input:: *)
gSeveralSlices= ParallelTable[Module[{},
sinogram= allSinograms[[indexSlice,All,All]];
imageSlice = InverseRadon[Image[sinogram,"Real"]];
g1  = ImageAdjust[ ImageResize[imageSlice,400]];


g=Show[g1,Epilog->Inset[Text[Style["slice = "<>ToString[indexSlice],20,Background->White]],Scaled[{0.8,0.8}]]];
(* export separate images *)
Export[pathFigures<>"reconstruction_"<>IntegerString[indexSlice,10,3]<>".png", g, "PNG"];

],{indexSlice,Range[sliceStart,NX,sliceStep]}];
(*
gAll = GraphicsColumn[gSeveralSlices];
Print["Export several_recon_slices.png to job stage figure directory for download"]
Export[pathFigures<>"several_recon_slices.png",gAll,"png"];
*)
(* ::Subsection:: *)
(*4.d  Save sinograms as FITS files*)

(* ::Input:: *)
Table[Module[{},
sinogram= allSinograms[[indexSlice,All,All]];filenameFITS = pathSinograms<>"sinogram_#"<>IntegerString[indexSlice,10,4]<>".fits";
Export[filenameFITS,sinogram]; 
],{indexSlice,NX}];


(* ::Subsection:: *)
(*DumpSave*)
DumpSave[pathRoot<>"bullet_jge_run_step_4.mx",{bestShift,estimatedCenteringShift}];
ClearAll["Global`*"]