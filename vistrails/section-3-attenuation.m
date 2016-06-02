(* Get pathRoot from commandline *)
eval = ToExpression[##]&;
ARGV = Drop[$CommandLine, 3]; 
Print[ARGV]
(* Print[Length[ARGV]] *)
pathRoot = ARGV[[1]];


Get[pathRoot<>"bullet_jge_run_step_1.mx"]
Get[pathRoot<>"bullet_jge_run_step_2.mx"]
Get[pathRoot<>"bullet_jge_run_step_3.mx"]

(* change pathFigures to stage folder for job download*)
If[DirectoryQ["figures"], Null, CreateDirectory["figures"]];
pathFigures = "figures/"


(* split of section 3 ::Subsection:: 3.e to 3.f *)

(*3.e Replace bad FITS files at #22 and #46 with the average of the neighboring data files*)

Print["3.e"]

(* ::Input:: *)
j=22;
filenameFITS= FileNames["attenuation_#*.fits",pathAttenuation];
attenuationReplace = ( Import[filenameFITS[[j-1]],"RawData" ][[1]]+
Import[filenameFITS[[j+1]],"RawData"  ][[1]])/2;

(*ArrayPlot[attenuationReplace,ImageSize->200];*)

filenameFITS = pathAttenuation<>"attenuation_#"<>IntegerString[j,10,4]<>"_"<>ToString[angleList[[j]]]<>"deg.fits";
Export[filenameFITS,attenuationReplace];



(* ::Input:: *)
j=46;
filenameFITS= FileNames["attenuation_#*.fits",pathAttenuation];
attenuationReplace = ( Import[filenameFITS[[j-1]],"RawData" ][[1]]+
Import[filenameFITS[[j+1]],"RawData"  ][[1]])/2;

ArrayPlot[attenuationReplace,ImageSize->200];

filenameFITS = pathAttenuation<>"attenuation_#"<>IntegerString[j,10,4]<>"_"<>ToString[angleList[[j]]]<>"deg.fits";
Export[filenameFITS,attenuationReplace];


(* ::Subsection:: *)
(*3.f. Read all absorption FITS.  Create *.png*)
Print["3.f"]

(* ::Input:: *)
Print["save attenuation images to figures/ dir for download"]

filenameFITS= FileNames["attenuation_#*.fits",pathAttenuation];
Table[Module[{},
attenuation =  Import[filenameFITS[[j]],"RawData" ][[1]];

gAttenuation= ArrayPlot[attenuation,PlotRange->grayScalePlotLimits,ClippingStyle->{White,Black},Frame->True,FrameTicks->True, AspectRatio->1,ImageSize->NY, ColorFunction->"GrayTones",
PlotLegends->{Placed[BarLegend[{"GrayTones",grayScalePlotLimits}],Right]},Epilog->Inset[Text[Style[ToString[angleList[[j]]]<>"\[Degree]",24,Blue]],Scaled[{0.2,0.9}]]];

filenamePNG = pathFigures<>"attenuation_#"<>IntegerString[j,10,4]<>"_"<>ToString[angleList[[j]]]<>"deg.png";
Export[filenamePNG,gAttenuation];

], {j, Length[filenameFITS]}];

(* ::Subsection::Closed:: *)
(*3.g Read all  *.png and make a movie (about 10 seconds)*)


(* ::Input:: *)
Timing[
filenamesPNG= FileNames["attenuation_#*",pathFigures];
indexList = ConstantArray[0,Length[filenamesPNG]];
For[j=1,j<= Length[filenamesPNG],j++,
filename = Last[FileNameSplit[filenamesPNG[[j]]]];
indexList[[j]] =ToExpression[ StringSplit[filename,{"#","_"}][[3]]];
];
indexList = Ordering[indexList];
filenamesPNG = filenamesPNG[[indexList]];

allImages =Table[ Import[filenamesPNG[[index]] ], {index,1, Length[filenamesPNG]}];

Print["save attenuation movie to stage folder for download"]
Switch[$OperatingSystem,
"MacOSX",Export[pathFigures<>"movie_attenuation_images.mov", allImages,"FrameRate"->15] ,
"Windows",Export[pathFigures<> "movie_attenuation_images.qt", allImages,"FrameRate"->15]  ,
"Unix",Export[pathFigures<> "movie_attenuation_images.avi", allImages,"FrameRate"->15]   ]
]

(* ::Subsection::Closed:: *)