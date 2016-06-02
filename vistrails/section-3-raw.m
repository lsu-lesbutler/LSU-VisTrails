(* Get pathRoot from commandline *)
eval = ToExpression[##]&;
ARGV = Drop[$CommandLine, 3]; 
Print[ARGV]
(* Print[Length[ARGV]] *)
pathRoot = ARGV[[1]];


Get[pathRoot<>"bullet_jge_run_step_1.mx"]
Get[pathRoot<>"bullet_jge_run_step_2.mx"]

(* change pathFigures to stage folder for job download*)
If[DirectoryQ["figures"], Null, CreateDirectory["figures"]];
pathFigures = "figures/"


grayScalePlotLimits = {-0.1,1.5};


(* split of section 3 ::Subsection:: 3.a to 3.d + dumpsave *)
(*3.a Get  filenames of all raw images*)


(* ::Input:: *)
filenamesRaw = FileNames["Bullet*",pathRawData];
angleList = ConstantArray[0,Length[filenamesRaw]];
For[j=1,j<= Length[filenamesRaw],j++,
filename = Last[FileNameSplit[filenamesRaw[[j]]]];
angleList[[j]] =ToExpression[ Last[StringSplit[filename]]];
];
indexList = Ordering[angleList];
angleList = angleList[[indexList]];
filenamesRaw = filenamesRaw[[indexList]];  Clear[indexList]
Dimensions[filenamesRaw]
filenamesRaw[[1]]
filenamesRaw[[Length[filenamesRaw]]]


(* ::Subsection::Closed:: *)
(*3.b. Make a short table of rotation angles and corresponding filenames*)


(* ::Input:: *)
TableForm[Transpose[{angleList,filenamesRaw}][[1;;30]],TableHeadings->{Automatic,{"angle","filename"}}]


(* ::Subsection::Closed:: *)
(*3.c Read all  * text files and make a movie*)


(* ::Text:: *)
(*We learn that there are two bad frames.*)


(* ::Input:: *)
Print["save raw images to stage folder for download"]

allImages =Table[ Module[{},
raw =Import[filenamesRaw[[index]],"Table"];
gRaw =Image[raw,"Bit16"]//ImageAdjust ];
Show[gRaw,Epilog->Inset[Text[index],Scaled[{0.1,0.95}],Background->White]
Export[pathFigures<>"raw_#"<>ToString[index]<>".png",gRaw,"PNG"]
], {index,1, Length[filenamesRaw]}];


(* ::Input:: *)
Print["save raw movie to stage folder for download"]

Switch[$OperatingSystem,
"MacOSX",Export[pathFigures<>"movie_raw_images.mov", allImages,"FrameRate"->15,Antialiasing->True,"VideoEncoding"->"Apple Intermediate Codec"] ,
"Windows",Export[pathFigures<> "movie_raw_images.qt", allImages,"FrameRate"->15] ,
"Unix",Export[pathFigures<> "movie_raw_images.avi", allImages,"FrameRate"->15]   ]

(*3.d. Read all raw images and make all attenuation files.  Save as FITS*)
(*(about 4 minutes on a laptop)*)


(* ::Input:: *)
ParallelTable[Module[{},
filename = Last[FileNameSplit[filenamesRaw[[j]]]];
raw = Import[filenamesRaw[[j]],"Table"];
raw=funcFilterRawImages[raw];
attenuation = Re[Log[ (openBeam/600 - dark/100)/(raw/140- dark/100)]]//N;
airRegion = MedianFilter[attenuation[[All,220;;NX]],1];
offset = Mean[Flatten[airRegion]];
attenuation =attenuation - offset;
attenuation=funcFilterAttenuation[attenuation,Max[grayScalePlotLimits]];
attenuation =Reverse[Transpose[ attenuation]];   
filenameFITS = pathAttenuation<>"attenuation_#"<>IntegerString[j,10,4]<>"_"<>ToString[angleList[[j]]]<>"deg.fits";
Export[filenameFITS,attenuation]; 
Print[{j,offset, Min[attenuation],Mean[Flatten[attenuation]],Max[attenuation]}];
],
{j, 1,Length[filenamesRaw],1}];



(*DumpSave*)


(* ::Input:: *)
DumpSave[pathRoot<>"bullet_jge_run_step_3.mx",{angleList}];
ClearAll["Global`*"]