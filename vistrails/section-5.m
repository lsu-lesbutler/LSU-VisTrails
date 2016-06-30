(* ::Section:: *)
(*Step 5. Reconstruct the sinograms into slices (1 minute). *)
(*Save slices as FITS. Save volume as HDF5.*)


(* ::Input:: *)

(* Get pathRoot from commandline *)
eval = ToExpression[##]&;
ARGV = Drop[$CommandLine, 3]; 
Print[ARGV]
pathRoot = ARGV[[1]];


Get[pathRoot<>"bullet_jge_run_step_1.mx"]
Get[pathRoot<>"bullet_jge_run_step_2.mx"]
Get[pathRoot<>"bullet_jge_run_step_3.mx"]
Get[pathRoot<>"bullet_jge_run_step_4.mx"]

If[DirectoryQ["volumes"], Null, CreateDirectory["volumes"]];


(* ::Subsection:: *)
(*5.a Read all sinograms and reconstruct into slices*)


(* ::Input:: *)
listFilenamesSinograms = FileNames["sinogram*.fits",pathSinograms];
Print[Length[listFilenamesSinograms]]

ParallelTable[Module[{},
filenameSinogram=listFilenamesSinograms[[indexSlice]];
filenameSlice = pathSlices<>StringReplace[Last[FileNameSplit[filenameSinogram]],"sinogram"->"slice"];
sinogram =  Import[filenameSinogram,"RawData" ][[1]];
slice = ImageData[InverseRadon[Image[sinogram,"Real"]],"Real"];
Export[filenameSlice,slice];
],
{indexSlice,Length[listFilenamesSinograms] }];


(* ::Subsection:: *)
(*5.b Read all slices and save as one HDF5 volume*)


(* ::Input:: *)
listFilenamesSlices = FileNames["slice*.fits",pathSlices];
slice =  Import[listFilenamesSlices[[1]],"RawData" ][[1]];
{NX,NY}=Dimensions[slice];
NZ = Length[listFilenamesSlices];
volume = ConstantArray[0,{NY,NX,NZ}];
For[indexSlice =1,indexSlice<= NZ,indexSlice++,
slice =  Import[listFilenamesSlices[[indexSlice]],"RawData" ][[1]];
volume[[All,All,indexSlice]]=slice;
];
Dimensions[volume]


(* ::Input:: *)
filenameHDF5 =pathVolumes<>"bullet_new_volume.h5";
Export[filenameHDF5,{volume,angleList,{bestShift}},{"Datasets",{"volume","angleList","bestShift"}}]

Print["save bullet_new_volume.h5 to stage folder for download"]
filenameHDF5 ="volumes/"<>"bullet_new_volume.h5";
Export[filenameHDF5,{volume,angleList,{bestShift}},{"Datasets",{"volume","angleList","bestShift"}}]


(* ::Subsection:: *)
(*5.c DumpSave*)


(* ::Input:: *)
DumpSave[pathRoot<>"bullet_jge_run_step_5.mx",{angleList}];
ClearAll["Global`*"]