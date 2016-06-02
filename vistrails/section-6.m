(* ::Section:: *)
(*Step 6. Store all slices into one HDF5 file*)


(* ::Input:: *)

(* Get pathRoot from commandline *)
eval = ToExpression[##]&;
ARGV = Drop[$CommandLine, 3]; 
Print[ARGV]
(* Print[Length[ARGV]] *)
pathRoot = ARGV[[1]];

Get[pathRoot<>"bullet_jge_run_step_1.mx"]
Get[pathRoot<>"bullet_jge_run_step_2.mx"]
Get[pathRoot<>"bullet_jge_run_step_3.mx"]
Get[pathRoot<>"bullet_jge_run_step_4.mx"]

(* change pathFigures to stage folder for job download*)
If[DirectoryQ["figures"], Null, CreateDirectory["figures"]];
pathFigures = "figures/"


(* ::Subsection:: *)
(*6.a. Make a 3D *)


(* ::Input:: *)
filenameVolume = FileNames["*.h5",pathVolumes]//First


(* ::Input:: *)
Import[filenameVolume]
volumeData = Import[filenameVolume,{"Datasets","/volume"}];
{NX,NY,NZ}=Dimensions[volumeData]


(* ::Input:: *)
volumeImage =ImageAdjust[Image3D[volumeData,"Real"]];
ImageHistogram[volumeImage]


(* ::Input:: *)
volumeBinary =DeleteSmallComponents[Binarize[volumeImage,0.45],5];


(* ::Input:: *)
volumeImage = ImageMultiply[volumeImage,volumeBinary];


(* ::Input:: *)
gAttenuationVolume=Image3D[ImageAdjust[volumeImage,{0,0,1},{0.0,0.8}],
Axes->True,Boxed->True,BoxRatios->Automatic,BoxStyle->Directive[White],
Axes->True,AxesLabel->{"slices","columns","rows"},AxesStyle->Directive[White,14],
ColorFunction->"XRay",Background->Black,
ViewVertical->{-1,0,0},ViewPoint->{-1.0, -5.0, 1.0},ImageSize->700];


(* ::Input:: *)
Print["save attenuation_volume.png to stage folder for download"]
Export[pathFigures<>"attenuation_volume.png",gAttenuationVolume,"png"];
