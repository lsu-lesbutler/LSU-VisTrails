(* ::Section:: *)
(*Step 2.  Attenuation at the first rotation angle*)


(* ::Input:: *)


(* Get pathRoot from commandline *)
eval = ToExpression[##]&;
ARGV = Drop[$CommandLine, 3]; 
Print[ARGV]
(* Print[Length[ARGV]] *)
pathRoot = ARGV[[1]];

Get[pathRoot<>"bullet_jge_run_step_1.mx"]

(* change pathFigures to stage folder for job download*)
If[DirectoryQ["figures"], Null, CreateDirectory["figures"]];
pathFigures = "figures/"



grayScalePlotLimits = {-0.2,1.5};


(* ::Subsection::Closed:: *)
(*2.a  Open Beam*)


(* ::Input:: *)
openBeam = Import[pathRawData<>"OpenBeam_600s","Table"];
openBeam=funcFilterRawImages[openBeam];
{Dimensions[openBeam],Min[openBeam],Mean[Flatten[openBeam]],Max[openBeam]}//N

gOpenBeam = ArrayPlot[openBeam, ImageSize->200];


(* ::Subsection::Closed:: *)
(*2.b  Dark *)


(* ::Input:: *)
dark =Import[pathRawData<>"DarkImage_FastShutterClosed_100s","Table"];
dark=funcFilterRawImages[dark];
{Dimensions[dark],Min[dark],Mean[Flatten[dark]],Max[dark]}//N


(* ::Subsection::Closed:: *)
(*2.c  Read 0 degree raw and make attenuation image*)


(* ::Input:: *)
raw = Import[pathRawData<>"Bullet   0.0","Table"];
raw=funcFilterRawImages[raw];
{Dimensions[raw],Min[raw],Mean[Flatten[raw]],Max[raw]}//N


(* ::Input:: *)
attenuation = Re[Log[ (openBeam/600 - dark/100)/(raw/140- dark/100)]]//N;
{Dimensions[attenuation],Min[attenuation],Mean[Flatten[attenuation]],Max[attenuation]}


(* ::Subsection::Closed:: *)
(*2.d  Normalize the intensity values based on air region *)


(* ::Text:: *)
(*After normalization, the air region should have an average intensity value of 0. *)
(*The object region is forced to have an constant average intensity.  *)
(*We use a linear equation for the normalization:       corrected intensity = uncorrected intensity - offset*)


(* ::Input:: *)
airRegion = MedianFilter[attenuation[[All,220;;NX]],1];
offset = Mean[Flatten[airRegion]];
attenuation=attenuation-offset;
attenuation=funcFilterAttenuation[attenuation,Max[grayScalePlotLimits]];
{offset,Min[attenuation],Mean[Flatten[attenuation]],Max[attenuation]}


gHist=Histogram[Flatten[attenuation],{Min[grayScalePlotLimits],Max[grayScalePlotLimits],0.02},"LogCount"];
gLineProbe=ListPlot[attenuation[[128,All]], PlotRange->All, AxesOrigin->{0,0}];


(* ::Subsection::Closed:: *)
(*2.e  Rotate the attenuation image by 90 degree and save as *.fits*)


(* ::Input:: *)
attenuation =Reverse[Transpose[ attenuation]];
Export[pathAttenuation<>"attenuation_0deg.fits",attenuation];


(* ::Subsection:: *)
(*2.f  Set grayscale plot limits, replot, and save as *.png*)


(* ::Input:: *)

gAttenuation= ArrayPlot[attenuation,PlotRange->grayScalePlotLimits,ClippingStyle->{White,Black},Frame->True,FrameTicks->True, AspectRatio->1,ImageSize->200,ColorFunction->"GrayTones",
PlotLegends->{Placed[BarLegend[{"GrayTones",grayScalePlotLimits}],Right]}];


(* ::Input:: *)
Print["Export attenuation_0deg.png to job stage directory for download"]
gAll=GraphicsGrid[{{gAttenuation,gHist,gLineProbe}},ImageSize->800];
Export[pathFigures<>"attenuation_0deg.png",gAll,"png"];

(* ::Subsection::Closed:: *)
(*2.g DumpSave*)


(* ::Input:: *)
DumpSave[pathRoot<>"bullet_jge_run_step_2.mx",{openBeam,dark, grayScalePlotLimits}];
ClearAll["Global`*"]

(* ::Section::Closed:: *)