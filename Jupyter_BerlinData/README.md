## General discussion about vectorized least squares

I look at the algorithm as five steps.  
First, setting up the basis vectors for offset, sin(xg/pg), and cos(xg/pg).  
Second, matrix multiply the basis vectors and other stuff to make the magic matrix G.  
Here starts the looping over tomography angles
Three, assemble all the images with the image (row,column) dimension flatten to (1, row*column) vector, then align all images (as vectors) based on xg distance. 
Four.  the answer matrix is the product of G * images
Five, disassemble the answer matrix into offset, amplitude, and phi

We are in the process of renaming variables in our code.  None of the Jupyter code has yet been updated.  
Old                                 New
transmission(some)         offset
visibility(some)                amplitude


Here are copy/paste examples from Les's Mathematica codes (Sorry, Les has not yet upgraded to Jupyter)
Step 0:  Setting gratings steps
In[90]:= gratingPeriodMM = 0.79;
numberGratingSteps = 14;
listGratingStepsMM = Subdivide[1.2, (numberGratingSteps - 1)]

Out[92]= {0., 0.0923077, 0.184615, 0.276923, 0.369231, 0.461538, \
0.553846, 0.646154, 0.738462, 0.830769, 0.923077, 1.01538, 1.10769, \
1.2}

First, setting up the basis vectors for offset, sin(xg/pg), and cos(xg/pg).
funcPrepareBvectorArbitrarySteps[gratingPeriodMM_, 
   listGratingStepsMM_] := Module[{b1, b2, b3, numberGratingSteps},
   (*xExpt =listGratingStepsMM;
   b1 = Table[xExpt[[i]]/gratingPeriod,{i,Length[xExpt]}];*)
   
   numberGratingSteps = Length[listGratingStepsMM];
   b1 = Table[1, {i, numberGratingSteps}];
   b2 = Table[
     Sin[2 \[Pi] listGratingStepsMM[[i]]/gratingPeriodMM] // N, {i, 
      numberGratingSteps}];
   b3 = Table[
     Cos[2 \[Pi] listGratingStepsMM[[i]]/gratingPeriodMM] // N, {i, 
      numberGratingSteps}];
   Chop[ Transpose[{b1, b2, b3} ]] ];
   
   
   
Second, matrix multiply the basis vectors and other stuff to make the magic matrix G.  IMPORTANT: Double check matrix dimensions.  In this worked example rows = 1849, columns = 1949, and rows*columns = 3603701
funcPrepareAllVectors[gratingPeriodMM_, listGratingStepsMM_, rows_, 
  columns_] := Module[{},
  bVector = 
   funcPrepareBvectorArbitrarySteps[gratingPeriodMM, 
    listGratingStepsMM];
  aVector = cVector = ConstantArray[0, {3, rows*columns}];
  aMatrix =  ConstantArray[0, {rows, columns, 3}];
  visibility = phi = ConstantArray[0, {rows, columns}];
  gMatrix = 
   Inverse[Transpose[bVector] . bVector] . Transpose[bVector];  ]
   
In[143]:= funcPrepareAllVectors[gratingPeriodMM, listGratingStepsMM, \
rows, columns]
{Dimensions[gMatrix], Dimensions[bVector], Dimensions[aVector], 
 Dimensions[aMatrix], Dimensions[visibility], Dimensions[phi]}

Out[144]= {{3, 14}, {14, 3}, {3, 3603701}, {1849, 1949, 3}, {1849, 
  1949}, {1849, 1949}}
   

Here starts the looping over tomography angles
Three, assemble all the images with the image (row,column) dimension flatten to (1, row*column) vector, then align all images (as vectors) based on xg distance. 
funcReadOneInterferogram[listFilenamesOneInterferogram_, rows_, 
   columns_, listGratingStepsMM_] := 
  Module[{filename, intensity, numberGratingSteps},
   numberGratingSteps = Length[listGratingStepsMM];
   allData = ConstantArray[0.0, {rows, columns, numberGratingSteps}];
   For[index = 1, index <= numberGratingSteps, index++,
    filename = listFilenamesOneInterferogram[[index]];
    (*intensity = Take[funcReadAndorTIFFFile[filename],cropRows,
    cropColumns,binning];*)
    
    intensity = 
     funcReadAndorTIFFFile[filename, cropRows, cropColumns, binning, 
      referenceDarkfield];
    allData[[All, All, index]] = intensity;
    ];  
   allData];
   
   
Four.  the answer matrix is the product of G * images
Five, disassemble the answer matrix into offset, amplitude, and phi  (*new names, below is still using old names *)

In[160]:= indexInterferogram = 2;
filenamesReference = 
 funcGetReferenceFilenamesForSpecificInterferogram[
  indexInterferogram]
interferogramReference = 
   funcReadOneInterferogram[filenamesReference, rows, columns, 
    listGratingStepsMM]; // Timing
Dimensions[interferogramReference]
{referenceTransmission, referenceVisibility, referencePhi} = 
   funcCalculateTransmissionVisibilityPhi[
    interferogramReference]; // Timing
{Dimensions[referenceTransmission], Dimensions[referenceVisibility], 
 Dimensions[referencePhi]}

Out[161]= \
{"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0015.tif", \
"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0016.tif", \
"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0017.tif", \
"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0018.tif", \
"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0019.tif", \
"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0020.tif", \
"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0021.tif", \
"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0022.tif", \
"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0023.tif", \
"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0024.tif", \
"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0025.tif", \
"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0026.tif", \
"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0027.tif", \
"/Users/les/Dropbox/Research/HZB_Mar2017/Raw_Data/Flatfield_2D 17 03 \
14_17 31/0_G0_Scan_x5/G0_Scan_x5_0028.tif"}

Out[162]= {47.7817, Null}   (* Most of the time is reading image files *)

Out[163]= {1849, 1949, 14}

Out[164]= {2.43691, Null}   (* The vectorized least squares is fast, 2.4seconds *)

Out[165]= {{1849, 1949}, {1849, 1949}, {1849, 1949}}
   
