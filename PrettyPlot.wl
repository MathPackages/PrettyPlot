(* ::Package:: *)

BeginPackage["PrettyPlot`"];


(* Exported function declarations *)
Shadowbox::usage = "Shadowbox[legend, size, height] creates a shadowed legend box with default size=100 and height=1, change box size by running 
LegendFunction\[Rule](Shadowbox[#,new_size,new_height]&).";
PrettyPlot::usage = "PrettyPlot[plotFunc, args, opts] creates a custom-styled plot with predefined options.";


Begin["Private`"];


(*PrettyPlot default options*)
Options[PrettyPlot]={PlotTheme->"Web",Frame->True,FrameStyle->Directive[Black,Thick],BaseStyle->FontSize->18,
LabelStyle->{FontFamily->If[OSystem=="Linux x86 (64-bit)","Latin Modern Roman","CMU Serif"],GrayLevel[0]},
ImagePadding->{{Automatic,30},{Automatic,40}},GridLines->Automatic,ImageSize->Large,ColorFunction->None,autoOption->Automatic};


(*Function Declarations*)
Shadowbox[legend_,size_:100,height_:1.0]:= 
	Graphics[{{Gray,EdgeForm[{Thick,Gray}],Rectangle[{-0.05,-0.05},{1.15,0.95*height}]},
		{White,EdgeForm[{Thin,Gray}],Rectangle[{-.1,0},{1.1,1*height}]},Inset[legend,{0.5,height/2},Center]},ImageSize->size];


PrettyPlot[plotFunc_,args___,opts:OptionsPattern[]]:=Module[{plotTheme,frame,frameStyle,baseStyle,labelStyle,imagePadding,gridLines,imageSize,colorFunction,plotLegends,frameTicks,combinedOpts},
plotTheme=OptionValue[PlotTheme];frame=OptionValue[Frame];
frameStyle=OptionValue[FrameStyle];baseStyle=OptionValue[BaseStyle];labelStyle=OptionValue[LabelStyle];
imagePadding = OptionValue[ImagePadding];
gridLines = OptionValue[GridLines];
imageSize = OptionValue[ImageSize];
colorFunction = OptionValue[ColorFunction];
plotLegends = OptionValue[autoOption];
frameTicks = OptionValue[autoOption];
 Off[OptionValue::nodef];
 (*Change contour plot settings*)
 If[plotFunc===ContourPlot||plotFunc===ListContourPlot,{gridLines=None,colorFunction = "SunsetColors",plotLegends=BarLegend[Automatic,LegendMarkerSize->500]}];
 (*Combine the default options with the provided options*)
 combinedOpts=Join[{PlotTheme->plotTheme,Frame->frame,FrameStyle->frameStyle,BaseStyle->baseStyle,LabelStyle->labelStyle,ImagePadding->imagePadding,GridLines->gridLines,
 ImageSize->imageSize,ColorFunction->colorFunction,PlotLegends->plotLegends,FrameTicks->frameTicks},FilterRules[{opts},Except[{PlotTheme,Frame,FrameStyle,BaseStyle,LabelStyle,ImagePadding,GridLines,ImageSize,ColorFunction,PlotLegends,FrameTicks}]]];
(*Call the plot function with the arguments and the combined options*)
plotFunc[args,Evaluate[Sequence@@combinedOpts]]];


End[];


EndPackage[];
