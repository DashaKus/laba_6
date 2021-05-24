(* ::Package:: *)

(* Exported Functions:
		ManipulatePlot
*)


(* ::Section::Closed:: *)
(*Begin*)


BeginPackage["ResourceFunctionHelpers`", {"ResourceFunctionHelpers`CommonFunctions`"}];

ClearAll[ManipulatePlot];
ManipulatePlot::usage = "Generate a plot with parameters in a manipulate that can change the plot range. ";
Begin["`ManipulatePlot`Private`"]


(* ::Section::Closed:: *)
(*Code*)


(* ::Subsection::Closed:: *)
(*ManipulatePlot*)


goodExpr[expr_] := FreeQ[expr, _Complex | _DirectedInfinity | ComplexInfinity]
$paramPattern = {{_Symbol, _?NumericQ}, _?NumericQ, _?NumericQ, ___?NumericQ} | {_Symbol, _?NumericQ, _?NumericQ, ___?NumericQ};


ManipulatePlot::underspecified = "Insufficient parameters specified.";
ManipulatePlot::paramvals = "Input contains symbols to be used for plotting or manipulating the plotted curve which already have values assigned to them.";


Options[ManipulatePlot] = Join[Options[Plot],Options[Manipulate], {"VerticalPlotRange" -> Automatic}];


ManipulatePlot[expr_?goodExpr, {x_Symbol, a_?NumericQ, b_?NumericQ}, iparams:($paramPattern..):None, opts:OptionsPattern[]] /; FreeQ[{iparams}, {_, aa_?NumericQ, aa_}] := Block[
	{paramsAndBounds, initialConditions, params, usedparams, condition, initXPR, xPR, plotRangeYFull, plotRangeYInitial, goodResFlag = True},
	
	usedparams = DeleteDuplicates@Cases[expr, _?usersymbolQ, Infinity];
	If[MatchQ[Hold[iparams], Hold[None]],
		paramsAndBounds = condition = params = {},
		
		paramsAndBounds = {iparams} /. {p_, lo_?NumericQ, hi_?NumericQ, inc___} :> {p, Sequence@@NumericalSort[{lo, hi}], inc};
		condition = paramsAndBounds /. {{param_, _?NumericQ}, lo_?NumericQ, hi_?NumericQ, ___} :> {param, lo, hi};
		params = condition[[All,1]];
	];
	
	If[!SubsetQ[Append[params, x], usedparams],
		goodResFlag = False;
		RFHMessage[ManipulatePlot::underspecified]
	];
	(
		initialConditions = Select[
			paramsAndBounds /. {param_Symbol, start_?NumericQ, ___} | {{param_Symbol, start_?NumericQ}, _?NumericQ, ___} :> (param -> start),
			!FreeQ[#, Alternatives @@ usedparams]&];
		condition = Select[condition, !FreeQ[#, Alternatives @@ usedparams]&];
		
		initXPR = NumericalSort[{a, b}];
		xPR = {initXPR[[1]] - Abs[a-b]/2, initXPR[[2]] + Abs[a-b]/2};
		
		If[MatchQ[OptionValue["VerticalPlotRange"], {_?NumericQ, _?NumericQ}],
			Block[
				{vPR = NumericalSort[OptionValue["VerticalPlotRange"]]},
				plotRangeYInitial = OptionValue["VerticalPlotRange"];
				plotRangeYFull = {vPR[[1]] - Abs[Subtract@@vPR]/2, vPR[[2]] + Abs[Subtract@@vPR]/2}
			],
			plotRangeYInitial = getPlotRangeY[expr, initialConditions, params, Prepend[xPR, x]];
			plotRangeYFull = ReplaceAll[
				getPlotRangeY[expr, condition, params, Prepend[xPR, x]],
				{lo_, hi_} :> ({If[NumericQ[lo],lo - #, -10], If[NumericQ[hi], hi + #, 10]}& [.5 * (hi - lo)])
			]
		];
		
		If[!IntervalMemberQ[Interval[plotRangeYFull], Interval[plotRangeYInitial]],
			plotRangeYInitial = IntervalIntersection[Interval[plotRangeYFull], Interval[plotRangeYInitial]] /. Interval[ls_]:>ls];
		
		paramsAndBounds = paramsAndBounds /. {var:(_List | _Symbol), lo_?NumericQ, hi_} :> Control@{var, lo, hi};
		
		makeManipulatePlot[
			expr,
			x,
			plotRangeYInitial,
			plotRangeYFull,
			makeMinInterval@plotRangeYFull,
			initXPR,
			xPR,
			makeMinInterval@xPR,
			paramsAndBounds,
			params,
			opts
		]
	) /; goodResFlag
]


makeMinInterval[ls_] := .05 * Subtract@@Reverse@ls


(* fall back DV to handle inputs where the manipulate or plot variables already have values *)
ManipulatePlot[expr_, {x_, __}, params:(_List..):None, opts:OptionsPattern[]] /; NumericQ[x] || !FreeQ[{params}, {{_?NumericQ, _}|_?NumericQ, __}]:= (
	RFHMessage[ManipulatePlot::paramvals];
	$Failed /; False
)


(* ::Subsection::Closed:: *)
(*Helpers*)


(* ::Subsubsection::Closed:: *)
(*Styles*)


$itemStyle = Directive[12, GrayLevel[.4]]
$headerStyle = Directive[14, GrayLevel[.1]]


style[expr_, "Header"] := Style[expr, $headerStyle]
style[expr_, "Item"] := Style[expr, $itemStyle]
style[expr_] := style[expr, "Item"]


(* ::Subsubsection::Closed:: *)
(*makeManipulatePlot*)


ClearAll[makeManipulatePlot];
Options[makeManipulatePlot] = Options[ManipulatePlot];

makeManipulatePlot[expr_, x_, {initylo_, inityhi_}, {absylo_, absyhi_}, minYInterval_, {initxlo_, initxhi_}, {absxlo_, absxhi_}, minXInterval_, paramsAndBounds_, params_, opts:OptionsPattern[]] := With[
	{plotLegends = If[
		Or[
			CurrentValue[EvaluationNotebook[], StyleDefinitions] === "WolframAlphaNotebook.nb",
			OptionValue[makeManipulatePlot, {opts}, PlotLegends] === "Expressions"
		],
		PlotLegends -> If[ListQ@expr,ToString/@expr, ToString@expr],
		Sequence@@{}
	]},
	DynamicModule[
	{openV = False, openH = False, ylo, yhi, xlo, xhi, xprVar, yprVar, t1, t2, t3, t4},
	{ylo, yhi} = N[adjustInitCond[{initylo, inityhi}, minYInterval]];
	{xlo, xhi} = N[adjustInitCond[{initxlo, initxhi}, minXInterval]];
	
	If[$CloudEvaluation,
		xprVar = {xlo, xhi};
		yprVar = {ylo, yhi};
	];

	Manipulate[
		Plot[
			expr,
			Evaluate@{x, xlo, xhi},
			PlotRange -> Evaluate[{xprVar, yprVar}],
			ImageSize -> Evaluate[OptionValue[ImageSize] /. Automatic | _OptionValue -> Large],
			plotLegends,
			Evaluate@FilterRules[Flatten@{opts}, Options[Plot]]
		],
		Evaluate[makeRangeSlider[{minYInterval, {ylo, yhi}, {absylo, absyhi}, "Vertical"}, ylo, yhi, yprVar, openV, {t1, t2}]],
		Evaluate[makeRangeSlider[{minXInterval, {xlo, xhi}, {absxlo, absxhi}, "Horizontal"}, xlo, xhi, xprVar, openH, {t3, t4}]],
		Delimiter,
		Evaluate[Sequence@@paramsAndBounds],
		ControlPlacement -> Bottom,
		LabelStyle -> $headerStyle,
		SaveDefinitions -> True,
		TrackedSymbols -> Join[params, {yprVar, xprVar}],
		Evaluate[FilterRules[Flatten@{opts}, Options[Manipulate]]/. {} -> Sequence@@{}]
	]
]]


adjustInitCond[{lo_, hi_}, minInterval_] := If[hi-lo<minInterval, {#-minInterval/2, #+minInterval/2}&@Mean[{hi,lo}], {lo,hi}]


(* ::Subsubsection::Closed:: *)
(*makeRangeSlider*)


$inFieldOpts := {
	BaseStyle -> {"Manipulator", FontSize -> CurrentValue[{"ControlsFontSize", Small}]},
	ImageMargins -> {{10,0},{0,0}},
	ImageSize -> {50, Small}
}


ClearAll[makeRangeSlider];
SetAttributes[makeRangeSlider, HoldRest];


(* Cloud -- using two sliders for x and y range since IntervalSlider isn't supported *)
makeRangeSlider[{minInterval_, initPR_, {absLo_, absHi_}, type_}, lo_, hi_, prVar_, _, {temp1_, temp2_}] /; $CloudEvaluation := 
Sequence @@ With[{absMid = Mean[{absLo, absHi}]},
	{(* min slider *)
		{
			{temp1, initPR[[1]], type<>" min"},
			Dynamic[
				Manipulator[
					Dynamic[
						temp1,
						(temp1 = lo = prVar[[1]] = #)&
					],
					{absLo, absMid}
				]
			]&
		},
		(* max slider *)
		{
			{temp2, initPR[[2]], type<>" max"},
			Dynamic[
				Manipulator[
					Dynamic[
						temp2,
						(temp2 = hi = prVar[[2]] = #)&
					],
					{absMid, absHi}
				]
			]&
		}
	}
]


(* Desktop -- using IntervalSlider to have nicer UI *)
makeRangeSlider[{minInterval_, initPR_, absPR:_, type_}, lo_, hi_, prVar_, openVar_, _] :=
{
	{prVar, initPR, type<>" range"},
	Dynamic[
		Grid[
			{
				{
					IntervalSlider[
						Dynamic[prVar, ({lo, hi} = prVar = #)&, TrackedSymbols -> {lo, hi, prVar}, Initialization :> (prVar = initPR)&],
						absPR,
						MinIntervalSize -> minInterval,
						Method -> "Push",
						Appearance -> ("ThumbAppearance" -> {style["[","Header"], None, style["]","Header"]})
					],
					Toggler[Dynamic[openVar, TrackedSymbols -> {openVar}], {True -> $minusIcon, False -> $plusIcon}]
				},
				If[openVar,
					{
						Row[{
							Spacer[8],
							style["Min:"],
							preventShiftEnter@InputField[Dynamic[lo, (prVar[[1]] = lo = #)&, TrackedSymbols -> {lo, prVar}], $inFieldOpts],
							Spacer[10],
							style["Max: "],
							preventShiftEnter@InputField[Dynamic[hi, (prVar[[2]] = hi = #)&, TrackedSymbols -> {hi, prVar}], $inFieldOpts]
						}], SpanFromLeft
					},
					Nothing
				]
			},
			Alignment -> Left,
			BaselinePosition -> {1,1}
		],
		TrackedSymbols -> {openVar}
	]&
}


$minusIcon = RawBoxes @ DynamicBox @ FEPrivate`FrontEndResource["FEBitmaps", "SquareMinusIcon"];
$plusIcon = RawBoxes @ DynamicBox @ FEPrivate`FrontEndResource["FEBitmaps", "SquarePlusIcon"];


preventShiftEnter[expr_] := EventHandler[
	expr,
	{
		{"MenuCommand", "HandleShiftReturn"} :> {},
		{"MenuCommand", "EvaluateCells"} :> {}
	}
]


(* ::Subsubsection::Closed:: *)
(*getPlotRangeY*)


ClearAll[getPlotRangeY];
getPlotRangeY[expr:Except[_List], conditions_, params_, xPR_List] := getPlotRangeY[{expr}, conditions, params, xPR]

getPlotRangeY[expr_, conditions:{_Rule..}, params_, xPR_List] := Last@PlotRange[Plot[#, xPR] /. Legended[g_, ___] :> g]&[(expr /. conditions)]

getPlotRangeY[exprs_, iconditions_, params_, xPR:{x_, a_, b_}] := Block[
	{args, conditions, minsAndMaxs},
	
	conditions = iconditions /. {param_Symbol, lo_?NumericQ, hi_?NumericQ} :> LessEqual[lo, param, hi];
	conditions = And @@ Prepend[conditions, LessEqual[a,x,b]];
	
	args = {{#, conditions}, Prepend[params, x]}& /@ exprs;
	
	minsAndMaxs = Quiet@Check[{NMinimize@@#, NMaximize@@#}& /@ args, $Failed, {NMaximize::cvdiv, NMinimize::cvdiv, NMaximize::nrnum, NMinimize::nrnum}];
	If[!MatchQ[minsAndMaxs, $Failed],
		quickMinMax[minsAndMaxs],
		safeMinMax[exprs, iconditions, params, xPR]
	]
]


(* ::Subsubsection::Closed:: *)
(*quickMinMax, safeMinMax*)


ClearAll[quickMinMax, safeMinMax];

quickMinMax[iminsAndMaxs_] := Block[
	{minsAndMaxs = iminsAndMaxs},
	minsAndMaxs = minsAndMaxs[[All, All, 1]];
	minsAndMaxs = Transpose@minsAndMaxs;
	{Min[#1], Max[#2]}&@@minsAndMaxs
]

safeMinMax[exprs_, conditions_, params_, xPR_] := Block[
	{minmax, prs, range, scale},
	prs = Table[
		Quiet[Last@
			Check[
				FullOptions[Plot[
					exprs,
					N@xPR,
					MaxRecursion -> 4
				] /. Legended[g_, ___] :> g, PlotRange],
				Nothing
			]
		],
		##
	]& @@ (maketableArg[#, Length[conditions]]& /@ conditions);
	
	minmax = MinMax[
		Flatten[
			DeleteCases[prs, _Last, Infinity]
		]
	];
	
	range = Subtract@@Reverse@minmax;
	scale = Ceiling[Log10[range]]/10.;
	{Floor[minmax[[1]]-scale, scale], Ceiling[minmax[[2]]+scale, scale]}
]


ClearAll[maketableArg];
maketableArg[{param_, lo_, hi_}, len_] := {param, lo, hi, (hi-lo)/Ceiling[9/len]}


(* ::Section::Closed:: *)
(*Examples*)


(* ::Input:: *)
(*ManipulatePlot[{c Sin[b*x]+a Cos[b*x], Sin[a x]},  {x, -2Pi, 2Pi}, {{a, 1},-1,2}, {{b,1}, -4,4}, {{c,3},-1,5}]*)


(* ::Section:: *)
(*Tests*)


(* ::Section::Closed:: *)
(*End*)


End[];
EndPackage[];


(* ::Section::Closed:: *)
(*Notes*)


(* ::Text:: *)
(*How do you set the base condition for an IntervalSlider?*)
