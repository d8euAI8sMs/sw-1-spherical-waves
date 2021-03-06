(* ::Package:: *)

(* ::Section::Closed:: *)
(*\:0418\:043c\:043f\:043e\:0440\:0442 \:0431\:0438\:0431\:043b\:0438\:043e\:0442\:0435\:043a*)


(* ::Input::Initialization:: *)
If[$FrontEnd =!= Null, AppendTo[$Path, FileNameJoin[{NotebookDirectory[], "..", "..", "lib", "mathematica"}]]];

ImportLibs := (Once@Get[#] &) /@ { "BesselRoots.m" };

ImportLibs;


(* ::Section::Closed:: *)
(*\:0418\:043d\:0438\:0446\:0438\:0430\:043b\:0438\:0437\:0430\:0446\:0438\:044f \:0432\:044b\:0447\:0438\:0441\:043b\:0438\:0442\:0435\:043b\:044c\:043d\:044b\:0445 \:0443\:0437\:043b\:043e\:0432*)


LaunchKernels[]


(* ::Section::Closed:: *)
(*\:0412\:0441\:043f\:043e\:043c\:043e\:0433\:0430\:0442\:0435\:043b\:044c\:043d\:044b\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0438*)


(* ::Text:: *)
(*\:0424\:0438\:0437\:0438\:0447\:0435\:0441\:043a\:0438\:0435 \:043a\:043e\:043d\:0441\:0442\:0430\:043d\:0442\:044b*)


ccc = 299792458; (* \:0441\:043a\:043e\:0440\:043e\:0441\:0442\:044c \:0441\:0432\:0435\:0442\:0430, \:043c/\:0441 *)
hhh = 6.582119514*10^(-16); (* \:043f\:043e\:0441\:0442\:043e\:044f\:043d\:043d\:0430\:044f \:041f\:043b\:0430\:043d\:043a\:0430, \:044d\:0412*\:0441 *)
kT = 0.026; (* \:043f\:0440\:043e\:0438\:0437\:0432\:0435\:0434\:0435\:043d\:0438\:0435 \:043f\:043e\:0441\:0442\:043e\:044f\:043d\:043d\:043e\:0439 \:0411\:043e\:043b\:044c\:0446\:043c\:0430\:043d\:0430 \:043d\:0430 \:0442\:0435\:043c\:043f\:0435\:0440\:0430\:0442\:0443\:0440\:0443 300\:041a, \:044d\:0412 *)


(* ::Text:: *)
(*\:0412\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:0435 \:0441\:0432\:0435\:0440\:0442\:043a\:0438 \:0441 \:043f\:0440\:044f\:043c\:043e\:0443\:0433\:043e\:043b\:044c\:043d\:044b\:043c \:043e\:043a\:043d\:043e\:043c*)


RectFilter0 = Compile[{
	{data, _Real, 2}, {w, _Real}
}, Module[{
		j1 = 1, j2 = 1, x1, x2, t = 0.0
	},
	Table[
		x1 = data[[i, 1]] - w / 2;
		x2 = data[[i, 1]] + w / 2;
		While[j1 <= j2 && data[[j1, 1]] < x1, t -= data[[j1, 2]]; ++j1];
		While[j2 <= Length[data] && data[[j2, 1]] <= x1, ++j2];
		While[j2 <= Length[data] && data[[j2, 1]] <= x2, t += data[[j2, 2]]; ++j2];
		t / w
	, {i, Length[data]}]
], CompilationTarget -> "C", CompilationOptions -> {
	"ExpressionOptimization" -> True, "InlineCompiledFunctions" -> True, "InlineExternalDefinitions" -> True
}, RuntimeOptions -> "Speed"];

RectFilter[data_?(ArrayQ[#, 2]&), w_?NumberQ] := Module[{flt},
	flt = RectFilter0[data, w];
	Table[{First@data[[i]], flt[[i]]}, {i, Length[data]}]
];


(* ::Text:: *)
(*\:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 \:0435\:0434\:0438\:043d\:043e\:0433\:043e \:043b\:0438\:043d\:0435\:0430\:0440\:0438\:0437\:043e\:0432\:0430\:043d\:043d\:043e\:0433\:043e \:0441\:043f\:0438\:0441\:043a\:0430 \:043c\:043e\:0434 \:043f\:043e \:044d\:043d\:0435\:0440\:0433\:0438\:044f\:043c \:0441 \:0443\:0447\:0435\:0442\:043e\:043c \:0432\:044b\:0440\:043e\:0436\:0434\:0435\:043d\:0438\:044f (l-\:044f \:043c\:043e\:0434\:0430 2l+1 \:0432\:044b\:0440\:043e\:0436\:0434\:0435\:043d\:0430)*)


MakeDiscreteModeDistribution[{ al1_, al2_ }, { bl1_, bl2_ }, r_] := Module[{a, b},
	a = ParallelTable[{ besselRoots[[i - radialFuncMinOrder + 1]][[j]], 2 i + 1 }, {i, al1, Min[al2, Length[besselRoots] + radialFuncMinOrder - 1]}, {j, Min[r, Length@besselRoots[[i - radialFuncMinOrder + 1]]]}];
	b = ParallelTable[{ riccatiBesselPrimeRoots[[i - radialFuncMinOrder + 1]][[j]], 2 i + 1 }, {i, bl1, Min[bl2, Length[riccatiBesselPrimeRoots] + radialFuncMinOrder - 1]}, {j, Min[r, Length@riccatiBesselPrimeRoots[[i - radialFuncMinOrder + 1]]]}];
	Return[SortBy[Flatten[{ a, b }, 2], First]];
];


(* ::Text:: *)
(*\:041e\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:0438\:0435 \:043a\:0430\:0447\:0435\:0441\:0442\:0432\:0430 \:0430\:043f\:043f\:0440\:043e\:043a\:0441\:0438\:043c\:0430\:0446\:0438\:0438 \:043a\:0432\:0430\:0434\:0440\:0430\:0442\:0438\:0447\:043d\:043e\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0435\:0439 \:0447\:0438\:0441\:043b\:0430 \:043c\:043e\:0434*)


TestSqFunc[] := Module[{sqFunc, sqFuncFit, correlation, plots},
	sqFunc = a x^2;

	sqFuncFit = FindFit[cmplConv, { sqFunc }, { a }, x];

	correlation = Correlation[Last /@ cmplConv, ((sqFunc /. sqFuncFit) /. x -> First@# &) /@ cmplConv];

	plots = {
		Pane@Show[{
			ListPlot[cmplConv, PlotRange -> { { 0, First@Last@cmplConv }, Full }],
			Plot[sqFunc /. sqFuncFit, { x, 0, First@Last@cmplConv }, PlotStyle -> { Red, Thick }]
		}, AxesLabel -> { "x", "\[CapitalDelta]N(x)" }],
		Pane@Show[{
			ListPlot[Take[cmplConv, 10], PlotRange -> { { 0, First@cmplConv[[10]] }, Full }],
			Plot[sqFunc /. sqFuncFit, { x, 0, First@cmplConv[[10]] }, PlotStyle -> { Red, Thick }]
		}, AxesLabel -> { "x", "\[CapitalDelta]N(x)" }]
	};
	
	{ a /. sqFuncFit, correlation, plots }
];


(* ::Text:: *)
(*\:0424\:043e\:0440\:043c\:0443\:043b\:044b \:041f\:043b\:0430\:043d\:043a\:0430*)


Plank[data_?ListQ, T_, r0_] := ({ First@#, First@# (Exp[hhh * First@# * ccc / (kT / 300 * T * r0)] - 1) ^ (-1) Last@# / (4/3 N@Pi r0^3) } &) /@ data;
Plank[T_, r0_] := Function[{x}, (x^3 / N@Pi^2 / r0^3 (Exp[hhh * x * ccc / (kT / 300 * T * r0)] - 1) ^ (-1))];


(* ::Text:: *)
(*\:041f\:043e\:0441\:0442\:0440\:043e\:0435\:043d\:0438\:0435 \:043f\:043b\:0430\:043d\:043a\:043e\:0432\:0441\:043a\:043e\:0439 \:043a\:0440\:0438\:0432\:043e\:0439*)


TestPlank[T_, r0_] := Module[{plank, plot, splot, lplot, slplot},
	plank = Plank[cmplConv, T, r0];
	lplot = ListPlot[plank, PlotRange -> Full, AxesLabel -> { "x", "u(x)" }];
	plot = Plot[Plank[T, r0][x], {x, 0, First@Last@cmplConv}, PlotRange -> Full, PlotStyle -> { Red, Thick }];
	slplot = ListPlot[Take[plank, 10], PlotRange -> Full];
	splot = Plot[Plank[T, r0][x], {x, 0, First@cmplConv[[10]]}, PlotRange -> Full, PlotStyle -> { Red, Thick }];
	{
		Pane@Show[{lplot, plot}, PlotRange -> All, AxesOrigin -> {0, 0}, AxesLabel -> { "x", "u(x)" }],
		Pane@Show[{slplot, splot}, PlotRange -> All, AxesOrigin -> {0, 0}, AxesLabel -> { "x", "u(x)" }]
	}
];


(* ::Text:: *)
(*\:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 \:0441\:043f\:0438\:0441\:043a\:0430 \:043c\:043e\:0434 \:0441 \:0434\:0435\:0442\:0430\:043b\:0438\:0437\:0430\:0446\:0438\:0435\:0439 \:043f\:043e \:043d\:043e\:043c\:0435\:0440\:0443 l*)


MakeDetailedModeDistribution[{ l1_, l2_ }, r_] := Module[{a, b, s},
	a = ParallelTable[{ besselRoots[[i - radialFuncMinOrder + 1]][[j]], 2 i + 1 }, {i, l1, Min[l2, Length[besselRoots] + radialFuncMinOrder - 1]}, {j, Min[r, Length@besselRoots[[i - radialFuncMinOrder + 1]]]}];
	b = ParallelTable[{ riccatiBesselPrimeRoots[[i - radialFuncMinOrder + 1]][[j]], 2 i + 1 }, {i, l1, Min[l2, Length[riccatiBesselPrimeRoots] + radialFuncMinOrder - 1]}, {j, Min[r, Length@riccatiBesselPrimeRoots[[i - radialFuncMinOrder + 1]]]}];
	Return[SortBy[Table[SortBy[Flatten[{ a[[i]], b[[i]] }, 1], First], {i, Min[Length[a], Length[b]]}], (First@First@# &)]];
];


(* ::Text:: *)
(*\:041f\:043e\:0441\:0442\:0440\:043e\:0435\:043d\:0438\:0435 \:043f\:043b\:0430\:043d\:043a\:043e\:0432\:0441\:043a\:043e\:0439 \:043a\:0440\:0438\:0432\:043e\:0439 \:0434\:043b\:044f \:0441\:043b\:0443\:0447\:0430\:044f \:0434\:0435\:0442\:0430\:043b\:0438\:0437\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:043e\:0433\:043e \:0441\:043f\:0438\:0441\:043a\:0430 \:043c\:043e\:0434*)


DetailedTestPlank[T_, r0_, l0_, nleg_] := Module[{plank, plot, splot, lplot, slplot, maxx, maxy},
	plank = Table[Plank[cmplModes[[i]], T, r0], {i, Length[cmplModes]}];
	maxx = 0; maxy = 0;
	Do[maxy = Max[maxy, plank[[i,j,2]]], {i, Length[plank]}, {j, Length[plank[[i]]]}];
	Do[If[plank[[i,j,2]] > 0.01 maxy, maxx = plank[[i,j,1]]; Break[]], {i, Length[plank], 1, -1}, {j, Length[plank[[i]]], 1, -1}];
	lplot = ListPlot[plank, PlotRange -> { {0, maxx}, Full }, PlotMarkers -> Automatic, PlotLegends -> Table[Style[StringForm["l = ``", l0 + i - 1], FontFamily -> "Times", Italic], {i, Min[Length[cmplModes], nleg]}]];
	plot = Plot[Plank[T, r0][x], {x, 0, maxx}, PlotRange -> Full, PlotStyle -> { Red, Thick }];
	Pane@Show[Flatten[{lplot, {plot}}, 1], PlotRange -> { {0, maxx}, All }, AxesOrigin -> {0, 0}, AxesLabel -> { "x", "u(x), \*SubscriptBox[W,n]" }]
];


(* ::Section:: *)
(*\:0412\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:044f*)


(* ::Subsection::Closed:: *)
(*\:041f\:0435\:0440\:0432\:044b\:0435 \:043d\:0435\:0441\:043a\:043e\:043b\:044c\:043a\:043e \:043a\:043e\:0440\:043d\:0435\:0439 \:0440\:0430\:0434\:0438\:0430\:043b\:044c\:043d\:044b\:0445 \:0444\:0443\:043d\:043a\:0446\:0438\:0439*)


(* \:041f\:0435\:0440\:0435\:043c\:0435\:043d\:043d\:044b\:0435 besselRoots, riccatiBesselPrimeRoots,
   radialFuncMinOrder, radialFuncMaxOrder \:0438\:0441\:0445\:043e\:0434\:044f\:0442 \:0438\:0437 \:0431\:0438\:0431\:043b\:0438\:043e\:0442\:0435\:043a\:0438-\:0434\:0430\:043c\:043f\:0430 \:043a\:043e\:0440\:043d\:0435\:0439 *)
TableForm[Take[(Take[#, UpTo[5]] &) /@ besselRoots, UpTo[10]], TableHeadings -> {
	(StringForm["l = ``", #] &) /@ Range[radialFuncMinOrder, radialFuncMaxOrder],
	Range[1, Length[First@besselRoots]]
}]
TableForm[Take[(Take[#, UpTo[5]] &) /@ riccatiBesselPrimeRoots, UpTo[10]], TableHeadings -> {
	(StringForm["l = ``", #] &) /@ Range[radialFuncMinOrder, radialFuncMaxOrder],
	Range[1, Length[First@riccatiBesselPrimeRoots]]
}]


(* ::Subsection::Closed:: *)
(*\:0412\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:0435 \:0447\:0430\:0441\:0442\:043e\:0442\:043d\:043e\:0433\:043e \:0440\:0430\:0441\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:0438\:044f \:043c\:043e\:0434 (\:0434\:0435\:043c\:043e\:043d\:0441\:0442\:0440\:0430\:0446\:0438\:044f)*)


(* ::Text:: *)
(*\:0418\:0437\:043e\:0431\:0440\:0430\:0437\:0438\:043c \:043d\:0438\:0436\:043d\:0438\:0439 \:043b\:0435\:0432\:044b\:0439 \:0443\:0433\:043e\:043b \:0440\:0430\:0441\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:0438\:044f \:043c\:043e\:0434 \:043f\:043e \:0447\:0430\:0441\:0442\:043e\:0442\:0430\:043c*)


modes = MakeDiscreteModeDistribution[{ 1, 50 }, { 1, 50 }, 50];
ListPlot[modes, PlotRange -> Full]


(* ::Text:: *)
(*\:0412\:0438\:0434\:043d\:043e, \:0447\:0442\:043e \:0442\:043e\:043b\:044c\:043a\:043e \:0432 \:043e\:0431\:043b\:0430\:0441\:0442\:0438 w < 50 \:0440\:0435\:0437\:0443\:043b\:044c\:0442\:0430\:0442 \:044f\:0432\:043b\:044f\:0435\:0442\:0441\:044f \:043f\:043e\:043b\:043d\:044b\:043c. \:0418\:0437\:043e\:0431\:0440\:0430\:0437\:0438\:043c \:044d\:0442\:0443 \:043e\:0431\:043b\:0430\:0441\:0442\:044c.*)


cmplModes = TakeWhile[modes, (First@# < 50 &)];
ListPlot[cmplModes, PlotRange -> Full]


(* ::Text:: *)
(*\:0421\:0432\:0435\:0440\:043d\:0435\:043c \:0440\:0430\:0441\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:0438\:0435 \:043c\:043e\:0434 \:0441 \:043e\:043a\:043d\:043e\:043c \:0438 \:0438\:0437\:043e\:0431\:0440\:0430\:0437\:0438\:043c \:0440\:0435\:0437\:0443\:043b\:044c\:0442\:0430\:0442. \:0428\:0438\:0440\:0438\:043d\:0430 \:043e\:043a\:043d\:0430 \:043c\:043e\:0436\:0435\:0442 \:0431\:044b\:0442\:044c \:043e\:0446\:0435\:043d\:0435\:043d\:0430 \:0438\:0437 \:0443\:0441\:043b\:043e\:0432\:0438\:044f \:043f\:043e\:043f\:0430\:0434\:0430\:043d\:0438\:044f \:043f\:0440\:0438\:043c\:0435\:0440\:043d\:043e 10-30 \:0440\:0430\:0437\:043b\:0438\:0447\:043d\:044b\:0445 \:0447\:0430\:0441\:0442\:043e\:0442 \:0432 \:043e\:0431\:043b\:0430\:0441\:0442\:044c \:043e\:043a\:043d\:0430.*)


conv = RectFilter[modes, 10];
ListPlot[conv, PlotRange -> Full]


(* ::Text:: *)
(*\:0412 \:043f\:043e\:043b\:0443\:0447\:0435\:043d\:043d\:043e\:043c \:0440\:0435\:0437\:0443\:043b\:044c\:0442\:0430\:0442\:0435 \:043d\:0430\:0441 \:0442\:043e\:0436\:0435 \:0438\:043d\:0442\:0435\:0440\:0435\:0441\:0443\:0435\:0442 \:043b\:0438\:0448\:044c \:043e\:0431\:043b\:0430\:0441\:0442\:044c \:043f\:043e\:043b\:043d\:044b\:0445 \:0434\:0430\:043d\:043d\:044b\:0445. \:0421 \:0443\:0447\:0435\:0442\:043e\:043c \:0448\:0438\:0440\:0438\:043d\:044b \:043e\:043a\:043d\:0430, \:0438\:0437\:043e\:0431\:0440\:0430\:0437\:0438\:043c \:044d\:0442\:0443 \:043e\:0431\:043b\:0430\:0441\:0442\:044c.*)


cmplConv = TakeWhile[conv, (First@# < 40 &)];
ListPlot[cmplConv, PlotRange -> Full]


(* ::Text:: *)
(*\:041e\:043d\:0430 \:043e\:0447\:0435\:043d\:044c \:043f\:043e\:0445\:043e\:0436\:0430 \:043d\:0430 \:043a\:0432\:0430\:0434\:0440\:0430\:0442\:0438\:0447\:043d\:0443\:044e \:0444\:0443\:043d\:043a\:0446\:0438\:044e. \:041f\:0440\:043e\:0432\:0435\:0440\:0438\:043c \:044d\:0442\:043e \:043f\:0440\:0435\:0434\:043f\:043e\:043b\:043e\:0436\:0435\:043d\:0438\:0435.*)


sqFuncCoef = TestSqFunc[]


(* ::Text:: *)
(*\:0412 \:043a\:043b\:0430\:0441\:0441\:0438\:0447\:0435\:0441\:043a\:043e\:0439 \:0442\:0435\:043e\:0440\:0438\:0438 \:043a\:043e\:044d\:0444\:0444\:0438\:0446\:0438\:0435\:043d\:0442 \:043f\:0440\:0438 x^2 \:0434\:043e\:043b\:0436\:0435\:043d \:0431\:044b\:0442\:044c \:0440\:0430\:0432\:0435\:043d 1/Pi^2. \:041e\:0434\:043d\:0430\:043a\:043e \:043c\:044b \:0442\:0430\:043a\:0436\:0435 \:0434\:043e\:043b\:0436\:043d\:044b \:043e\:0442\:043d\:0435\:0441\:0442\:0438 \:043f\:043e\:043b\:0443\:0447\:0435\:043d\:043d\:044b\:0435 \:0440\:0435\:0437\:0443\:043b\:044c\:0442\:0430\:0442\:044b \:043a 4/3 Pi r^3, \:043f\:043e\:0441\:043a\:043e\:043b\:044c\:043a\:0443 \:043d\:0430\:0441 \:0438\:043d\:0442\:0435\:0440\:0435\:0441\:0443\:0435\:0442 \:0432\:0435\:043b\:0438\:0447\:0438\:043d\:0430, \:043f\:0440\:0438\:0445\:043e\:0434\:044f\:0449\:0430\:044f\:0441\:044f \:043d\:0430 \:0435\:0434\:0438\:043d\:0438\:0446\:0443 \:043e\:0431\:044a\:0435\:043c\:0430 \:0440\:0435\:0437\:043e\:043d\:0430\:0442\:043e\:0440\:0430.*)


sqFuncCoef[[1]] / (4/3 N@Pi) * Pi^2


(* ::Text:: *)
(*\:041c\:043e\:0436\:043d\:043e \:043e\:0442\:043c\:0435\:0442\:0438\:0442\:044c \:0432\:044b\:0441\:043e\:043a\:0443\:044e \:043a\:043e\:0440\:0440\:0435\:043b\:0438\:0440\:043e\:0432\:0430\:043d\:043d\:043e\:0441\:0442\:044c \:043a\:0440\:0438\:0432\:043e\:0439 \:0438 \:0434\:0438\:0441\:043a\:0440\:0435\:0442\:043d\:044b\:0445 \:0434\:0430\:043d\:043d\:044b\:0445.*)


(* ::Text:: *)
(*\:0412 \:043e\:0431\:043b\:0430\:0441\:0442\:0438 \:0432\:0431\:043b\:0438\:0437\:0438 \:043d\:0443\:043b\:044f \:0442\:043e\:0447\:043a\:0438 \:043f\:043b\:043e\:0445\:043e \:043b\:043e\:0436\:0430\:0442\:0441\:044f \:043d\:0430 \:043d\:0435\:043f\:0440\:0435\:0440\:044b\:0432\:043d\:0443\:044e \:043a\:0440\:0438\:0432\:0443\:044e. \:041f\:043e\:043f\:0440\:043e\:0431\:0443\:0435\:043c \:0443\:043c\:0435\:043d\:044c\:0448\:0438\:0442\:044c \:0440\:0430\:0437\:043c\:0435\:0440 \:0441\:0432\:0435\:0440\:0442\:043e\:0447\:043d\:043e\:0433\:043e \:043e\:043a\:043d\:0430.*)


cmplConv = TakeWhile[RectFilter[TakeWhile[modes, (First@# < 50 &)], 5], (First@# < 40 &)];
ListPlot[cmplConv, PlotRange -> Full]


sqFuncCoef = TestSqFunc[]


(* ::Text:: *)
(*\:041c\:043e\:0436\:043d\:043e \:0432\:0438\:0434\:0435\:0442\:044c, \:0447\:0442\:043e \:0441 \:0443\:043c\:0435\:043d\:044c\:0448\:0435\:043d\:0438\:0435\:043c \:0440\:0430\:0437\:043c\:0435\:0440\:0430 \:043e\:043a\:043d\:0430 \:0443\:0432\:0435\:043b\:0438\:0447\:0438\:0432\:0430\:0435\:0442\:0441\:044f \:0440\:0430\:0437\:0431\:0440\:043e\:0441 \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0439 \:043f\:0440\:0438 \:0431\:043e\:043b\:044c\:0448\:0438\:0445 \:0447\:0430\:0441\:0442\:043e\:0442\:0430\:0445. \:041f\:0440\:0438 \:043c\:0435\:043d\:044c\:0448\:0438\:0445 \:0447\:0430\:0441\:0442\:043e\:0442\:0430\:0445 \:0442\:043e\:0447\:043a\:0438 \:043b\:0443\:0447\:0448\:0435 \:043b\:043e\:0436\:0430\:0442\:0441\:044f \:043d\:0430 \:043a\:0440\:0438\:0432\:0443\:044e. \:041d\:0435 \:0441\:043e\:0432\:0441\:0435\:043c \:0441\:043c\:043e\:0442\:0440\:0435\:0442\:044c \:043d\:0430 \:0441\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0438\:0435 \:0442\:043e\:0447\:0435\:043a \:043a\:0440\:0438\:0432\:043e\:0439 \:0432 \:043e\:0431\:043b\:0430\:0441\:0442\:0438 w < Rw/2, \:0433\:0434\:0435 Rw -- \:0448\:0438\:0440\:0438\:043d\:0430 \:043e\:043a\:043d\:0430. \:0412 \:0434\:0430\:043d\:043d\:043e\:043c \:0441\:043b\:0443\:0447\:0430\:0435 \:043c\:043e\:0436\:043d\:043e \:0433\:043e\:0432\:043e\:0440\:0438\:0442\:044c \:043e \:0431\:043b\:0438\:0437\:043e\:0441\:0442\:0438 \:0442\:043e\:0447\:0435\:043a \:043a \:043a\:0440\:0438\:0432\:043e\:0439 \:0438 \:043f\:0440\:0438 \:0441\:0430\:043c\:044b\:0445 \:043d\:0438\:0437\:043a\:0438\:0445 \:0447\:0430\:0441\:0442\:043e\:0442\:0430\:0445.*)


(* ::Subsection::Closed:: *)
(*\:0412\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:0435 \:0447\:0430\:0441\:0442\:043e\:0442\:043d\:043e\:0433\:043e \:0440\:0430\:0441\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:0438\:044f \:043c\:043e\:0434*)


(* ::Text:: *)
(*\:0412\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:0435 \:0447\:0430\:0441\:0442\:043e\:0442\:043d\:043e\:0433\:043e \:0440\:0430\:0441\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:0438\:044f*)


cmplModes = MakeDiscreteModeDistribution[{ 1, 1000 }, { 1, 1000 }, 1000];
Do[If[Length[besselRoots[[i]]] != 0, maxW = First@besselRoots[[i]]; Break[]], {i, Length[besselRoots], 1, -1}];
Do[If[Length[riccatiBesselPrimeRoots[[i]]] != 0, maxW = Min[maxW, First@riccatiBesselPrimeRoots[[i]]]; Break[]], {i, Length[riccatiBesselPrimeRoots], 1, -1}];
conv = RectFilter[cmplModes, 10];
cmplConv = TakeWhile[conv, (First@# < maxW - 10 &)];


(* ::Text:: *)
(*\:0410\:043f\:043f\:0440\:043e\:043a\:0441\:0438\:043c\:0430\:0446\:0438\:044f \:043a\:0432\:0430\:0434\:0440\:0430\:0442\:0438\:0447\:043d\:044b\:043c \:0437\:0430\:043a\:043e\:043d\:043e\:043c*)


sqFuncCoef = TestSqFunc[]


(* ::Subsection::Closed:: *)
(*\:041f\:043e\:0441\:0442\:0440\:043e\:0435\:043d\:0438\:0435 \:043f\:043b\:0430\:043d\:043a\:043e\:0432\:0441\:043a\:043e\:0439 \:043a\:0440\:0438\:0432\:043e\:0439*)


(* ::Text:: *)
(*\:041f\:043e\:043b\:0443\:0447\:0435\:043d\:043d\:043e\:0435 \:0440\:0430\:0441\:043f\:0440\:0435\:0434\:0435\:043b\:0435\:043d\:0438\:0435 \:043c\:043e\:0434 \:043f\:043e \:0447\:0430\:0441\:0442\:043e\:0442\:0430\:043c \:0438\:0441\:043f\:043e\:043b\:044c\:0437\:0443\:0435\:043c \:0434\:043b\:044f \:043f\:043e\:0441\:0442\:0440\:043e\:0435\:043d\:0438\:044f \:043f\:043b\:0430\:043d\:043a\:043e\:0432\:0441\:043a\:043e\:0439 \:043a\:0440\:0438\:0432\:043e\:0439*)


(* ::Text:: *)
(*\:041f\:043e\:0441\:0442\:0440\:043e\:0435\:043d\:0438\:0435 \:043d\:0435\:0441\:043a\:043e\:043b\:044c\:043a\:0438\:0445 \:043a\:0440\:0438\:0432\:044b\:0445 \:0434\:043b\:044f \:0440\:0430\:0437\:043d\:044b\:0445 \:0440\:0430\:0437\:043c\:0435\:0440\:043e\:0432 \:0440\:0435\:0437\:043e\:043d\:0430\:0442\:043e\:0440\:0430*)


plankTestR = {
	TestPlank[300, First@Last@cmplConv / 1000 * 0.01],
	TestPlank[300, First@Last@cmplConv / 1000 * 0.001],
	TestPlank[300, First@Last@cmplConv / 1000 * 0.0005]
};

plankTestR //TableForm


(* ::Text:: *)
(*\:041f\:043e\:0441\:0442\:0440\:043e\:0435\:043d\:0438\:0435 \:043d\:0435\:0441\:043a\:043e\:043b\:044c\:043a\:0438\:0445 \:043a\:0440\:0438\:0432\:044b\:0445 \:0434\:043b\:044f \:0440\:0430\:0437\:043d\:044b\:0445 \:0442\:0435\:043c\:043f\:0435\:0440\:0430\:0442\:0443\:0440*)


plankTestT = {
	TestPlank[First@Last@cmplConv / 1000 * 600, 0.001],
	TestPlank[First@Last@cmplConv / 1000 * 300, 0.001],
	TestPlank[First@Last@cmplConv / 1000 * 200, 0.001]
};

plankTestT //TableForm


(* ::Subsection::Closed:: *)
(*\:042d\:043a\:0441\:043f\:043e\:0440\:0442 \:0438\:0437\:043e\:0431\:0440\:0430\:0436\:0435\:043d\:0438\:0439*)


modes = MakeDiscreteModeDistribution[{ 1, 20 }, { 1, 20 }, 20];
last = MakeDiscreteModeDistribution[{ 20, 20 }, { 20, 20 }, 1];


modesPlot = {
	Pane@ListPlot[modes, AxesLabel -> { "x", "N(x)" }],
	Pane@ListPlot[TakeWhile[modes, (First@# <= Min[First@First@last, First@Last@last] &)], AxesLabel -> { "x", "N(x)" }]
}


modes = MakeDiscreteModeDistribution[{ 1, 100 }, { 1, 100 }, 100];
cmplModes = TakeWhile[modes, (First@# < 100 &)];
conv = RectFilter[cmplModes, 5];
cmplConv = TakeWhile[conv, (First@# < 100 - 5 &)];


sqFuncCoef = TestSqFunc[]


modes = MakeDiscreteModeDistribution[{ 1, 300 }, { 1, 300 }, 300];
cmplModes = TakeWhile[modes, (First@# < 300 &)];
conv = RectFilter[cmplModes, 5];
cmplConv = TakeWhile[conv, (First@# < 300 - 5 &)];


plankTestR = {
	TestPlank[300, First@Last@cmplConv / 1000 * 0.002],
	TestPlank[300, First@Last@cmplConv / 1000 * 0.001],
	TestPlank[300, First@Last@cmplConv / 1000 * 0.0005]
};

plankTestR //TableForm


modes = MakeDetailedModeDistribution[{ 1, 60 }, 60];
cmplModes = TakeWhile[Table[TakeWhile[modes[[i]], (First@# < 60 &)], {i, Length[modes]}], (# != {} &)];


plankTestSmallT = Table[DetailedTestPlank[i, 20 * 10^(-6), 1, 5], {i, {50, 100, 200, 300}}];
plankTestSmallT
plankTestSmallTImage = (Legended[Show[#[[1,1]], ImageSize -> {800, 600} / 1.5], #[[1,2]]] &) /@ plankTestSmallT;


Quiet@CreateDirectory[FileNameJoin[{NotebookDirectory[], "dist"}]];
Export[FileNameJoin[{NotebookDirectory[], "dist", "n.png"}],        modesPlot [[1,1]],   Background -> None, ImageSize -> {800, 600}];
Export[FileNameJoin[{NotebookDirectory[], "dist", "n_full.png"}],   modesPlot [[2,1]],   Background -> None, ImageSize -> {800, 600}];
Export[FileNameJoin[{NotebookDirectory[], "dist", "dndx.png"}],     sqFuncCoef[[3,1,1]], Background -> None, ImageSize -> {800, 600}];
Export[FileNameJoin[{NotebookDirectory[], "dist", "dndx_mag.png"}], sqFuncCoef[[3,2,1]], Background -> None, ImageSize -> {800, 600}];
Export[FileNameJoin[{NotebookDirectory[], "dist", "plank.png"}],    plankTestR[[2,1,1]], Background -> None, ImageSize -> {800, 600}];
Export[FileNameJoin[{NotebookDirectory[], "dist", "plank_small_t_1.png"}], plankTestSmallTImage[[1]], Background -> None, ImageResolution -> 96];
Export[FileNameJoin[{NotebookDirectory[], "dist", "plank_small_t_2.png"}], plankTestSmallTImage[[2]], Background -> None, ImageResolution -> 96];
Export[FileNameJoin[{NotebookDirectory[], "dist", "plank_small_t_3.png"}], plankTestSmallTImage[[3]], Background -> None, ImageResolution -> 96];
Export[FileNameJoin[{NotebookDirectory[], "dist", "plank_small_t_4.png"}], plankTestSmallTImage[[4]], Background -> None, ImageResolution -> 96];
