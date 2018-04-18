(* ::Package:: *)

(* ::Section::Closed:: *)
(*\:0418\:043c\:043f\:043e\:0440\:0442 \:0431\:0438\:0431\:043b\:0438\:043e\:0442\:0435\:043a*)


(* ::Input::Initialization:: *)
If[$FrontEnd =!= Null, AppendTo[$Path, FileNameJoin[{NotebookDirectory[], "..\\..\\lib\\mathematica"}]]];

ImportLibs := (Once@Get[#] &) /@ { "BesselZeros`" };

ImportLibs;


(* ::Section::Closed:: *)
(*\:0418\:043d\:0438\:0446\:0438\:0430\:043b\:0438\:0437\:0430\:0446\:0438\:044f \:0432\:044b\:0447\:0438\:0441\:043b\:0438\:0442\:0435\:043b\:044c\:043d\:044b\:0445 \:0443\:0437\:043b\:043e\:0432*)


(* ::Input::Initialization:: *)
LaunchKernels[];
Block[{$path = $Path}, DistributeDefinitions[$path]; ParallelEvaluate[$Path = $path]];
ParallelEvaluate[ImportLibs];


(* ::Section::Closed:: *)
(*\:0412\:0441\:043f\:043e\:043c\:043e\:0433\:0430\:0442\:0435\:043b\:044c\:043d\:044b\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0438*)


(* ::Text:: *)
(*\:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 n \:043a\:043e\:0440\:043d\:0435\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0411\:0435\:0441\:0441\:0435\:043b\:044f \:043f\:043e\:043b\:0443\:0446\:0435\:043b\:043e\:0433\:043e \:043f\:043e\:0440\:044f\:0434\:043a\:0430*)


(* ::Text:: *)
(*\:041e\:0441\:0443\:0449\:0435\:0441\:0442\:0432\:043b\:044f\:0435\:0442\:0441\:044f \:0447\:0435\:0440\:0435\:0437 \:0443\:0442\:043e\:0447\:043d\:0435\:043d\:0438\:0435 \:043a\:043e\:0440\:043d\:0435\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0411\:0435\:0441\:0441\:0435\:043b\:044f \:043f\:0440\:0435\:0434\:044b\:0434\:0443\:0449\:0435\:0433\:043e \:043f\:043e\:0440\:044f\:0434\:043a\:0430. \:041a\:043e\:0440\:043d\:0438 \:0434\:043b\:044f \:0432\:043c\:0435\:043d\:044f\:0435\:043c\:044b\:0445 \:043f\:043e\:0440\:044f\:0434\:043a\:043e\:0432 \:043e\:0442\:043b\:0438\:0447\:0430\:044e\:0442\:0441\:044f \:043d\:0430 \:0432\:0435\:043b\:0438\:0447\:0438\:043d\:0443 \:0432 \:0434\:0438\:0430\:043f\:0430\:0437\:043e\:043d\:0435 ~Pi/4..Pi/2, \:043f\:0440\:0438\:0447\:0435\:043c \:043f\:0440\:0430\:0432\:0430\:044f \:0433\:0440\:0430\:043d\:0438\:0446\:0430 \:0441\:043b\:0435\:0434\:0443\:0435\:0442 \:0438\:0437 \:0430\:0441\:0438\:043c\:043f\:0442\:043e\:0442\:0438\:043a\:0438. \:0412 \:043a\:0430\:0447\:0435\:0441\:0442\:0432\:0435 \:043d\:0430\:0447\:0430\:043b\:044c\:043d\:043e\:0433\:043e \:043f\:0440\:0438\:0431\:043b\:0438\:0436\:0435\:043d\:0438\:044f \:043f\:0440\:0435\:043a\:0440\:0430\:0441\:043d\:043e \:0441\:0433\:043e\:0434\:044f\:0442\:0441\:044f \:043a\:043e\:0440\:043d\:0438 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0411\:0435\:0441\:0441\:0435\:043b\:044f \:0446\:0435\:043b\:043e\:0433\:043e \:043f\:043e\:0440\:044f\:0434\:043a\:0430.*)


BesselJHalfIntegerOrderZeros::prec = "Insufficient WorkingPrecision selected, TargetEpsilon reached.";
Options[BesselJHalfIntegerOrderZeros] = {
	WorkingPrecision -> MachinePrecision,
	AccuracyGoal -> 6,
	TargetEpsilon -> 10^(-4),
	EvaluationMonitor -> None
};
BesselJHalfIntegerOrderZeros[l_?IntegerQ, n_?IntegerQ, opts:OptionsPattern[]] := Module[{fn, roots},
	roots = BesselJZeros[l, n, opts];
	Return[BesselJHalfIntegerOrderZeros[l, roots, opts]];
];
BesselJHalfIntegerOrderZeros[l_?IntegerQ, roots_?VectorQ, opts:OptionsPattern[]] := Module[{fn, roots0, wp, ag, te, err},
	wp = OptionValue[WorkingPrecision];
	ag = OptionValue[AccuracyGoal];
	te = OptionValue[TargetEpsilon];
	fn[r_] := N[BesselJ[l + 1 / 2, r], wp];
	err = False;
	Quiet[
		SetSharedVariable[err];
		roots0 = ParallelMap[(Check[ r /. FindRoot[fn[r], {r, # + Pi / 4, # + Pi / 2},
					WorkingPrecision -> wp, AccuracyGoal -> ag, MaxIterations -> 35], err = True] &), roots];
		UnsetShared[err];
	, { ParallelMap::subpar, SetSharedVariable::subnopar, UnsetShared::subnopar }];
	If[err || Abs[fn[roots0[[1]]]] > te,
			Message[BesselJHalfIntegerOrderZeros::prec]; Return[$Failed]];
	Return[roots0];
];
BesselJHalfIntegerOrderZeros[{ l0_, l1_ }, n_?(IntegerQ[#]||VectorQ[#]&), maxr_?NumberQ, opts:OptionsPattern[]] := Module[{fn, roots, lf},
	roots = ConstantArray[{}, l1 - l0 + 1];
	roots[[1]] = BesselJHalfIntegerOrderZeros[l0, n, opts];
	roots[[1]] = TakeWhile[roots[[1]], (# <= maxr &)];
	If[roots[[1]] === $Failed, Return[{}]];
	If[Length[roots[[1]]] == 0, Return[{}]];
	OptionValue[EvaluationMonitor];
	lf = l0 - 1;
	Do[
		roots[[l - l0 + 1]] = BesselJHalfIntegerOrderZeros[l, roots[[l - l0]], opts];
		If[roots[[l - l0 + 1]] === $Failed, lf = l; Break[]];
		roots[[l - l0 + 1]] = TakeWhile[roots[[l - l0 + 1]], (# <= maxr &)];
		If[Length[roots[[l - l0 + 1]]] == 0, lf = l; Break[]];
		OptionValue[EvaluationMonitor];
	, {l, l0 + 1, l1}];
	If[lf >= l0, Return[Take[roots, lf - l0 + 1]]];
	Return[roots];
];


(* ::Text:: *)
(*\:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 n \:043a\:043e\:0440\:043d\:0435\:0439 \:043f\:0440\:043e\:0438\:0437\:0432\:043e\:0434\:043d\:043e\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0420\:0438\:043a\:043a\:0430\:0442\:0438-\:0411\:0435\:0441\:0441\:0435\:043b\:044f*)


(* ::Text:: *)
(*\:041e\:0441\:0443\:0449\:0435\:0441\:0442\:0432\:043b\:044f\:0435\:0442\:0441\:044f \:0447\:0435\:0440\:0435\:0437 \:0443\:0442\:043e\:0447\:043d\:0435\:043d\:0438\:0435 \:043a\:043e\:0440\:043d\:0435\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0420\:0438\:043a\:043a\:0430\:0442\:0438-\:0411\:0435\:0441\:0441\:0435\:043b\:044f \:043f\:0440\:0435\:0434\:044b\:0434\:0443\:0449\:0435\:0433\:043e \:043f\:043e\:0440\:044f\:0434\:043a\:0430. \:041a\:043e\:0440\:043d\:0438 \:0434\:043b\:044f \:0432\:043c\:0435\:043d\:044f\:0435\:043c\:044b\:0445 \:043f\:043e\:0440\:044f\:0434\:043a\:043e\:0432 \:043e\:0442\:043b\:0438\:0447\:0430\:044e\:0442\:0441\:044f \:043d\:0430 \:0432\:0435\:043b\:0438\:0447\:0438\:043d\:0443 \:0432 \:0434\:0438\:0430\:043f\:0430\:0437\:043e\:043d\:0435 ~Pi/4..Pi/2, \:043f\:0440\:0438\:0447\:0435\:043c \:043f\:0440\:0430\:0432\:0430\:044f \:0433\:0440\:0430\:043d\:0438\:0446\:0430 \:0441\:043b\:0435\:0434\:0443\:0435\:0442 \:0438\:0437 \:0430\:0441\:0438\:043c\:043f\:0442\:043e\:0442\:0438\:043a\:0438. \:0412 \:043a\:0430\:0447\:0435\:0441\:0442\:0432\:0435 \:043d\:0430\:0447\:0430\:043b\:044c\:043d\:043e\:0433\:043e \:043f\:0440\:0438\:0431\:043b\:0438\:0436\:0435\:043d\:0438\:044f \:043f\:0440\:0435\:043a\:0440\:0430\:0441\:043d\:043e \:0441\:0433\:043e\:0434\:044f\:0442\:0441\:044f \:043a\:043e\:0440\:043d\:0438 \:043f\:0440\:043e\:0438\:0437\:0432\:043e\:0434\:043d\:043e\:0439 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0411\:0435\:0441\:0441\:0435\:043b\:044f \:0446\:0435\:043b\:043e\:0433\:043e \:043f\:043e\:0440\:044f\:0434\:043a\:0430.*)


RiccatiBesselJPrimeZeros::prec = "Insufficient WorkingPrecision selected, TargetEpsilon reached.";
Options[RiccatiBesselJPrimeZeros] = {
	WorkingPrecision -> MachinePrecision,
	AccuracyGoal -> 6,
	TargetEpsilon -> 10^(-4),
	EvaluationMonitor -> None
};
RiccatiBesselJPrimeZeros[l_?IntegerQ, n_?IntegerQ, opts:OptionsPattern[]] := Module[{fn, roots},
	roots = BesselJPrimeZeros[l, n, opts];
	Return[RiccatiBesselJPrimeZeros[l, roots, opts]];
];
RiccatiBesselJPrimeZeros[l_?IntegerQ, roots_?VectorQ, opts:OptionsPattern[]] := Module[{fn, roots0, wp, ag, err},
	wp = OptionValue[WorkingPrecision];
	ag = OptionValue[AccuracyGoal];
	te = OptionValue[TargetEpsilon];
	fn[r_] := N[D[o SphericalBesselJ[l, o], o] /. o -> r, wp];
	err = False;
	Quiet[
		SetSharedVariable[err];
		roots0 = ParallelMap[(Check[ r /. FindRoot[fn[r], {r, # + Pi / 4, # + Pi / 2},
					WorkingPrecision -> wp, AccuracyGoal -> ag, MaxIterations -> 35], err = True] &), roots];
		UnsetShared[err];
	, { ParallelMap::subpar, SetSharedVariable::subnopar, UnsetShared::subnopar }];
	If[err || Abs[fn[roots0[[1]]]] > te,
			Message[RiccatiBesselJPrimeZeros::prec]; Return[$Failed]];
	Return[roots0];
];
RiccatiBesselJPrimeZeros[{ l0_, l1_ }, n_?(IntegerQ[#]||VectorQ[#]&), maxr_?NumberQ, opts:OptionsPattern[]] := Module[{fn, roots, lf},
	roots = ConstantArray[{}, l1 - l0 + 1];
	roots[[1]] = RiccatiBesselJPrimeZeros[l0, n, opts];
	If[roots[[1]] === $Failed, Return[{}]];
	roots[[1]] = TakeWhile[roots[[1]], (# <= maxr &)];
	If[Length[roots[[1]]] == 0, Return[{}]];
	OptionValue[EvaluationMonitor];
	Do[
		roots[[l - l0 + 1]] = RiccatiBesselJPrimeZeros[l, roots[[l - l0]], opts];
		If[roots[[l - l0 + 1]] === $Failed, lf = l; Break[]];
		roots[[l - l0 + 1]] = TakeWhile[roots[[l - l0 + 1]], (# <= maxr &)];
		If[Length[roots[[l - l0 + 1]]] == 0, lf = l; Break[]];
		OptionValue[EvaluationMonitor];
	, {l, l0 + 1, l1}];
	If[lf >= l0, Return[Take[roots, lf - l0 + 1]]];
	Return[roots];
];


(* ::Section::Closed:: *)
(*\:0422\:0435\:0441\:0442\:0438\:0440\:043e\:0432\:0430\:043d\:0438\:0435 \:0432\:0441\:043f\:043e\:043c\:043e\:0433\:0430\:0442\:0435\:043b\:044c\:043d\:044b\:0445 \:0444\:0443\:043d\:043a\:0446\:0438\:0439*)


Options[BesselJZerosTest] = {
	WorkingPrecision -> MachinePrecision,
	AccuracyGoal -> Automatic,
	EvaluationMonitor -> None
};
BesselJZerosTest[tfn_, { l0_, l1_ }, n_?IntegerQ, maxr_?NumberQ, opts:OptionsPattern[]] := Module[{fn, roots, roots0, plt, plts, err},
	If[tfn === BesselJHalfIntegerOrderZeros, fn = BesselJ[l + 1/2, r]];
	If[tfn === RiccatiBesselJPrimeZeros, fn = D[r SphericalBesselJ[l, r], r]];
	roots = tfn[{ l0, l1 }, n, maxr, opts];
	err = Null;
	plts = Table[
		roots0 = roots[[l - l0 + 1]];
		plt = Show[{
			Plot[fn, {r, 0, Last[roots0]}, PlotRange -> Full, PerformanceGoal -> "Speed"],
			ListPlot[({#, fn /. r -> #} &) /@ roots0, Filling -> Axis, Joined -> True, PlotRange -> Full, PlotStyle -> Red],
			ListPlot[({#, fn /. r -> #} &) /@ roots0, PlotStyle -> { Red, PointSize[0.03] }]
		}];
		Do[If[roots0[[i + 1]] - roots0[[i]] < Pi / 4, err = $Failed; Break[]], {i, n - 1}];
		plt
	, {l, l0, l1}];
	If[err === $Failed, Return[$Failed]];
	Return[plts];
];


DoBesselJZerosTest[fn_, monitor_] := Module[{res, iter = 0, secs = 0, time, plots},
	time = TimeUsed[];
	monitor[{iter, secs}];
	res = BesselJZerosTest[fn, {2, 100}, 10, 300,
		EvaluationMonitor :> (iter++;secs=TimeUsed[]-time;monitor[{iter, secs}]), WorkingPrecision -> 30];
	plots = Pane /@ res;
	Return[plots];
];
ShowPlots[plots_] := Manipulate[Take[plots, {i, i + 3}], {i, 1, Length[plots] - 3, 1}];


Block[{s1 = {0, 0}, s2 = {0, 0}},
	PrintTemporary[Dynamic[s1], " / ", Dynamic[s2]];
	SetSharedVariable[s1, s2];
	Print @ (ShowPlots /@ Parallelize[{
		DoBesselJZerosTest[BesselJHalfIntegerOrderZeros, ((s1=#)&)],
		DoBesselJZerosTest[RiccatiBesselJPrimeZeros, ((s2=#)&)]
	}, Method -> "ItemsPerEvaluation" -> 1]);
	UnsetShared[s1, s2];
]


(* ::Section:: *)
(*\:0412\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:044f*)


(* ::Subsection::Closed:: *)
(*\:0412\:0441\:043f\:043e\:043c\:043e\:0433\:0430\:0442\:0435\:043b\:044c\:043d\:044b\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0438*)


GenerateRadialFunctionZeros::prec = "Insufficient WorkingPrecision (``) selected, continue with 1.2 * WorkingPrecision = ``.";
GenerateRadialFunctionZeros::canc = "Increasing WorkingPrecision has no effect.";
GenerateRadialFunctionZeros[fn_, { l1_, l2_ }, n_, maxr_, monitor_] := Module[{iter = 0, secs = 0, time, roots, eval, wp, l3, err},
	time = SessionTime[];
	monitor[{iter, secs}];
	wp = OptionValue[fn, WorkingPrecision];
	eval[l0_] := fn[{ l0, l2 }, n, maxr, EvaluationMonitor :> (iter++;secs=SessionTime[]-time;monitor[{iter, secs}]), WorkingPrecision -> wp];
	roots = eval[l1];
	err = False;
	While[Length[roots] == 0 || Last@roots === $Failed,
		If[wp == MachinePrecision, wp = Round[$MachinePrecision]];
		If[wp > 1000, Message[GenerateRadialFunctionZeros::canc]; err = True; Break[]];
		Message[GenerateRadialFunctionZeros::prec, wp, IntegerPart[Ceiling[wp * 1.2]]];
		wp = IntegerPart[Ceiling[wp * 1.2]];
		l3 = l1 + Length[roots] - 1;
		roots = Flatten[{ Take[roots, Length[roots] - 1], eval[l3] }, 1];
	];
	If[err, Return[$Failed]];
	Return[roots];
];


(* ::Subsection::Closed:: *)
(*\:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 \:043d\:0443\:043b\:0435\:0439 \:0440\:0430\:0434\:0438\:0430\:043b\:044c\:043d\:044b\:0445 \:0444\:0443\:043d\:043a\:0446\:0438\:0439*)


$radialFuncMinOrder = 1;
$radialFuncMaxOrder = 1000;
$radialFuncMaxRoot  = 1000;
$radialFuncMaxR     = 1200;
$outputFilename = "BesselRoots.m";

Off[General::stop];

Block[{s1 = {0, 0}, s2 = {0, 0}},
	PrintTemporary[Dynamic[s1], " / ", Dynamic[s2]];
	SetSharedVariable[s1, s2];
	{ radialFuncMinOrder, radialFuncMaxOrder } = { $radialFuncMinOrder, $radialFuncMaxOrder };
	{ besselRoots, riccatiBesselPrimeRoots } = Parallelize[{
		GenerateRadialFunctionZeros[
			BesselJHalfIntegerOrderZeros, { radialFuncMinOrder, radialFuncMaxOrder }, $radialFuncMaxRoot, $radialFuncMaxR, ((s1=#)&)],
		GenerateRadialFunctionZeros[
			RiccatiBesselJPrimeZeros, { radialFuncMinOrder, radialFuncMaxOrder }, $radialFuncMaxRoot, $radialFuncMaxR, ((s2=#)&)]
	}, Method->"ItemsPerEvaluation" -> 1];
	UnsetShared[s1, s2];
];

On[General::stop];

Save[FileNameJoin[{NotebookDirectory[], $outputFilename}], {
	radialFuncMinOrder, radialFuncMaxOrder, besselRoots, riccatiBesselPrimeRoots
}];

TableForm[Take[(Take[#, UpTo[5]] &) /@ besselRoots, UpTo[10]], TableHeadings -> {
	(StringForm["l = ``", #] &) /@ Range[$radialFuncMinOrder, 10],
	Range[1, 5]
}]
TableForm[Take[(Take[#, UpTo[5]] &) /@ riccatiBesselPrimeRoots, UpTo[10]], TableHeadings -> {
	(StringForm["l = ``", #] &) /@ Range[$radialFuncMinOrder, 10],
	Range[1, 5]
}]
