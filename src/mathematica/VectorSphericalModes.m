(* ::Package:: *)

(* ::Section::Closed:: *)
(*\:0418\:043c\:043f\:043e\:0440\:0442 \:0431\:0438\:0431\:043b\:0438\:043e\:0442\:0435\:043a*)


(* ::Input::Initialization:: *)
Quiet[NotebookEvaluate[FileNameJoin[{NotebookDirectory[],"..\\..\\lib\\mathematica\\LibGet.m"}]]];

ImportLibs[{ "Riemannian", "Killing" }, LibRoot -> $LibRoot];


(* ::Section::Closed:: *)
(*\:0412\:0441\:043f\:043e\:043c\:043e\:0433\:0430\:0442\:0435\:043b\:044c\:043d\:044b\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0438*)


SimplifyAsEquation[e_, nzero_ : {}] := Module[{e0},
	e0 = Simplify[e == 0, Thread[nzero != 0]];
	Return[e0[[1]] - e0[[2]] //Simplify];
];


(* ::Section:: *)
(*\:0412\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:044f*)


(* ::Subsection::Closed:: *)
(*\:041f\:043e\:043b\:044f\:0440\:0438\:0437\:0430\:0446\:0438\:0438 \:0432\:0435\:043a\:0442\:043e\:0440\:043d\:044b\:0445 \:043c\:043e\:0434*)


Spher;


{
	{ 1, a0 = { a1[r,u], a2[r,u], a3[r,u] }; a0 // MatrixForm, " -> ", LapV[a0] /. m -> 0 // MatrixForm },
	{ 2, a0 = { a1[r,u],       0,       0 }; a0 // MatrixForm, " -> ", LapV[a0] /. m -> 0 // MatrixForm },
	{ 3, a0 = {       0, a2[r,u],       0 }; a0 // MatrixForm, " -> ", LapV[a0] /. m -> 0 // MatrixForm },
	{ 4, a0 = {       0,       0, a3[r,u] }; a0 // MatrixForm, " -> ", LapV[a0] /. m -> 0 // MatrixForm },
	{ 5, a0 = { a1[r,u], a2[r,u],       0 }; a0 // MatrixForm, " -> ", LapV[a0] /. m -> 0 // MatrixForm },
	{ 6, a0 = { a1[r,u],       0, a3[r,u] }; a0 // MatrixForm, " -> ", LapV[a0] /. m -> 0 // MatrixForm },
	{ 7, a0 = {       0, a2[r,u], a3[r,u] }; a0 // MatrixForm, " -> ", LapV[a0] /. m -> 0 // MatrixForm }
} // TableForm


(* ::Text:: *)
(*\:0412\:0438\:0434\:043d\:043e, \:0447\:0442\:043e \:0434\:043b\:044f \:0431\:0430\:0437\:043e\:0432\:043e\:0439 \:043c\:043e\:0434\:044b \:0440\:0435\:0430\:043b\:0438\:0437\:0443\:0435\:043c\:044b \:043b\:0438\:0448\:044c \:043a\:043e\:043d\:0444\:0438\:0433\:0443\:0440\:0430\:0446\:0438\:0438 4 \:0438 5. \:041a\:043e\:043d\:0444\:0438\:0433\:0443\:0440\:0430\:0446\:0438\:044f 1 -- \:0438\:0445 \:0441\:0443\:043f\:0435\:0440\:043f\:043e\:0437\:0438\:0446\:0438\:044f.*)


(* ::Subsection::Closed:: *)
(*\:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 \:0443\:0433\:043b\:043e\:0432\:044b\:0445 \:0447\:0430\:0441\:0442\:0435\:0439 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0445 \:043c\:043e\:0434*)


Spher;


(* ::Text:: *)
(*\:0423\:0433\:043b\:043e\:0432\:044b\:0435 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0435 \:0431\:0430\:0437\:043e\:0432\:044b\:0435 \:043c\:043e\:0434\:044b \:0440\:0430\:0437\:043b\:0438\:0447\:043d\:044b\:0445 \:043f\:043e\:043b\:044f\:0440\:0438\:0437\:0430\:0446\:0438\:0439. \:0411\:0430\:0437\:043e\:0432\:0430\:044f \:043c\:043e\:0434\:0430 \:043d\:0435 \:0437\:0430\:0432\:0438\:0441\:0438\:0442 \:043e\:0442 w, \:043f\:043e\:0441\:043a\:043e\:043b\:044c\:043a\:0443 \:0443\:0434\:043e\:0432\:043b\:0435\:0442\:0432\:043e\:0440\:044f\:0435\:0442 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:044e udzSpherOp[[3]] @ angleSpherMode == 0. \:0417\:0430\:0432\:0438\:0441\:0438\:043c\:043e\:0441\:0442\:044c \:043e\:0442 r \:043f\:043e\:043a\:0430 \:043d\:0435 \:0440\:0430\:0441\:0441\:043c\:0430\:0442\:0440\:0438\:0432\:0430\:0435\:0442\:0441\:044f.*)


angleSpherModeVectIUnk = { f1[u], f2[u], 0 };
angleSpherModeVectIIUnk = { 0, 0, f3[u] };


(* ::Text:: *)
(*\:0414\:0438\:0444\:0444\:0435\:0440\:0435\:043d\:0446\:0438\:0430\:043b\:044c\:043d\:043e\:0435 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435 \:043d\:0430 \:0431\:0430\:0437\:043e\:0432\:044b\:0435 \:0443\:0433\:043b\:043e\:0432\:044b\:0435 \:043c\:043e\:0434\:044b*)


angleSpherModeVectIDE =
(
	udzSpherOp[[1]] @ udzSpherOp[[2]] @ angleSpherModeVectIUnk
	- (- l (l + 1) angleSpherModeVectIUnk)
) // FullSimplify;
angleSpherModeVectIIDE =
(
	udzSpherOp[[1]] @ udzSpherOp[[2]] @ angleSpherModeVectIIUnk
	- (- l (l + 1) angleSpherModeVectIIUnk)
) // FullSimplify;

angleSpherModeVectIDE = Table[angleSpherModeVectIDE[[i]] == 0, {i, 1, 2}];
angleSpherModeVectIIDE = angleSpherModeVectIIDE[[3]] == 0;

angleSpherModeVectIDE // MatrixForm
angleSpherModeVectIIDE // MatrixForm


(* ::Text:: *)
(*\:0423\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:044f \:0440\:0430\:0441\:0446\:0435\:043f\:043b\:044f\:044e\:0442\:0441\:044f \:0438 \:043c\:043e\:0433\:0443\:0442 \:0440\:0435\:0448\:0430\:0442\:044c\:0441\:044f \:043e\:0442\:0434\:0435\:043b\:044c\:043d\:043e. \:041e\:0434\:043d\:0430\:043a\:043e \:0440\:0435\:0448\:0435\:043d\:0438\:0435 \:0442\:0440\:0435\:0442\:044c\:0435\:0433\:043e \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:044f \:043f\:0440\:0435\:0434\:0441\:0442\:0430\:0432\:043b\:044f\:0435\:0442 \:0442\:0440\:0443\:0434\:043d\:043e\:0441\:0442\:0438. \:0418\:0441\:0445\:043e\:0434\:044f \:0438\:0437 \:0432\:0438\:0434\:0430 \:043e\:043f\:0435\:0440\:0430\:0442\:043e\:0440\:043e\:0432 \:041a\:0438\:043b\:043b\:0438\:043d\:0433\:0430 \:043c\:043e\:0436\:043d\:043e \:0434\:043e\:0431\:0438\:0442\:044c\:0441\:044f \:0441\:0438\:043c\:043c\:0435\:0442\:0440\:0438\:0438 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0439, \:044f\:0432\:043d\:043e \:0432\:044b\:0434\:0435\:043b\:044f\:044f \:0432 f3[u] \:043d\:0435\:043a\:043e\:0442\:043e\:0440\:044b\:0439 \:043c\:043d\:043e\:0436\:0438\:0442\:0435\:043b\:044c: f3[u] -> f3[u] s[u]. \:041d\:0430\:0439\:0434\:0435\:043c \:044d\:0442\:043e\:0442 \:043c\:043d\:043e\:0436\:0438\:0442\:0435\:043b\:044c, \:043f\:043e\:0434\:0435\:0439\:0441\:0442\:0432\:043e\:0432\:0430\:0432 \:043d\:0430 \:0431\:0430\:0437\:043e\:0432\:044b\:0435 \:043c\:043e\:0434\:044b \:0440\:0430\:0437\:043b\:0438\:0447\:043d\:044b\:0445 \:043f\:043e\:043b\:044f\:0440\:0438\:0437\:0430\:0446\:0438\:0439 \:043e\:043f\:0435\:0440\:0430\:0442\:043e\:0440\:043e\:043c \:043f\:043e\:0432\:044b\:0448\:0435\:043d\:0438\:044f:*)


angleSpherModeVectIIUnk = { 0, 0, f3[u] s[u] };

angleSpherModeVectIUnkUp = udzSpherOp[[1]] @ angleSpherModeVectIUnk // FullSimplify;
angleSpherModeVectIIUnkUp = udzSpherOp[[1]] @ angleSpherModeVectIIUnk // FullSimplify;

angleSpherModeVectIUnkUp // MatrixForm
angleSpherModeVectIIUnkUp // MatrixForm


(* ::Text:: *)
(*\:0412\:0438\:0434\:043d\:043e, \:0447\:0442\:043e \:0444\:043e\:0440\:043c\:0430 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0439 \:0431\:0443\:0434\:0435\:0442 \:0441\:0438\:043c\:043c\:0435\:0442\:0440\:0438\:0447\:043d\:043e\:0439, \:0435\:0441\:043b\:0438 \:043c\:043d\:043e\:0436\:0438\:0442\:0435\:043b\:044c \:043f\:0435\:0440\:0435\:0434 f3[u] \:0431\:0443\:0434\:0435\:0442 \:0440\:0430\:0432\:0435\:043d \:043d\:0443\:043b\:044e.*)


angleSpherModeVectUnkSDE = D[angleSpherModeVectIIUnkUp[[3]], f3[u]] /. w -> 0;

angleSpherModeVectUnkSSolution = DSolve[angleSpherModeVectUnkSDE == 0, s[u], u] // First


(* ::Text:: *)
(*\:041a\:043e\:043d\:0441\:0442\:0430\:043d\:0442\:0443 \:043f\:043e\:043b\:043e\:0436\:0438\:043c \:0440\:0430\:0432\:043d\:043e\:0439 i.*)


angleSpherModeVectIIUnk = angleSpherModeVectIIUnk /. angleSpherModeVectUnkSSolution /. C[_] -> I;

angleSpherModeVectIIUnk // MatrixForm


(* ::Text:: *)
(*\:0414\:0438\:0444\:0444\:0435\:0440\:0435\:043d\:0446\:0438\:0430\:043b\:044c\:043d\:043e\:0435 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435 \:043d\:0430 \:0431\:0430\:0437\:043e\:0432\:044b\:0435 \:0443\:0433\:043b\:043e\:0432\:044b\:0435 \:043c\:043e\:0434\:044b II-\:043f\:043e\:043b\:044f\:0440\:0438\:0437\:0430\:0446\:0438\:0438*)


angleSpherModeVectIIDE =
(
	udzSpherOp[[1]] @ udzSpherOp[[2]] @ angleSpherModeVectIIUnk
	- (- l (l + 1) angleSpherModeVectIIUnk)
) // FullSimplify;

angleSpherModeVectIIDE = angleSpherModeVectIIDE[[3]] == 0;

angleSpherModeVectIIDE // MatrixForm


(* ::Text:: *)
(*\:041f\:043e\:043b\:0443\:0447\:0435\:043d\:043d\:043e\:0435 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435 \:043f\:043e\:0432\:0442\:043e\:0440\:044f\:0435\:0442 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435 \:043d\:0430 f2[u] \:043d\:0430 I-\:043f\:043e\:043b\:044f\:0440\:0438\:0437\:043e\:0432\:0430\:043d\:043d\:044b\:0435 \:043c\:043e\:0434\:044b.*)


(* ::Text:: *)
(*\:041f\:043e\:0441\:043a\:043e\:043b\:044c\:043a\:0443 \:0431\:0430\:0437\:043e\:0432\:044b\:0435 \:0441\:0440\:0435\:0434\:0441\:0442\:0432\:0430 Wolfram \:043d\:0435 \:043f\:043e\:0437\:0432\:043e\:043b\:044f\:044e\:0442 \:0440\:0430\:0437\:0440\:0435\:0448\:0438\:0442\:044c \:043e\:0434\:043d\:043e \:0438\:0437 \:043f\:043e\:043b\:0443\:0447\:0435\:043d\:043d\:044b\:0445 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0439, \:043d\:0435 \:043f\:0440\:0438\:0431\:0435\:0433\:0430\:044f \:043a \:0433\:0438\:043f\:0435\:0440\:0433\:0435\:043e\:043c\:0435\:0442\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:043c \:0444\:0443\:043d\:043a\:0446\:0438\:044f\:043c \:043e\:0431\:0449\:0435\:0433\:043e \:0432\:0438\:0434\:0430, \:0432\:043e\:0441\:043f\:043e\:043b\:044c\:0437\:0443\:0435\:043c\:0441\:044f \:0437\:043d\:0430\:043d\:0438\:044f\:043c\:0438 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0439 \:043c\:0430\:0442\:0444\:0438\:0437\:0438\:043a\:0438 \:0438 \:0440\:0435\:0437\:0443\:043b\:044c\:0442\:0430\:0442\:043e\:043c, \:043f\:043e\:043b\:0443\:0447\:0435\:043d\:043d\:044b\:043c \:0434\:043b\:044f \:0441\:043a\:0430\:043b\:044f\:0440\:043d\:044b\:0445 \:043c\:043e\:0434. \:041f\:0435\:0440\:0432\:043e\:0435 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435 \:0443\:0436\:0435 \:0437\:043d\:0430\:043a\:043e\:043c\:043e \:043d\:0430\:043c \:0438 \:043f\:0440\:0435\:0434\:0441\:0442\:0430\:0432\:043b\:044f\:0435\:0442 \:0441\:043e\:0431\:043e\:0439 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435 \:043d\:0430 P- \:0438 Q-\:043f\:043e\:043b\:0438\:043d\:043e\:043c\:044b \:041b\:0435\:0436\:0430\:043d\:0434\:0440\:0430. \:0412\:0442\:043e\:0440\:043e\:0435 \:043f\:0440\:0435\:0434\:0441\:0442\:0430\:0432\:043b\:044f\:0435\:0442 \:0441\:043e\:0431\:043e\:0439 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435 \:043d\:0430 \:043f\:0440\:0438\:0441\:043e\:0435\:0434\:0438\:043d\:0435\:043d\:043d\:044b\:0435 P- \:0438 Q-\:043f\:043e\:043b\:0438\:043d\:043e\:043c\:044b \:041b\:0435\:0436\:0430\:043d\:0434\:0440\:0430 \:0441 m = 1. Q-\:043f\:043e\:043b\:0438\:043d\:043e\:043c\:044b \:043d\:0435 \:0440\:0435\:0433\:0443\:043b\:044f\:0440\:043d\:044b \:0432 \:043d\:0443\:043b\:0435, \:043f\:043e\:0442\:043e\:043c\:0443 \:043d\:0435 \:043f\:0440\:0435\:0434\:0441\:0442\:0430\:0432\:043b\:044f\:044e\:0442 \:0444\:0438\:0437\:0438\:0447\:0435\:0441\:043a\:0438 \:0440\:0435\:0430\:043b\:0438\:0437\:0443\:0435\:043c\:043e\:0433\:043e \:0440\:0435\:0448\:0435\:043d\:0438\:044f. \:0418\:0442\:0430\:043a,*)


angleSpherModeVectI = angleSpherModeVectIUnk /. {
	f1[u] -> C[1] LegendreP[l, Cos[u]],
	f2[u] -> C[2] LegendreP[l, 1, Cos[u]]
};

angleSpherModeVectII = angleSpherModeVectIIUnk /. {
	f3[u] -> C[3] LegendreP[l, 1, Cos[u]]
};

angleSpherModeVectI // MatrixForm
angleSpherModeVectII // MatrixForm


LapV[angleSpherModeVectII]//FullSimplify
Table[%//FunctionExpand//PowerExpand//FullSimplify//PowerExpand//FullSimplify, {l,4}]


(* ::Subsection::Closed:: *)
(*\:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 \:0440\:0430\:0434\:0438\:0430\:043b\:044c\:043d\:044b\:0445 \:0447\:0430\:0441\:0442\:0435\:0439 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0445 \:043c\:043e\:0434*)


(* ::Text:: *)
(*\:0417\:0430\:043f\:0438\:0448\:0435\:043c \:0431\:0430\:0437\:043e\:0432\:0443\:044e \:043c\:043e\:0434\:0443 \:0441 \:0435\:0449\:0435 \:043d\:0435 \:0438\:0437\:0432\:0435\:0441\:0442\:043d\:043e\:0439 \:0440\:0430\:0434\:0438\:0430\:043b\:044c\:043d\:043e\:0439 \:0447\:0430\:0441\:0442\:044c\:044e.*)


baseSpherModeVectIUnk  = angleSpherModeVectI * { f1[r], f2[r], f3[r] };
baseSpherModeVectIIUnk = angleSpherModeVectII * { f1[r], f2[r], f3[r] };

baseSpherModeVectIUnk //MatrixForm
baseSpherModeVectIIUnk //MatrixForm


(* ::Text:: *)
(*\:0421\:043e\:0441\:0442\:0430\:0432\:0438\:043c \:0434\:0438\:0444\:0444\:0435\:0440\:0435\:043d\:0446\:0438\:0430\:043b\:044c\:043d\:044b\:0435 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:044f \:043d\:0430 \:0440\:0430\:0434\:0438\:0430\:043b\:044c\:043d\:044b\:0435 \:0447\:0430\:0441\:0442\:0438 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0445 \:043c\:043e\:0434.*)


radialSpherModeVectIDE  = LapV[baseSpherModeVectIUnk] /. m -> L;
radialSpherModeVectIIDE = LapV[baseSpherModeVectIIUnk] /. m -> L;
radialSpherModeVectIDE  = Table[radialSpherModeVectIDE[[i]] //Simplify, {i,2}];
radialSpherModeVectIIDE = radialSpherModeVectIIDE[[3]] //Simplify;
radialSpherModeVectIDE //MatrixForm
radialSpherModeVectIIDE //MatrixForm


(* ::Text:: *)
(*\:0414\:0430\:043d\:043d\:044b\:0435 \:0432\:044b\:0440\:0430\:0436\:0435\:043d\:0438\:044f \:0438 \:043f\:043e\:0441\:043b\:0435 \:0443\:043f\:0440\:043e\:0449\:0435\:043d\:0438\:044f \:0441\:0440\:0435\:0434\:0441\:0442\:0432\:0430\:043c\:0438 Wolfram \:0432\:044b\:0433\:043b\:044f\:0434\:044f\:0442 \:0433\:0440\:043e\:043c\:043e\:0437\:0434\:043a\:0438\:043c\:0438, \:043e\:0434\:043d\:0430\:043a\:043e \:043e\:043d\:0438 \:043c\:043e\:0433\:0443\:0442 \:0432\:044b\:0433\:043b\:044f\:0434\:0435\:0442\:044c \:043f\:0440\:043e\:0449\:0435. \:041c\:044b \:043f\:043e\:043a\:0430\:0436\:0435\:043c \:044d\:0442\:043e \:043f\:043e \:0438\:043d\:0434\:0443\:043a\:0446\:0438\:0438, \:043e\:0434\:043d\:0430\:043a\:043e \:0432\:043e\:0437\:043c\:043e\:0436\:043d\:043e \:0438 \:0441\:0442\:0440\:043e\:0433\:043e\:0435 \:043c\:0430\:0442\:0435\:043c\:0430\:0442\:0438\:0447\:0435\:0441\:043a\:043e\:0435 \:0434\:043e\:043a\:0430\:0437\:0430\:0442\:0435\:043b\:044c\:0441\:0442\:0432\:043e.*)


radialSpherModeVectIDETable = Table[radialSpherModeVectIDE, {l, 5}] //FullSimplify //PowerExpand //FullSimplify;
radialSpherModeVectIIDETable = Table[radialSpherModeVectIIDE, {l, 5}] //FullSimplify //PowerExpand //FullSimplify;

radialSpherModeVectIDETable //TableForm
radialSpherModeVectIIDETable //TableForm


(* ::Text:: *)
(*\:041c\:043e\:0436\:043d\:043e \:0437\:0430\:043c\:0435\:0442\:0438\:0442\:044c, \:0447\:0442\:043e, \:0432\:043e-\:043f\:0435\:0440\:0432\:044b\:0445, \:0437\:0430\:0432\:0438\:0441\:044f\:0449\:0438\:0435 \:043e\:0442 \:0443\:0433\:043b\:0430 \:0438 \:0437\:0430\:0432\:0438\:0441\:044f\:0449\:0438\:0435 \:043e\:0442 \:0440\:0430\:0434\:0438\:0443\:0441\:0430 \:043c\:043d\:043e\:0436\:0438\:0442\:0435\:043b\:0438 \:0440\:0430\:0437\:0434\:0435\:043b\:044f\:044e\:0442\:0441\:044f, \:0430 \:0432\:043e-\:0432\:0442\:043e\:0440\:044b\:0445, \:043a\:043e\:044d\:0444\:0444\:0438\:0446\:0438\:0435\:043d\:0442\:044b \:043f\:0440\:0438 f1, f2 \:0438 f3 \:043f\:043e\:0434\:0447\:0438\:043d\:044f\:044e\:0442\:0441\:044f \:043d\:0435\:043a\:043e\:0442\:043e\:0440\:044b\:043c \:043a\:0432\:0430\:0434\:0440\:0430\:0442\:0438\:0447\:043d\:044b\:043c \:0437\:0430\:043a\:043e\:043d\:0430\:043c. \:041e\:0441\:0432\:043e\:0431\:043e\:0434\:0438\:043c\:0441\:044f \:043e\:0442 \:043b\:0438\:0448\:043d\:0438\:0445 \:043c\:043d\:043e\:0436\:0438\:0442\:0435\:043b\:0435\:0439 \:0438 \:043e\:043f\:0440\:0435\:0434\:0435\:043b\:0438\:043c \:043a\:043e\:044d\:0444\:0444\:0438\:0446\:0438\:0435\:043d\:0442\:044b.*)


radialSpherModeVectIDETable = radialSpherModeVectIDETable /. { Cos[_] -> 1, Sin[_] -> 1 } //Numerator;
radialSpherModeVectIIDETable = radialSpherModeVectIIDETable /. { Cos[_] -> 1, Sin[_] -> 1 } //Numerator;

radialSpherModeVectIDETable //TableForm
radialSpherModeVectIIDETable //TableForm

coefSeries1 = { 4, 8, 14, 22, 32 };
coefSeries2 = { 4, 12, 24, 40, 60 };
coefSeries3 = { 0, 4, 10, 18, 28 };

coefFunc1 = Fit[Table[{ i, coefSeries1[[i]] }, {i,5}], { 1, l, l^2 }, l] //Chop //Rationalize //Simplify
coefFunc2 = Fit[Table[{ i, coefSeries2[[i]] }, {i,5}], { 1, l, l^2 }, l] //Chop //Rationalize //Simplify
coefFunc3 = Fit[Table[{ i, coefSeries3[[i]] }, {i,5}], { 1, l, l^2 }, l] //Chop //Rationalize //Simplify


(* ::Text:: *)
(*\:0422\:0430\:043a\:0438\:043c \:043e\:0431\:0440\:0430\:0437\:043e\:043c, \:043e\:043a\:043e\:043d\:0447\:0430\:0442\:0435\:043b\:044c\:043d\:043e*)


radialSpherModeVectIDE = radialSpherModeVectIDETable[[5]] /. {
	-Evaluate[coefFunc1 /. l -> 5] -> -coefFunc1,
	Evaluate[coefFunc2 /. l -> 5] -> coefFunc2,
	-Evaluate[coefFunc3 /. l -> 5] -> -coefFunc3
};
radialSpherModeVectIIDE = radialSpherModeVectIIDETable[[5]] /. {
	-Evaluate[coefFunc3 /. l -> 5] -> -coefFunc3
};
radialSpherModeVectIDE = Table[radialSpherModeVectIDE[[i]] //SimplifyAsEquation, {i,2}];
radialSpherModeVectIIDE = radialSpherModeVectIIDE //SimplifyAsEquation;

radialSpherModeVectIDE //MatrixForm
radialSpherModeVectIIDE //MatrixForm


DSolve[Table[radialSpherModeVectIDE[[i]] == 0, {i,2}], { f1[r], f2[r] }, r]
DSolve[radialSpherModeVectIIDE == 0, f3[r], r]


(* ::Text:: *)
(*\:0412 \:043a\:0430\:0447\:0435\:0441\:0442\:0432\:0435 \:0440\:0435\:0448\:0435\:043d\:0438\:044f \:0432\:0442\:043e\:0440\:043e\:0433\:043e \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:044f \:0432\:044b\:0431\:0435\:0440\:0435\:043c \:0442\:043e\:043b\:044c\:043a\:043e J-\:0441\:043e\:0441\:0442\:0430\:0432\:043b\:044f\:044e\:0449\:0443\:044e.*)


(* ::Text:: *)
(*\:041f\:0435\:0440\:0432\:0430\:044f \:0441\:0438\:0441\:0442\:0435\:043c\:0430 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0439 \:043d\:0435 \:043c\:043e\:0436\:0435\:0442 \:0431\:044b\:0442\:044c \:0440\:0435\:0448\:0435\:043d\:0430 \:0441\:0440\:0435\:0434\:0441\:0442\:0432\:0430\:043c\:0438 Wolfram. \:0420\:0435\:0448\:0438\:043c \:0435\:0435 \:0432\:0440\:0443\:0447\:043d\:0443\:044e.*)


(* ::Text:: *)
(*\:041c\:043e\:0436\:043d\:043e \:0437\:0430\:043c\:0435\:0442\:0438\:0442\:044c, \:0447\:0442\:043e \:0431\:0435\:0437 \:0447\:043b\:0435\:043d\:0430 \:0441 f1[r] \:043f\:0435\:0440\:0432\:043e\:0435 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435 \:0431\:044b\:043b\:043e \:0431\:044b \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435\:043c \:043d\:0430 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0411\:0435\:0441\:0441\:0435\:043b\:044f. \:0412 \:0442\:0430\:043a\:043e\:043c \:0441\:043b\:0443\:0447\:0430\:0435 \:0440\:0430\:0437\:0443\:043c\:043d\:043e \:0438\:0441\:043a\:0430\:0442\:044c \:0440\:0435\:0448\:0435\:043d\:0438\:0435 \:0432 \:043a\:043b\:0430\:0441\:0441\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0439 \:0411\:0435\:0441\:0441\:0435\:043b\:044f. \:041e\:043f\:044b\:0442\:043d\:044b\:043c \:043f\:0443\:0442\:0435\:043c \:043c\:043e\:0436\:043d\:043e \:043f\:043e\:043b\:0443\:0447\:0438\:0442\:044c \:0442\:0430\:043a\:043e\:0435 \:0440\:0435\:0448\:0435\:043d\:0438\:0435:*)


radialSpherModeVectI  = {
	1 / C[1] l   SphericalBesselJ[l - 1, Sqrt[L] r],
	1 / (C[2] r) SphericalBesselJ[l - 1, Sqrt[L] r]
};
radialSpherModeVectII = 1 / C[3] l (l + 1) / r SphericalBesselJ[l, Sqrt[L] r];

radialSpherModeVectIDE /. {
	f1 -> (radialSpherModeVectI[[1]] /. r -> # &),
	f2 -> (radialSpherModeVectI[[2]] /. r -> # &)
} //FullSimplify

radialSpherModeVectIIDE /. {
	f3 -> (radialSpherModeVectII /. r -> # &)
} //FullSimplify


(* ::Text:: *)
(*\:0422\:0435\:043c \:0436\:0435 \:043f\:0443\:0442\:0435\:043c \:043c\:043e\:0436\:043d\:043e \:043f\:043e\:043b\:0443\:0447\:0438\:0442\:044c \:0431\:043e\:043b\:0435\:0435 \:043f\:0440\:0438\:0432\:043b\:0435\:043a\:0430\:0442\:0435\:043b\:044c\:043d\:043e\:0435 \:0440\:0435\:0448\:0435\:043d\:0438\:0435 \:0432\:0438\:0434\:0430*)


radialSpherModeVectI  = {
	1 / C[1] l (l + 1) / r SphericalBesselJ[l, Sqrt[L] r],
	1 / C[2] (Sqrt[L] r SphericalBesselJ[l - 1, Sqrt[L] r] - l SphericalBesselJ[l, Sqrt[L] r]) / r^2
};
radialSpherModeVectII = 1 / C[3] l (l + 1) / r SphericalBesselJ[l, Sqrt[L] r];

radialSpherModeVectIDE /. {
	f1 -> (radialSpherModeVectI[[1]] /. r -> # &),
	f2 -> (radialSpherModeVectI[[2]] /. r -> # &)
} //FullSimplify

radialSpherModeVectIIDE /. {
	f3 -> (radialSpherModeVectII /. r -> # &)
} //FullSimplify


(* ::Subsection::Closed:: *)
(*\:0418\:0437\:043e\:0431\:0440\:0430\:0436\:0435\:043d\:0438\:0435 \:0443\:0433\:043b\:043e\:0432\:044b\:0445 \:0447\:0430\:0441\:0442\:0435\:0439 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0445 \:043c\:043e\:0434*)


Spher;


angleModeVectIIFunc[l_, m_] := Module[{ mode, result },
	mode = angleSpherModeVectII // FunctionExpand;
	Do[mode = udzSpherOp[[1]] @ mode // Simplify, { i, m }];
	mode = mode /. { C[_] -> 1, r -> 1 };
	result = FullSimplify[Im[mode], Element[u, Reals]] // ComplexExpand;
	Return[result.Inverse[HH].result /. { C[_] -> 1, r -> 1 }];
];

angleModesVectII = Table[
	Table[
		angleModeVectIIFunc[l, m],
		{m, 0, l}
	],
	{l, 1, 4}
];

angleModeVectIIPlots = Table[
	Table[
		SphericalPlot3D[
			angleModesVectII[[l, m]],
			{u, 0, Pi}, {w, 0, 2Pi},
			PlotRange -> Full,
			Boxed -> False,
			Axes -> False,
			Mesh -> None
		],
		{m, Length[angleModesVectII[[l]]]}
	],
	{l, Length[angleModesVectII]}
];

angleModeVectIIPlotsTable = TableForm[
	angleModeVectIIPlots,
	TableHeadings ->
	{
		Table[StringForm["l = ``", l - 0], {l, Length[angleModesVectII]}],
		Table[StringForm["m = ``", m - 1], {m, Length[angleModesVectII]}]
	},
	TableAlignments -> Center
];

angleModeVectIIPlotsTable

Export[FileNameJoin[{NotebookDirectory[], "angle_modes_vect_ii.png"}], angleModeVectIIPlotsTable, Background -> None];





(* ::Subsection::Closed:: *)
(*\:0418\:0437\:043e\:0431\:0440\:0430\:0436\:0435\:043d\:0438\:0435 \:0440\:0430\:0434\:0438\:0430\:043b\:044c\:043d\:044b\:0445 \:0447\:0430\:0441\:0442\:0435\:0439 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0445 \:043c\:043e\:0434*)


Spher;


radialSpherModesVectIB = Table[radialSpherModeVectI[[2]] /. { L -> 1, C[_] -> 1 }, {l, 2, 5}];
radialSpherModesVectII = Table[radialSpherModeVectII /. { L -> 1, C[_] -> 1 }, {l, 1, 4}];

radialSpherModesVectPlotIB = Plot[
	Evaluate[Table[Callout[radialSpherModesVectIB[[l-1]], StringForm["l = ``", l], Above], {l, 2, 5}]],
	{r,0,15},
	PlotRange -> Full,
	ClippingStyle -> Automatic,
	Ticks -> {{0, 5, 10, 15}, {0, 0.1, 0.2}},
	AxesLabel -> { "r", "\*SubsuperscriptBox[a,n,2](r)" }
]

radialSpherModesVectPlotII = Plot[
	Evaluate[Table[Callout[radialSpherModesVectII[[l]], StringForm["l = ``", l], Above], {l, 1, 4}]],
	{r,0,15},
	PlotRange -> Full,
	Ticks -> {{0, 5, 10, 15}, {0, 0.25, 0.5}},
	AxesLabel -> { "r", "\*SubsuperscriptBox[b,n,3](r)" }
]

Export[FileNameJoin[{NotebookDirectory[], "radial_modes_vect_ib.png"}], radialSpherModesVectPlotIB, Background -> None, ImageSize -> {800, 600}];
Export[FileNameJoin[{NotebookDirectory[], "radial_modes_vect_ii.png"}], radialSpherModesVectPlotII, Background -> None, ImageSize -> {800, 600}];
