(* ::Package:: *)

(* ::Section::Closed:: *)
(*\:0418\:043c\:043f\:043e\:0440\:0442 \:0431\:0438\:0431\:043b\:0438\:043e\:0442\:0435\:043a*)


(* ::Input::Initialization:: *)
libDir = "..\\..\\lib\\mathematica";
Quiet[NotebookEvaluate[FileNameJoin[{NotebookDirectory[],libDir, "Ricci_sq.m"}]]]


(* ::Section::Closed:: *)
(*\:0412\:0441\:043f\:043e\:043c\:043e\:0433\:0430\:0442\:0435\:043b\:044c\:043d\:044b\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0438*)


TestRicci[Coord_, GG_] :=
{
	Subscript[\[Gamma], ij] -> (GG // MatrixForm),
	Subscript[R, ij] -> (Ricci[Coord, GG] // MatrixForm),
	R -> rs,
	Sqrt[\[Gamma]] -> gd
};


(* ::Text:: *)
(*\:041f\:0435\:0440\:0435\:0432\:043e\:0434 \:0432\:0435\:043a\:0442\:043e\:0440\:043e\:0432 \:0438\:0437 \:043e\:0434\:043d\:0438\:0445 \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442 \:0432 \:0434\:0440\:0443\:0433\:0438\:0435. \:041e\:0436\:0438\:0434\:0430\:0435\:0442 \:043d\:0430 \:0432\:0445\:043e\:0434 \:0432\:0435\:043a\:0442\:043e\:0440 x(x') \:0438 \:043a\:043e\:0432\:0430\:0440\:0438\:0430\:043d\:0442\:043d\:044b\:0439 \:0432\:0435\:043a\:0442\:043e\:0440. \:0412\:043e\:0437\:0432\:0440\:0430\:0449\:0430\:0435\:0442 \:043a\:043e\:0432\:0430\:0440\:0438\:0430\:043d\:0442\:043d\:044b\:0439 \:0432\:0435\:043a\:0442\:043e\:0440.*)


SwitchCoordinates[xOfY_, v_] := Module[{J},
	J = Table[D[xOfY[[j]], x[i]], {i, 1, 3}, {j, 1, 3}];
	Return[J.v // S];
];


(* ::Text:: *)
(*\:0414\:0438\:0444\:0444\:0435\:0440\:0435\:043d\:0446\:0438\:0430\:043b\:044c\:043d\:044b\:0439 \:043e\:043f\:0435\:0440\:0430\:0442\:043e\:0440*)


Dv[v_, a_] := Sum[v[[i]] D[a, x[i]], {i, 1, 3}];
Dv[v_] := (Dv[v, #] &);


(* ::Text:: *)
(*\:041b\:0438-\:0432\:0430\:0440\:0438\:0430\:0446\:0438\:044f*)


Lie[v_, a_ /; !ListQ[a]] := Dv[v, a];
Lie[v_, a_ /; VectorQ[a]] := Dv[v, a] - Dv[a, v];
Lie[v_] := (Lie[v, #] &);


(* ::Section:: *)
(*\:0412\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:044f*)


(* ::Subsection::Closed:: *)
(*\:0422\:0435\:043d\:0437\:043e\:0440 \:0420\:0438\:0447\:0447\:0438, \:0440\:0438\:043c\:0430\:043d\:043e\:0432\:043e \:043f\:0440\:043e\:0441\:0442\:0440\:0430\:043d\:0441\:0442\:0432\:043e*)


(* ::Text:: *)
(*\:0422\:0435\:043d\:0437\:043e\:0440 \:0420\:0438\:0447\:0447\:0438 \:0434\:0435\:043a\:0430\:0440\:0442\:043e\:0432\:044b\:0445 \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442 \:0432 \:0435\:0432\:043a\:043b\:0438\:0434\:043e\:0432\:043e\:043c \:043f\:0440\:043e\:0441\:0442\:0440\:0430\:043d\:0441\:0442\:0432\:0435*)


TestRicci[{x, y, z},
	IdentityMatrix[3]
]
TestRicci[{x, y, z},
	IdentityMatrix[3] + RotateRight[IdentityMatrix[3]]
]


(* ::Text:: *)
(*\:0422\:0435\:043d\:0437\:043e\:0440 \:0420\:0438\:0447\:0447\:0438 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0445 \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442 \:0432 \:0435\:0432\:043a\:043b\:0438\:0434\:043e\:0432\:043e\:043c \:043f\:0440\:043e\:0441\:0442\:0440\:0430\:043d\:0441\:0442\:0432\:0435*)


TestRicci[{r,u,w},
	DiagonalMatrix[{1, r^2, r^2 Sin[u]^2}]
]


(* ::Text:: *)
(*\:0422\:0435\:043d\:0437\:043e\:0440 \:0420\:0438\:0447\:0447\:0438 \:043c\:0435\:0442\:0440\:0438\:043a\:0438 \:0434\:0432\:0443\:043c\:0435\:0440\:043d\:043e\:0439 \:0441\:0444\:0435\:0440\:044b \:0432 \:0435\:0432\:043a\:043b\:0438\:0434\:043e\:0432\:043e\:043c \:043f\:0440\:043e\:0441\:0442\:0440\:0430\:043d\:0441\:0442\:0432\:0435*)


TestRicci[{ro,u,w},
	DiagonalMatrix[{1, r^2, r^2 Sin[u]^2}]
]


(* ::Text:: *)
(*\:0422\:0435\:043d\:0437\:043e\:0440 \:0420\:0438\:0447\:0447\:0438 \:043c\:0435\:0442\:0440\:0438\:043a\:0438 \:0442\:0440\:0435\:0445\:043c\:0435\:0440\:043d\:043e\:0439 \:0441\:0444\:0435\:0440\:044b*)


TestRicci[{u,v,v},
	DiagonalMatrix[{r^2, r^2 Sin[u]^2, r^2 Sin[u]^2 Sin[v]^2}]
]


(* ::Text:: *)
(*\:0422\:0435\:043d\:0437\:043e\:0440 \:0420\:0438\:0447\:0447\:0438 \:043c\:0435\:0442\:0440\:0438\:043a\:0438 \:0442\:0440\:0435\:0445\:043c\:0435\:0440\:043d\:043e\:0439 \:0441\:0444\:0435\:0440\:044b \:0432 \:043a\:043e\:043d\:0444\:043e\:0440\:043c\:043d\:044b\:0445 \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442\:0430\:0445*)


TestRicci[{x,y,z},
	IdentityMatrix[3] /(1 + (x^2+y^2+z^2)/(4r^2))^2
]


(* ::Subsection::Closed:: *)
(*\:041b\:0438-\:0432\:0430\:0440\:0438\:0430\:0446\:0438\:044f, \:043f\:043e\:043b\:044f (\:0432\:0435\:043a\:0442\:043e\:0440\:044b) \:041a\:0438\:043b\:043b\:0438\:043d\:0433\:0430 \:0442\:0440\:0435\:0445\:043c\:0435\:0440\:043d\:043e\:0433\:043e \:0435\:0432\:043a\:043b\:0438\:0434\:043e\:0432\:0430 \:043f\:0440\:043e\:0441\:0442\:0440\:0430\:043d\:0441\:0442\:0432\:0430*)


(* ::Text:: *)
(*\:0414\:0435\:043a\:0430\:0440\:0442\:043e\:0432\:044b \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442\:044b*)


killingDec = { nDec, lDec } =
{
	(* \:0412\:0435\:043a\:0442\:043e\:0440\:044b \:0441\:0434\:0432\:0438\:0433\:043e\:0432 *)
	IdentityMatrix[3],

	(* \:0412\:0435\:043a\:0442\:043e\:0440\:044b \:0432\:0440\:0430\:0449\:0435\:043d\:0438\:0439 *)
	{{  0,  z, -y },
	 { -z,  0,  x },
	 {  y, -x,  0 }}
};
killingDec = Flatten[killingDec, 1];


(* ::Text:: *)
(*\:0421\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0435 \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442\:044b*)


killingSpher = { nSpher, lSpher } =
{
	(* \:0412\:0435\:043a\:0442\:043e\:0440\:044b \:0441\:0434\:0432\:0438\:0433\:043e\:0432 *)
	{{ r Sin[u] Cos[w], Cos[u] Cos[w], - Csc[u] Sin[w] },
	 { r Sin[u] Sin[w], Cos[u] Sin[w], + Csc[u] Cos[w] },
	 { r Cos[u], - Sin[u], 0 }} / r,

	(* \:0412\:0435\:043a\:0442\:043e\:0440\:044b \:0432\:0440\:0430\:0449\:0435\:043d\:0438\:0439 *)
	{{ 0, Sin[w],  Cos[w] Cot[u] },
	 { 0, Cos[w], -Cot[u] Sin[w] },
	 { 0, 0, 1 }}
};
killingSpher = Flatten[killingSpher, 1];


(* ::Text:: *)
(*\:041b\:0438-\:0432\:0430\:0440\:0438\:0430\:0446\:0438\:0438 \:043c\:0435\:0442\:0440\:0438\:0447\:0435\:0441\:043a\:043e\:0433\:043e \:0442\:0435\:043d\:0437\:043e\:0440\:0430 \:043d\:0430 \:043f\:043e\:043b\:044f\:0445 \:041a\:0438\:043b\:043b\:0438\:043d\:0433\:0430 \:0434\:0435\:043a\:0430\:0440\:0442\:043e\:0432\:043e\:0439 \:0441\:0438\:0441\:0442\:0435\:043c\:044b \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442*)


Decart;
killingDec // Transpose // MatrixForm
MatrixForm /@ (Killing /@ killingDec)


(* ::Text:: *)
(*\:041b\:0438-\:0432\:0430\:0440\:0438\:0430\:0446\:0438\:0438 \:043c\:0435\:0442\:0440\:0438\:0447\:0435\:0441\:043a\:043e\:0433\:043e \:0442\:0435\:043d\:0437\:043e\:0440\:0430 \:043d\:0430 \:043f\:043e\:043b\:044f\:0445 \:041a\:0438\:043b\:043b\:0438\:043d\:0433\:0430 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:043e\:0439 \:0441\:0438\:0441\:0442\:0435\:043c\:044b \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442*)


Spher;
killingSpher // Transpose // MatrixForm
MatrixForm /@ (Killing /@ killingSpher)


(* ::Text:: *)
(*\:041e\:043f\:0435\:0440\:0430\:0442\:043e\:0440\:044b \:043f\:043e\:0432\:044b\:0448\:0435\:043d\:0438\:044f \:0438 \:043f\:043e\:043d\:0438\:0436\:0435\:043d\:0438\:044f  \:0432 \:0434\:0435\:043a\:0430\:0440\:0442\:043e\:0432\:044b\:0445 \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442\:0430\:0445*)


udzDec =
{
	lDec[[1]] - I lDec[[2]], (* up *)
	lDec[[1]] + I lDec[[2]], (* down *)
	lDec[[3]] (* z *)
};

udzDecOp = Lie /@ udzDec;

Decart;
udzDec // Transpose // MatrixForm
MatrixForm /@ (Killing /@ udzDec)


(* ::Text:: *)
(*\:041e\:043f\:0435\:0440\:0430\:0442\:043e\:0440\:044b \:043f\:043e\:0432\:044b\:0448\:0435\:043d\:0438\:044f \:0438 \:043f\:043e\:043d\:0438\:0436\:0435\:043d\:0438\:044f  \:0432 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0445 \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442\:0430\:0445*)


udzSpher =
{
	lSpher[[1]] - I lSpher[[2]], (* up *)
	lSpher[[1]] + I lSpher[[2]], (* down *)
	lSpher[[3]] (* z *)
};

udzSpherOp = Lie /@ udzSpher;

Spher;
udzSpher // Transpose // MatrixForm
MatrixForm /@ (Killing /@ udzSpher)


(* ::Text:: *)
(*\:041d\:0435\:043a\:043e\:0442\:043e\:0440\:044b\:0435 \:043a\:043e\:043c\:043c\:0443\:0442\:0430\:0446\:0438\:043e\:043d\:043d\:044b\:0435 \:0441\:043e\:043e\:0442\:043d\:043e\:0448\:0435\:043d\:0438\:044f \:0432 \:0434\:0435\:043a\:0430\:0440\:0442\:043e\:0432\:043e\:0439 \:0438 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:043e\:0439 \:0441\:0438\:0441\:0442\:0435\:043c\:0430\:0445 \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442*)


Decart;
(Commut[nDec[[Part[#, 1]]], nDec[[Part[#, 2]]]] &) /@ {{1, 2}, {1, 3}, {2, 3}}
(Commut[lDec[[Part[#, 1]]], lDec[[Part[#, 2]]]] &) /@ {{1, 2}, {1, 3}, {2, 3}}

Spher;
(Commut[nSpher[[Part[#, 1]]], nSpher[[Part[#, 2]]]] &) /@ {{1, 2}, {1, 3}, {2, 3}}
(Commut[lSpher[[Part[#, 1]]], lSpher[[Part[#, 2]]]] &) /@ {{1, 2}, {1, 3}, {2, 3}}


(* ::Subsection::Closed:: *)
(*\:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 \:043f\:043e\:043b\:0435\:0439 \:041a\:0438\:043b\:043b\:0438\:043d\:0433\:0430 \:0432 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0445 \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442\:0430\:0445*)


Spher;


(* ::Text:: *)
(*\:0412\:044b\:0440\:0430\:0436\:0435\:043d\:0438\:0435 \:0434\:0435\:043a\:0430\:0440\:0442\:043e\:0432\:044b\:0445 \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442 \:0447\:0435\:0440\:0435\:0437 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0435*)


cartOfSp =
{
	r Sin[u] Cos[w], (* x(r,u,w) *)
	r Sin[u] Sin[w], (* y(r,u,w) *)
	r Cos[u]         (* z(r,u,w) *)
};
cartToSp = AssociationThread[{x, y, z} -> cartOfSp];


(* ::Text:: *)
(*\:041f\:0435\:0440\:0435\:0432\:043e\:0434 \:0438\:0437 \:0434\:0435\:043a\:0430\:0440\:0442\:043e\:0432\:043e\:0439 \:0441\:0438\:0441\:0442\:0435\:043c\:044b \:043a\:043e\:043e\:0440\:0434\:0438\:043d\:0430\:0442 \:0432 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0443\:044e*)


killingSpherProved = (HH.SwitchCoordinates[cartOfSp, #1] &) @ Transpose[killingDec /. cartToSp];
killingSpherProved // MatrixForm


(* ::Subsection::Closed:: *)
(*\:041f\:043e\:043b\:044f\:0440\:0438\:0437\:0430\:0446\:0438\:0438 \:0432\:0435\:043a\:0442\:043e\:0440\:043d\:044b\:0445 \:043c\:043e\:0434*)


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
(*\:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 \:0443\:0433\:043b\:043e\:0432\:044b\:0445 \:0447\:0430\:0441\:0442\:0435\:0439 \:0441\:043a\:0430\:043b\:044f\:0440\:043d\:044b\:0445 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0445 \:043c\:043e\:0434*)


Spher;


(* ::Text:: *)
(*\:0423\:0433\:043b\:043e\:0432\:0430\:044f \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0430\:044f \:0431\:0430\:0437\:043e\:0432\:0430\:044f \:043c\:043e\:0434\:0430. \:0411\:0430\:0437\:043e\:0432\:0430\:044f \:043c\:043e\:0434\:0430 \:043d\:0435 \:0437\:0430\:0432\:0438\:0441\:0438\:0442 \:043e\:0442 w, \:043f\:043e\:0441\:043a\:043e\:043b\:044c\:043a\:0443 \:0443\:0434\:043e\:0432\:043b\:0435\:0442\:0432\:043e\:0440\:044f\:0435\:0442 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:044e udzSpherOp[[3]] @ angleSpherMode == 0. \:0417\:0430\:0432\:0438\:0441\:0438\:043c\:043e\:0441\:0442\:044c \:043e\:0442 r \:043f\:043e\:043a\:0430 \:043d\:0435 \:0440\:0430\:0441\:0441\:043c\:0430\:0442\:0440\:0438\:0432\:0430\:0435\:0442\:0441\:044f.*)


angleSpherModeUnk = f[u];


(* ::Text:: *)
(*\:0414\:0438\:0444\:0444\:0435\:0440\:0435\:043d\:0446\:0438\:0430\:043b\:044c\:043d\:043e\:0435 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435 \:043d\:0430 \:0431\:0430\:0437\:043e\:0432\:044b\:0435 \:0443\:0433\:043b\:043e\:0432\:044b\:0435 \:043c\:043e\:0434\:044b*)


angleSpherModeDE =
(
	udzSpherOp[[1]] @ udzSpherOp[[2]] @ angleSpherModeUnk
	- (- l (l + 1) angleSpherModeUnk)
) // FullSimplify;

angleSpherModeDE = angleSpherModeDE == 0;

angleSpherModeDE


(* ::Text:: *)
(*\:0420\:0435\:0448\:0438\:043c \:043f\:043e\:043b\:0443\:0447\:0435\:043d\:043d\:043e\:0435 \:0434\:0438\:0444\:0444\:0435\:0440\:0435\:043d\:0446\:0438\:0430\:043b\:044c\:043d\:043e\:0435 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435.*)


angleSpherModeSolutionGeneric = DSolve[angleSpherModeDE, f[u], u] // First;

angleSpherModeSolutionGeneric


(* ::Text:: *)
(*LegendreQ \:0443\:0445\:043e\:0434\:044f\:0442 \:043d\:0430 \:0431\:0435\:0441\:043a\:043e\:043d\:0435\:0447\:043d\:043e\:0441\:0442\:044c \:043f\:0440\:0438 \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0438 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430 Cos[u] = \[PlusMinus]1, \:043f\:043e\:044d\:0442\:043e\:043c\:0443 \:043d\:0435 \:0444\:0438\:0437\:0438\:0447\:043d\:044b*)


Table[
	Plot[
		LegendreQ[l, Cos[u]],
		{u, 0, \[Pi]}
	],
	{l, 5}
]

Table[
	Plot[
		LegendreP[l, Cos[u]],
		{u, 0, \[Pi]}
	],
	{l, 5}
]


angleSpherMode = angleSpherModeUnk /. angleSpherModeSolutionGeneric /. {C[2] -> 0};
angleSpherMode


(* ::Text:: *)
(*\:041f\:0440\:0438\:0441\:043e\:0435\:0434\:0438\:043d\:0435\:043d\:043d\:044b\:0435 P-\:043f\:043e\:043b\:0438\:043d\:043e\:043c\:044b \:041b\:0435\:0436\:0430\:043d\:0434\:0440\:0430 \:0441\:0432\:044f\:0437\:0430\:043d\:044b \:0441 P-\:043f\:043e\:043b\:0438\:043d\:043e\:043c\:0430\:043c\:0438 \:0441\:043e\:043e\:0442\:043d\:043e\:0448\:0435\:043d\:0438\:0435\:043c*)


LegendreP[l, m, x] == (-1)^m (1 - x^2)^(m/2) D[LegendreP[l, x], {x, m}] // FullSimplify


(* ::Text:: *)
(*\:0414\:0435\:0439\:0441\:0442\:0432\:0438\:0435 \:043e\:043f\:0435\:0440\:0430\:0442\:043e\:0440\:0430 ( LegendreUp[m] : f[l, x] => f[l, m, x] ), x = Cos[u] \:043c\:043e\:0436\:043d\:043e \:043f\:0440\:0435\:0434\:0441\:0442\:0430\:0432\:0438\:0442\:044c \:0432 \:0432\:0438\:0434\:0435*)


LegendreUp[m_] = ((-1)^m (1 - x^2)^(m/2) D[#[l, x], {x, m}] &) @ f /. {x -> Cos[u]} // FullSimplify // PowerExpand


(* ::Text:: *)
(*\:0410 \:0434\:0435\:0439\:0441\:0442\:0432\:0438\:0435 \:043e\:043f\:0435\:0440\:0430\:0442\:043e\:0440\:0430 \:043f\:043e\:0432\:044b\:0448\:0435\:043d\:0438\:044f -- \:0432 \:0432\:0438\:0434\:0435*)


Module[{f0},
	Table[
		f0 = f[l, Cos[u]];
		Do[f0 = udzSpherOp[[1]] @ f0 // FullSimplify, {i, 1, m}];
		{ StringForm["m = ``:", m], f0, " = ", f0 / LegendreUp[m] f[l, m, Cos[u]] },
		{m, 1, 8}
	]
] // TableForm


(* ::Text:: *)
(*\:041e\:0442\:0441\:044e\:0434\:0430, \:0432\:0441\:0435 \:043f\:0440\:043e\:0438\:0437\:0432\:043e\:0434\:043d\:044b\:0435 \:043c\:043e\:0434\:044b \:0432\:044b\:0440\:0430\:0436\:0430\:044e\:0442\:0441\:044f \:0447\:0435\:0440\:0435\:0437 \:043f\:0440\:0438\:0441\:043e\:0435\:0434\:0438\:043d\:0435\:043d\:043d\:044b\:0435 P-\:043f\:043e\:043b\:0438\:043d\:043e\:043c\:044b \:041b\:0435\:0436\:0430\:043d\:0434\:0440\:0430. \:0417\:0430\:043f\:0438\:0448\:0435\:043c \:043e\:043f\:0435\:0440\:0430\:0442\:043e\:0440 \:043f\:043e\:0432\:044b\:0448\:0435\:043d\:0438\:044f (m > 0) \:0438 \:043f\:043e\:043d\:0438\:0436\:0435\:043d\:0438\:044f (m < 0) \:0431\:0430\:0437\:043e\:0432\:043e\:0439 \:043c\:043e\:0434\:044b \:043d\:0430 m:*)


baseModeUp[bm_, m_] := bm / LegendreP[l, Cos[u]] (-I)^m Exp[m I w] LegendreP[l, Abs[m], Cos[u]];

Table[{StringForm["m = ``:", m], baseModeUp[angleSpherMode, m]}, {m, 1, 8}] // TableForm


(* ::Text:: *)
(*\:0418\:0437\:043e\:0431\:0440\:0430\:0436\:0435\:043d\:0438\:0435 \:0443\:0433\:043b\:043e\:0432\:044b\:0445 \:043c\:043e\:0434*)


angleModes = Table[
	Table[
		Re[baseModeUp[angleSpherMode, m] /. { C[_] -> 1, r -> 1 }],
		{m, 0, l}
	],
	{l, 0, 3}
];

angleModePlots = Table[
	Table[
		SphericalPlot3D[
			Evaluate[
				If[EvenQ[l - 1], angleModes[[l, m]], { angleModes[[l, m]], - angleModes[[l, m]]}]
			],
			{u, 0, Pi}, {w, 0, 2Pi},
			PlotRange -> Full,
			Boxed -> False,
			Axes -> False,
			Mesh -> None
		],
		{m, Length[angleModes[[l]]]}
	],
	{l, Length[angleModes]}
];

angleModePlotsTable = TableForm[
	angleModePlots,
	TableHeadings ->
	{
		Table[StringForm["l = ``", l - 1], {l, Length[angleModes]}],
		Table[StringForm["m = ``", m - 1], {m, Length[angleModes]}]
	},
	TableAlignments -> Center
];

angleModePlotsTable

Export[FileNameJoin[{NotebookDirectory[], "angle_modes.png"}], angleModePlotsTable, Background -> None];


(* ::Subsection::Closed:: *)
(*\:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 \:0443\:0433\:043b\:043e\:0432\:044b\:0445 \:0447\:0430\:0441\:0442\:0435\:0439 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0445 \:043c\:043e\:0434*)


Spher;


(* ::Text:: *)
(*\:0423\:0433\:043b\:043e\:0432\:0430\:044f \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0430\:044f \:0431\:0430\:0437\:043e\:0432\:0430\:044f \:043c\:043e\:0434\:0430. \:0411\:0430\:0437\:043e\:0432\:0430\:044f \:043c\:043e\:0434\:0430 \:043d\:0435 \:0437\:0430\:0432\:0438\:0441\:0438\:0442 \:043e\:0442 w, \:043f\:043e\:0441\:043a\:043e\:043b\:044c\:043a\:0443 \:0443\:0434\:043e\:0432\:043b\:0435\:0442\:0432\:043e\:0440\:044f\:0435\:0442 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:044e udzSpherOp[[3]] @ angleSpherMode == 0. \:0417\:0430\:0432\:0438\:0441\:0438\:043c\:043e\:0441\:0442\:044c \:043e\:0442 r \:043f\:043e\:043a\:0430 \:043d\:0435 \:0440\:0430\:0441\:0441\:043c\:0430\:0442\:0440\:0438\:0432\:0430\:0435\:0442\:0441\:044f.*)


angleSpherModeUnk = { 0, f1[u], f2[u] };


(* ::Text:: *)
(*\:0414\:0438\:0444\:0444\:0435\:0440\:0435\:043d\:0446\:0438\:0430\:043b\:044c\:043d\:043e\:0435 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435 \:043d\:0430 \:0431\:0430\:0437\:043e\:0432\:044b\:0435 \:0443\:0433\:043b\:043e\:0432\:044b\:0435 \:043c\:043e\:0434\:044b*)


angleSpherModeDE =
(
	udzSpherOp[[1]] @ udzSpherOp[[2]] @ angleSpherModeUnk
	- (- l (l + 1) angleSpherModeUnk)
) // FullSimplify;

angleSpherModeDE = Table[angleSpherModeDE[[i]] == 0, {i, 2, 3}];

angleSpherModeDE // MatrixForm


(* ::Text:: *)
(*\:0423\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:044f \:0440\:0430\:0441\:0446\:0435\:043f\:043b\:044f\:044e\:0442\:0441\:044f \:0438 \:043c\:043e\:0433\:0443\:0442 \:0440\:0435\:0448\:0430\:0442\:044c\:0441\:044f \:043e\:0442\:0434\:0435\:043b\:044c\:043d\:043e.*)


angleSpherModeSolutionGeneric = DSolve[angleSpherModeDE, {f1[u], f2[u]}, u] // First;

angleSpherModeSolutionGeneric // MatrixForm


(* ::Text:: *)
(*LegendreQ \:0443\:0445\:043e\:0434\:044f\:0442 \:043d\:0430 \:0431\:0435\:0441\:043a\:043e\:043d\:0435\:0447\:043d\:043e\:0441\:0442\:044c \:043f\:0440\:0438 \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0438 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:0430 Cos[u] = \[PlusMinus]1, \:043f\:043e\:044d\:0442\:043e\:043c\:0443 \:043d\:0435 \:0444\:0438\:0437\:0438\:0447\:043d\:044b*)


Table[
	Plot[
		LegendreQ[l, Cos[u]],
		{u, 0, \[Pi]}
	],
	{l, 5}
]

Table[
	Plot[
		LegendreP[l, Cos[u]],
		{u, 0, \[Pi]}
	],
	{l, 5}
]


angleSpherMode = angleSpherModeUnk /. angleSpherModeSolutionGeneric /. {C[2] -> 0, C[4] -> 0};
angleSpherMode // MatrixForm


(* ::Text:: *)
(*\:041f\:0440\:0438\:0441\:043e\:0435\:0434\:0438\:043d\:0435\:043d\:043d\:044b\:0435 P-\:043f\:043e\:043b\:0438\:043d\:043e\:043c\:044b \:041b\:0435\:0436\:0430\:043d\:0434\:0440\:0430 \:0441\:0432\:044f\:0437\:0430\:043d\:044b \:0441 P-\:043f\:043e\:043b\:0438\:043d\:043e\:043c\:0430\:043c\:0438 \:0441\:043e\:043e\:0442\:043d\:043e\:0448\:0435\:043d\:0438\:0435\:043c*)


LegendreP[l, m, x] == (-1)^m (1 - x^2)^(m/2) D[LegendreP[l, x], {x, m}] // FullSimplify


(* ::Text:: *)
(*\:0414\:0435\:0439\:0441\:0442\:0432\:0438\:0435 \:043e\:043f\:0435\:0440\:0430\:0442\:043e\:0440\:0430 ( LegendreUp[m] : f[l, x] => f[l, m, x] ), x = Cos[u] \:043c\:043e\:0436\:043d\:043e \:043f\:0440\:0435\:0434\:0441\:0442\:0430\:0432\:0438\:0442\:044c \:0432 \:0432\:0438\:0434\:0435*)


LegendreUp[m_] = ((-1)^m (1 - x^2)^(m/2) D[#[l, x], {x, m}] &) @ f /. {x -> Cos[u]} // FullSimplify // PowerExpand


(* ::Text:: *)
(*\:0410 \:0434\:0435\:0439\:0441\:0442\:0432\:0438\:0435 \:043e\:043f\:0435\:0440\:0430\:0442\:043e\:0440\:0430 \:043f\:043e\:0432\:044b\:0448\:0435\:043d\:0438\:044f -- \:0432 \:0432\:0438\:0434\:0435*)


Module[{f0},
	Table[
		f0 = f[l, Cos[u]];
		Do[f0 = udzSpherOp[[1]] @ f0 // FullSimplify, {i, 1, m}];
		{ StringForm["m = ``:", m], f0, " = ", f0 / LegendreUp[m] f[l, m, Cos[u]] },
		{m, 1, 8}
	]
] // TableForm


(* ::Text:: *)
(*\:041e\:0442\:0441\:044e\:0434\:0430, \:0432\:0441\:0435 \:043f\:0440\:043e\:0438\:0437\:0432\:043e\:0434\:043d\:044b\:0435 \:043c\:043e\:0434\:044b \:0432\:044b\:0440\:0430\:0436\:0430\:044e\:0442\:0441\:044f \:0447\:0435\:0440\:0435\:0437 \:043f\:0440\:0438\:0441\:043e\:0435\:0434\:0438\:043d\:0435\:043d\:043d\:044b\:0435 P-\:043f\:043e\:043b\:0438\:043d\:043e\:043c\:044b \:041b\:0435\:0436\:0430\:043d\:0434\:0440\:0430. \:0417\:0430\:043f\:0438\:0448\:0435\:043c \:043e\:043f\:0435\:0440\:0430\:0442\:043e\:0440 \:043f\:043e\:0432\:044b\:0448\:0435\:043d\:0438\:044f (m > 0) \:0438 \:043f\:043e\:043d\:0438\:0436\:0435\:043d\:0438\:044f (m < 0) \:0431\:0430\:0437\:043e\:0432\:043e\:0439 \:043c\:043e\:0434\:044b \:043d\:0430 m:*)


baseModeUp[bm_, m_] := bm / LegendreP[l, Cos[u]] (-I)^m Exp[m I w] LegendreP[l, Abs[m], Cos[u]];

Table[{StringForm["m = ``:", m], baseModeUp[angleSpherMode[[2]], m]}, {m, 1, 8}] // TableForm


(* ::Text:: *)
(*\:0418\:0437\:043e\:0431\:0440\:0430\:0436\:0435\:043d\:0438\:0435 \:0443\:0433\:043b\:043e\:0432\:044b\:0445 \:043c\:043e\:0434*)


angleModes = Table[
	Table[
		Re[baseModeUp[angleSpherMode[[2]], m] /. { C[_] -> 1, r -> 1 }],
		{m, 0, l}
	],
	{l, 0, 3}
];

angleModePlots = Table[
	Table[
		SphericalPlot3D[
			Evaluate[
				If[EvenQ[l - 1], angleModes[[l, m]], { angleModes[[l, m]], - angleModes[[l, m]]}]
			],
			{u, 0, Pi}, {w, 0, 2Pi},
			PlotRange -> Full,
			Boxed -> False,
			Axes -> False,
			Mesh -> None
		],
		{m, Length[angleModes[[l]]]}
	],
	{l, Length[angleModes]}
];

angleModePlotsTable = TableForm[
	angleModePlots,
	TableHeadings ->
	{
		Table[StringForm["l = ``", l - 1], {l, Length[angleModes]}],
		Table[StringForm["m = ``", m - 1], {m, Length[angleModes]}]
	},
	TableAlignments -> Center
];

angleModePlotsTable

Export[FileNameJoin[{NotebookDirectory[], "angle_modes.png"}], angleModePlotsTable, Background -> None];


(* ::Subsection::Closed:: *)
(*\:041f\:043e\:043b\:0443\:0447\:0435\:043d\:0438\:0435 \:0440\:0430\:0434\:0438\:0430\:043b\:044c\:043d\:044b\:0445 \:0447\:0430\:0441\:0442\:0435\:0439 \:0441\:043a\:0430\:043b\:044f\:0440\:043d\:044b\:0445 \:0441\:0444\:0435\:0440\:0438\:0447\:0435\:0441\:043a\:0438\:0445 \:043c\:043e\:0434*)


(* ::Text:: *)
(*\:0417\:0430\:043f\:0438\:0448\:0435\:043c \:0437\:0430\:0434\:0430\:0447\:0443 \:0428\:0442\:0443\:0440\:043c\:0430-\:041b\:0438\:0443\:0432\:0438\:043b\:043b\:044f \:0434\:043b\:044f \:043e\:043f\:0435\:0440\:0430\:0442\:043e\:0440\:0430 \:041b\:0430\:043f\:043b\:0430\:0441\:0430, \:043e\:0442\:043a\:0443\:0434\:0430 \:043f\:043e\:043b\:0443\:0447\:0438\:043c \:0434\:0438\:0444\:0444\:0435\:0440\:0435\:043d\:0446\:0438\:0430\:043b\:044c\:043d\:043e\:0435 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435 \:043d\:0430 \:0440\:0430\:0434\:0438\:0430\:043b\:044c\:043d\:0443\:044e \:0447\:0430\:0441\:0442\:044c \:0431\:0430\:0437\:043e\:0432\:043e\:0439 \:043c\:043e\:0434\:044b. \:041f\:043e\:0441\:043a\:043e\:043b\:044c\:043a\:0443 \:043e\:043d\:043e \:0434\:043e\:043b\:0436\:043d\:043e \:0432\:044b\:043f\:043e\:043b\:043d\:044f\:0442\:044c\:0441\:044f \:043f\:0440\:0438 \:043b\:044e\:0431\:043e\:043c u, \:043f\:043e\:043b\:043e\:0436\:0438\:043c u = 0.*)


baseModeWithRadialUnk = f[r] angleSpherMode /. C[_] -> 1;
lapSturmLiouville[l0_] := (((Evaluate[Lap[baseModeWithRadialUnk] + k baseModeWithRadialUnk]) /. l -> l0) // FunctionExpand // FullSimplify) /. u -> 0;

TableForm[
	Table[{ l, l (l + 1), lapSturmLiouville[l] }, {l, 0, 5}],
	TableHeadings -> {None, {"l", "l(l+1)", "\:0414\:0423"}},
	TableAlignments -> Center
]


(* ::Text:: *)
(*\:041e\:0442\:0441\:044e\:0434\:0430 \:0432\:0438\:0434\:043d\:043e, \:0447\:0442\:043e \:0434\:0438\:0444\:0444\:0435\:0440\:0435\:043d\:0446\:0438\:0430\:043b\:044c\:043d\:043e\:0435 \:0443\:0440\:0430\:0432\:043d\:0435\:043d\:0438\:0435 \:043d\:0430 f[r] \:043f\:0440\:0438\:043d\:0438\:043c\:0430\:0435\:0442 \:0432\:0438\:0434*)


lapSturmLiouvilleEq = lapSturmLiouville[5] /. -30 -> - l (l + 1)


(* ::Text:: *)
(*\:0415\:0433\:043e \:0440\:0435\:0448\:0435\:043d\:0438\:0435*)


radialSpherModeSolution = DSolve[lapSturmLiouvilleEq == 0, f[r], r]


(* ::Text:: *)
(*\:0432\:044b\:0440\:0430\:0436\:0430\:0435\:0442\:0441\:044f \:0447\:0435\:0440\:0435\:0437 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 \:0411\:0435\:0441\:0441\:0435\:043b\:044f. Y-\:0444\:0443\:043d\:043a\:0446\:0438\:044f \:0438\:043c\:0435\:0435\:0442 \:0440\:0430\:0441\:0445\:043e\:0434\:0438\:043c\:043e\:0441\:0442\:044c, \:043f\:043e\:0442\:043e\:043c\:0443*)


radialSpherMode = (f[r] /. First[radialSpherModeSolution]) /. {C[1] -> 1, C[2] -> 0}


(* ::Text:: *)
(*\:0418\:0437\:043e\:0431\:0440\:0430\:0437\:0438\:043c \:043d\:0435\:0441\:043a\:043e\:043b\:044c\:043a\:043e \:0440\:0430\:0434\:0438\:0430\:043b\:044c\:043d\:044b\:0445 \:043c\:043e\:0434*)


radialSpherModes = Table[radialSpherMode /. k -> 1, {l, 0, 3}];

radialSpherModesPlot = Plot[
	Evaluate[Table[Callout[radialSpherModes[[l+1]], StringForm["l = ``", l], Above], {l, 0, 3}]],
	{r,0,10},
	PlotRange -> Full,
	Ticks -> {{0, 5, 10}, {0, 0.5, 1}},
	AxesLabel -> { "r", "\*SubscriptBox[j,n](r)" }
]

Export[FileNameJoin[{NotebookDirectory[], "radial_modes.png"}], radialSpherModesPlot, Background -> None, ImageSize -> {800, 600}];
