(* ::Package:: *)

(* ::Section::Closed:: *)
(*\:0418\:043c\:043f\:043e\:0440\:0442 \:0431\:0438\:0431\:043b\:0438\:043e\:0442\:0435\:043a*)


(* ::Input::Initialization:: *)
If[$FrontEnd =!= Null, AppendTo[$Path, FileNameJoin[{NotebookDirectory[], "..", "..", "lib", "mathematica"}]]];

(Once@Get[#] &) /@ { "Riemannian.m", "Killing.m" };


(* ::Section::Closed:: *)
(*\:0412\:0441\:043f\:043e\:043c\:043e\:0433\:0430\:0442\:0435\:043b\:044c\:043d\:044b\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0438*)


TestRicci[Coord_, GG_] :=
{
	Subscript[\[Gamma], ij] -> (GG // MatrixForm),
	Subscript[R, ij] -> (Ricci[Coord, GG] // MatrixForm),
	R -> rs,
	Sqrt[\[Gamma]] -> gd
};


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
