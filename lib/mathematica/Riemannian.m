(* ::Package:: *)

(* ::Section:: *)
(*\:0418\:043c\:043f\:043e\:0440\:0442 \:0431\:0438\:0431\:043b\:0438\:043e\:0442\:0435\:043a*)


(* ::Input::Initialization:: *)
libDir = ".";
Quiet[NotebookEvaluate[FileNameJoin[{NotebookDirectory[],libDir, "Ricci_sq.m"}]]]


(* ::Section:: *)
(*\:0412\:0441\:043f\:043e\:043c\:043e\:0433\:0430\:0442\:0435\:043b\:044c\:043d\:044b\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0438*)


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
