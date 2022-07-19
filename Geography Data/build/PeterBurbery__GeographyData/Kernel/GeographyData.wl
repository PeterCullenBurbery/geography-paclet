(* ::Package:: *)

BeginPackage["PeterBurbery`GeographyData`"];

(* Declare your packages public symbols here. *)

SayHello;

Begin["`Private`"];

(* Define your public and private symbols here. *)

SayHello[name_?StringQ] := Print["Hello ", name, "!"];
WikidataGeoPosition[place_?StringQ]:=First[WikidataData[First[WikidataSearch[place]],ExternalIdentifier["WikidataID","P625",<|"Label"->"coordinate location","Description"->"geocoordinates of the subject. For Earth, please note that only WGS84 coordinating system is supported at the moment"|>]]]
End[]; (* End `Private` *)

EndPackage[];
