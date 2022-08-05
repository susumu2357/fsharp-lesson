#r "nuget:Deedle"
open Deedle

#r "nuget:FParsec"
open FParsec

#load "Common.fs"
open Common

#load "Parser.fs"
open Parser
#load "Relation.fs"
open Relation
#load "Eval.fs"
open Eval

eval "print シラバス"
eval "project (シラバス) 名前, 学年"
eval "project (project (シラバス) 専門, 学年, 場所) 専門, 学年"
eval "シラバス"

eval "test_relation = project (project (シラバス) 専門, 学年, 場所) 専門, 学年"
eval "list"
