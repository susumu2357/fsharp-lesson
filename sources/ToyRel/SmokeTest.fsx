#r "nuget:Deedle"
open Deedle

#r "nuget:FParsec"

open FParsec

open System

Environment.CurrentDirectory
Environment.CurrentDirectory <- @"C:\Users\susum\OneDrive\Documents\fsharp\fsharp-lesson\sources\ToyRel"

#load "Common.fs"
open Common
#load "Parser.fs"
open Parser
#load "Relation.fs"
open Relation
#load "Eval.fs"
open Eval

// show the list of relations in database/main
eval "list"

// set the DB
eval "use wikipedia"

// print the content of the relation
eval "print Employee"

// pick up the columns and save the result with a random name
eval "project (Employee) Name, DeptName"

// evaluate the right-hand side and save the result as the name of left-hand side
eval "test_relation = project (project (Employee) Name, DeptName) Name"

// いろいろprojectを実行してみよう
// wikipediaデータベースのデータで、Employeeの名前の一覧を表示してみましょう。
eval "test1 = project (Employee) Name"
eval "print test1"

// tandp.mdの図書館データベースについて、この図書館に所蔵されている本の著者の一覧を表示しましょう。
eval "test2 = project (book) author"
eval "print test2"

// tandp.mdの在庫管理データベースについて、商品を作っている生産者の一覧を表示しましょう。
eval "use glossary"
eval "test3 = project (goods) producer"
eval "print test3"

// tandp.mdの在庫管理データベースについて、どこかの支社に一度でも配送したことなる生産者の一覧を表示しましょう。
eval "test4 = project (delivery) producer"
eval "print test4"

// tandp.mdの図書館データベースで、図書館にまったく本が存在しないsubjectの一覧を取り出す
eval "use library"
eval "not_in_library_class = (project (subject) class) difference (project (index) class)"
eval "print not_in_library_class"

// wikipediaデータベースでEmployeeの居ない部署を取り出す
eval "use wikipedia"
eval "no_employees_dept = (project (Dept) DeptName) difference (project (Employee) DeptName)"
eval "print no_employees_dept"

// Error handling
eval "use wikipedia"

// ParseError
eval "print"
eval "project Employee Name"
eval "(project (Dept) DeptName) difference project (Employee) DeptName"

// IncorrectPathError
eval "print Emp"
eval "project (Empl) Name"
eval "(project (Dept) DeptName) difference (project (Empl) DeptName)"

// ProjectionError ColumnNotFound
eval "project (Employee) hoge"

// ComparabilityError ColumnsMismatch
eval "(project (Employee) DeptName) difference (project (Dept) Manager)"

// ComparabilityError ColumnsOrderMismatch
eval "(project (Employee) Name, DeptName) difference (project (Employee) DeptName, Name)"

// ComparabilityError ColumnsTypesMismatch
eval "(project (Employee) EmpId) difference (project (EmployeeTypeMismatch) EmpId)"
