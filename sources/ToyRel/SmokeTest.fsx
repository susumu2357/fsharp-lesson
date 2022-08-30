#r "nuget:Deedle"
open Deedle

#r "nuget:FParsec"

open FParsec

#r "nuget:RadLine"

open RadLine

open System

Environment.CurrentDirectory
Environment.CurrentDirectory <- @"C:\Users\susum\OneDrive\Documents\fsharp\fsharp-lesson\sources\ToyRel"
// Environment.CurrentDirectory <- @"C:\Users\susum\Documents\fsharp-lesson\sources\ToyRel"

#load "Common.fs"
open Common
#load "Parser.fs"
open Parser
#load "Relation.fs"
open Relation
#load "Eval.fs"
open Eval
#load "Interpreter.fs"
open Interpreter

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
eval "print test_relation"

// いろいろprojectを実行してみよう
// wikipediaデータベースのデータで、Employeeの名前の一覧を表示してみましょう。
eval "test1 = project (Employee) Name"
eval "print test1"

// tandp.mdの図書館データベースについて、この図書館に所蔵されている本の著者の一覧を表示しましょう。
eval "use library"
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

// tandp.mdの図書館データベースで、indexから作者がヘミングウェイでクラスがc3のものを取り出しましょう。
eval "use library"
eval "test_c3 = restrict (index) ((author = \"HEMINGWAY\") and (class = \"C3\"))"
eval "print test_c3"

// tandp.mdの在庫管理データベースについて、L1支社に現在まだ在庫としてある商品（INSTOCK）のsell_priceとcost_priceの一覧を取り出しましょう。
eval "use glossary"
eval "test_instock = project (restrict (stock) (date_out = \"INSTOCK\")) sell_price, cost_price"
eval "print test_instock"

// Error handling for the restrict condition
eval "use wikipedia"

// TypesMismatch
eval "restrict (Employee) (Name = 1)"
eval "restrict (Employee) (Name = EmpId)"

// IlldifinedOperatorForStrings
eval "restrict (Employee) (Name > \"Harry\")"
eval "restrict (Employee) (Name <= \"Harry\")"

// ColumnNotFound
eval "restrict (Employee) (hoge = 1)"

// When both conditions are invalid, only raise the first error (ColumnNotFound in this case)
eval "restrict (Employee) ((hoge = 1) and (Name > \"Harry\"))"

// Test the product
eval "use wikipedia"
eval "test = (Employee) product (Dept)"
eval "print test"

eval "test = (Employee) product (EmployeeTypeMismatch)"
eval "print test"

eval "test = (Employee) product (project (EmployeeTypeMismatch) EmpId)"
eval "print test"

// Test the join
// Wikipediaの例
// Natural joinの所の例にあるEmployeeとDeptのjoin結果となるようなtheta-joinを書いてみよう
eval "use wikipedia"
eval "natural_join = join (Employee) (Dept) (Employee.DeptName = Dept.DeptName)"
eval "print natural_join"

// CarとBoatの例を動かせ
eval "car_boat = join (Car) (Boat) (CarPrice >= BoatPrice)"
eval "print car_boat"

// tandpの例
// 4.3.1 商品を提供している全producerを調べよ
eval "use glossary"
eval "goods_producers = project (goods) producer"
eval "print goods_producers"

// 4.3.2 支社に配送している全producerを調べよ
eval "branch_producers = project (delivery) producer"
eval "print branch_producers"

// 4.3.3 L1支社に配送されてまだin stockな状態の全商品の、sell_priceとcost_priceを以下の２つの方法で調べよ
// P1にL1支社でin stockなrowの一覧を入れ、次にP1からsell_price, cost_priceを取り出して表示する、という２つのクエリに分けるやり方
eval "P1 = restrict (stock) ((branch = \"L1\") and (date_out = \"INSTOCK\"))"
eval "solution1 = project (P1) sell_price, cost_price"
eval "print solution1"

//上と同じものをかっこを使ってネストして一文にしたやり方
eval "solution2 = project (restrict (stock) ((branch = \"L1\") and (date_out = \"INSTOCK\"))) sell_price, cost_price"
eval "print solution2"

// 4.3.4 以下の条件を満たすproducer, product_code, descriptionを表示せよ：
// 全てのブランチで、届いた日と同じ日に売れたもの。（以下ヒントを書くので、自分で好きに書いたあとにヒントの通りにも書いてみて下さい）
// まずは自分で好きに書いてみる。
eval
    "tmp1 = join (restrict (stock) (date_in = date_out)) (delivery) ((branch = delivery.branch) and (stock = delivery.stock))"

eval
    "my_solution = project (join (tmp1) (goods) (tmp1.product_code = goods.product_code)) producer, product_code, description"

eval "print my_solution"

// r1にstockのうちdate_inとdate_outが等しいものだけを入れる
eval "r1 = restrict (stock) (date_in = date_out)"

// r2でr1とdeliveryを、ブランチとストックが同じようにjoin
eval "r2 = join (r1) (delivery) ((r1.branch = delivery.branch) and (r1.stock = delivery.stock))"

// r3にr2とgoodsをjoin
eval "r3 = join (r2) (goods) (r2.product_code = goods.product_code)"

// r4でr3をproject
eval "r4 = project (r3) producer, product_code, description"
eval "print r4"

// このやり方は最後にprojetをやっているので効率が悪い。もっと早くprojectを行うようにクエリを直すとどうなるか？
eval "r1 = project (restrict (project (stock) branch, stock, date_in, date_out) (date_in = date_out)) branch, stock"
eval "r2 = project (join (r1) (delivery) ((r1.branch = delivery.branch) and (r1.stock = delivery.stock))) product_code"
eval "r3 = join (r2) (goods) (r2.product_code = goods.product_code)"
eval "print r3"

// 4.3.5 以下の条件を満たすbranch, size, colour, sell_priceの一覧を表示せよ：
// まだ売れてないdress全て
eval
    "r1 = project (restrict (project (stock) branch, stock, size, colour, sell_price, date_out) (date_out = \"INSTOCK\")) branch, stock, size, colour, sell_price"

eval
    "r2 = project (join (r1) (delivery) ((r1.branch = delivery.branch) and (r1.stock = delivery.stock))) product_code, branch, size, colour, sell_price"

eval
    "r3 = project (join (restrict (goods) (description = \"DRESS\")) (r2) (product_code = r2.product_code)) branch, size, colour, sell_price"

eval "print r3"
