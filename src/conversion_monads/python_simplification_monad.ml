open Generic_conversion_monad;;

module Python_Simple_Lang =
struct
  type directive = Python2_simplified_ast.stmt
  type statement = Python2_simplified_ast.annotated_stmt
  let annotate annot d = Python2_ast_types.annotate annot d
end

module Simplification_monad = Conversion_monad(Python_Simple_Lang);;

type 'a m = 'a Simplification_monad.t;;
