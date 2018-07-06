open Generic_conversion_monad;;

module Python_Normal_Lang =
struct
  type directive = Python2_normalized_ast.stmt
  type statement = Python2_normalized_ast.annotated_stmt
  let annotate annot d = Python2_ast_types.annotate annot d
end

module Normalization_monad = Conversion_monad(Python_Normal_Lang);;

type 'a m = 'a Normalization_monad.t;;
