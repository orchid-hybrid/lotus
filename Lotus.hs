import Control.Monad
import Language.Haskell.Exts hiding (Type(..), Lambda, Case)
import qualified Language.Haskell.Exts as E
import System.Environment

data Term = Function String Expression deriving (Eq, Show)
data Expression = Variable_e String
                | Lambda String Expression
                | Expression :$ Expression
                | Case Expression [(Pattern, Expression)]
                | Constant Int
                | Str String
                | Boolean Bool
                | Expression Operator Expression Expression
                deriving (Eq, Show)

data Pattern = Constructor String [String]
             | Variable_p String
             | WildCard
             deriving (Eq, Show)

data Operator = Add
              | Subtract
              | Multiply
              | Divide
              deriving (Eq, Show)

data Type = A Atomic | Type :-> Type
data Atomic = BoolTy | IntTy | ListTy Atomic

transform :: Module -> Maybe [Term]
transform (Module _ _ _ _ _ _ ds) = foldM (\a d -> do
    t <- transformDeclaration d
    return (t:a)) [] ds

transformDeclaration :: Decl -> Maybe Term
transformDeclaration (FunBind [(Match _ (Ident i) vs _ (UnGuardedRhs b) _)]) = do
    b' <- transformExpression b

    (v':vs') <- foldM (\a v -> case v of
        (PVar (Ident i)) -> return (i:a)
        _ -> Nothing) [] vs

    return (Function i (foldr (\v a -> Lambda v a) (Lambda v' b') (reverse vs')))
transformDeclaration (PatBind _ (PVar (Ident i)) (UnGuardedRhs b) _) = do
    b' <- transformExpression b
    return (Function i b')
transformDeclaration _ = Nothing

transformExpression :: Exp -> Maybe Expression
transformExpression (E.Case e ps) = do
    e' <- transformExpression e

    ps' <- (flip3 foldM) ps [] $ \a (Alt _ p b _) -> do
        b' <- case b of
            (UnGuardedRhs b') -> transformExpression b'

        p <- case p of
            (PVar (Ident i)) -> return (Variable_p i)
            (PApp (UnQual (Ident i)) as) -> do
                as' <- (flip3 foldM) as [] $ \a p -> case p of
                    (PVar (Ident i)) -> return (i:a)
                    _ -> Nothing

                return (Constructor i as')
            PWildCard -> return WildCard

        return ((p, b'):a)

    return (Case e' ps')
transformExpression (Lit (Int i)) = return (Constant (fromIntegral i))
transformExpression (Lit (String s)) = return (Str s)
transformExpression (Var (UnQual (Ident i))) = return (Variable_e i)
transformExpression _ = Nothing

flip3 f a b c = f c b a