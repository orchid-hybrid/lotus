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
transform (Module _ _ _ _ _ _ ds) = (flip3 foldM) ds [] $ \a d -> do
    t <- transformDeclaration d
    return (t:a)

transformDeclaration :: Decl -> Maybe Term
transformDeclaration (FunBind [(Match _ (Ident i) vs _ (UnGuardedRhs b) _)]) = do
    b' <- transformExpression b

    (v':vs') <- (flip3 foldM) vs [] $ \a v -> case v of
        (PVar (Ident i)) -> return (i:a)
        _ -> Nothing

    return (Function i (foldr (\v a -> Lambda v a) (Lambda v' b') (reverse vs')))
transformDeclaration (PatBind _ (PVar (Ident i)) (UnGuardedRhs b) _) = do
    b' <- transformExpression b
    return (Function i b')
transformDeclaration _ = Nothing

transformExpression :: Exp -> Maybe Expression
transformExpression (App p q) = do
    p' <- transformExpression p
    q' <- transformExpression q

    return (p' :$ q')
transformExpression (E.Case e ps) = do
    e' <- transformExpression e

    ps' <- (flip3 foldM) ps [] $ \a (Alt _ p b _) -> do
        b' <- case b of
            (UnGuardedRhs b') -> transformExpression b'
        p' <- transformPattern p

        return ((p', b'):a)

    return (Case e' ps')
transformExpression (Con (UnQual (Ident i))) = return (Variable_e i)
transformExpression (InfixApp p (QVarOp (UnQual (Symbol o))) q) = do
    p' <- transformExpression p
    q' <- transformExpression q
    o' <- case o of
        "+" -> return Add
        "-" -> return Subtract
        "*" -> return Multiply
        "/" -> return Divide
        _ -> Nothing

    return (Expression o' p' q')
transformExpression (E.Lambda _ vs b) = do
    b' <- transformExpression b
    (v':vs') <- (flip3 foldM) vs [] $ \a v -> case v of
        (PVar (Ident i)) -> return (i:a)
        _ -> Nothing

    return (foldr (\v a -> Lambda v a) (Lambda v' b') (reverse vs'))
transformExpression (Lit (Int i)) = return (Constant (fromIntegral i))
transformExpression (Lit (String s)) = return (Str s)
transformExpression (Paren e) = do
    e' <- transformExpression e
    return e'
transformExpression (Var (UnQual (Ident i))) = return (Variable_e i)
transformExpression _ = Nothing

transformPattern :: Pat -> Maybe Pattern
transformPattern (PVar (Ident i)) = return (Variable_p i)
transformPattern (PApp (UnQual (Ident i)) as) = do
    as' <- (flip3 foldM) as [] $ \a p -> case p of
        (PVar (Ident i)) -> return (i:a)
        _ -> Nothing

    return (Constructor i as')
transformPattern (PParen p) = transformPattern p
transformPattern PWildCard = return WildCard
transformPattern _ = Nothing

flip3 f a b c = f c b a