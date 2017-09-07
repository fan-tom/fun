{-# LANGUAGE RecordWildCards #-}
module Lambda where

data Expr = Abstraction {arg :: Expr, body :: Expr}
          | Application {lhs :: Expr, rhs :: Expr}
          | Var {name :: String}
          deriving Eq

instance Show Expr where
  show Var{..} = name
  show Application{..} = "("++show lhs++" "++show rhs++")"
  show Abstraction{..} = "\\"++show arg++"."++show body

newName :: Expr -> Expr
newName Var{..} = Var{name='`':name}

vars :: Expr -> [Expr] -> [Expr] -> ([Expr], [Expr])
vars v@Var{} bound free |v `elem` bound = (bound, free)
                        |otherwise = (bound, v:free)
vars Application{..} bound free = let (lb, lf) = vars lhs bound free in
  let (rb, rf) = vars rhs lb lf in (rb, rf)
vars Abstraction{..} bound free = vars body (arg:bound) free

free_vars :: Expr -> [Expr]
free_vars e = let (_, free) = vars e [] [] in free
bound_vars e = let (bound, _) = vars e [] [] in bound

rename :: Expr -> Expr -> Expr -> Expr
rename v@(Var name1) (Var name2) r@Var{} = if name1==name2 then r else v
rename Abstraction{..} a@Var{} b@Var{} = Abstraction{arg=rename arg a b, body=rename body a b}
rename Application{..} a@Var{} b@Var{} = Application{lhs=rename lhs a b, rhs=rename rhs a b}
rename a b c = error ("rename error:"++show a++"|" ++ show b++ "|" ++show c)

subst :: Expr -> Expr -> Expr -> Expr
subst (Var name1) (Var name2) s | name1==name2 = s
subst v@Var{} Var{} _ = v
subst Application{..} v@Var{} s = Application{lhs=subst lhs v s, rhs=subst rhs v s}
subst Abstraction{..} v@Var{} s = Abstraction{arg=a, body = subst b v s}
  where
  (bound, free) = vars s [] []
  (a, b) = if arg `elem` bound++free
           then let newParameter = newName arg in
            let newBody = rename body arg newParameter in
              (newParameter, newBody)
           else (arg, body)
subst a b c = error ("subst error:"++show a++"|" ++ show b++ "|" ++show c)

make_app :: Expr -> Expr -> Expr
make_app Abstraction{..} p = subst body arg p

reduce1 :: Expr -> Expr
reduce1 Application{..} = let lhsReduced = reduce1 lhs
                          in let rhsReduced = reduce1 rhs
                          in make_app lhsReduced rhsReduced
reduce1 a = a

reduce :: Expr -> Expr
reduce Application{..} = let lhsReduced = reduce lhs
                         in let rhsReduced = reduce rhs
                         in let res = make_app lhsReduced rhsReduced
                         in reduce res

reduce a = a

-- examples
-- \x.\y.x - true
tru = Abstraction{arg=Var "x", body=Abstraction{arg=Var "y", body=Var "x"}}

-- \x.\y.y - false
fls = Abstraction{arg=Var "x", body=Abstraction{arg=Var "y", body=Var "y"}}

-- \l.\r.(l l r) - or
orexpr = Abstraction{arg = Var "l",
                     body=Abstraction{arg=Var "r",
                                      body= Application{lhs=Application {lhs=Var "l", rhs=tru},
                                                        rhs = fls}}}
-- or tru fls
true_or_false = Application{lhs=Application{lhs=orexpr, rhs=tru}, rhs=fls}

-- \l.\r.(l r l) - and
andexpr = Abstraction{arg = Var "l",
                     body=Abstraction{arg=Var "r",
                                      body= Application{lhs=Application {lhs=Var "l", rhs=Var "r" },
                                                        rhs = Var "l"}}}
-- and tru fls
true_and_false = Application {lhs=Application{lhs=andexpr, rhs=tru}, rhs=fls}

-- \l.(l fls tru) - not
notexpr = Abstraction{arg = Var "l",
                      body= Application{lhs=Application {lhs=Var "l", rhs=fls},
                                        rhs = tru}}
