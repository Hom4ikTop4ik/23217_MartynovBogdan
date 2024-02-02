--
data LambdaTerm = Var String | Abs String LambdaTerm | App LambdaTerm LambdaTerm

-- ВЫБЕРЕТЕ НАБОР СКОБОК САМИ
variant = 0 -- 0, или 1, или 2 

-- I can't color in terminal, but I can add more brackets)))
skobki_open'  = [["("], ["(", "[", "{", "<"], ["(", "[", "{", "<", "|", "/"]]
skobki_close' = [[")"], [")", "]", "}", ">"], [")", "]", "}", ">", "|", "/"]]

-- [""] delete the most внешние brackets
skobki_open  = [""] ++ cycle (skobki_open'  !! variant)
skobki_close = [""] ++ cycle (skobki_close' !! variant)

instance Show LambdaTerm where
    show :: LambdaTerm -> String
    show = show_lambda 0 where
        show_lambda lvl term = case term of
            Var abc -> "" ++ abc ++ ""
            App ab cd -> (skobki_open !! lvl) ++ (show_lambda (lvl+1) ab) ++ " " ++ (show_lambda (lvl+1) cd) ++ (skobki_close !! lvl)
            Abs x lambda -> (skobki_open !! lvl) ++ "\\" ++ x ++ "." ++ (show_lambda (lvl+1) lambda) ++ (skobki_close !! lvl)

instance Eq LambdaTerm where
    -- (==) tree1 tree2 = case (tree1, tree2) of
    --     (Nil, Nil) -> True
    --     _ -> False
    (==) term1 term2 = case (term1, term2) of
        (Var a, Var b) -> a == b
        (Abs s1 t1, Abs s2 t2) -> (s1 == s2) && (t1 == t2)
        (App t1 t2, App t3 t4) -> (t1 == t3) && (t2 == t4) -- it won't work if term1 = App (\x.x) t, term2 = App (\y.y) t. 
                                                           -- Probably, I add alpha-"reduce" (I know it isn't reduce, alpha-change)  
        _ -> False

-- ===        
-- YOUR TESTS

id' = Abs "x" (Var "x")
xx = Abs "x" (App (Var "x") (Var "x"))
t = Abs "x" $ App (Var "f") (App (Var "x") (Var "x"))
yComb = Abs "f" (App t t)

testTerm1 = Abs "n" (App (Var "f") (Var "n"))
testTerm2 = App testTerm1 (Abs "a" (Var "a"))
testTerm3 = App (Abs "x" (App (Var "x") (Var "x"))) (Var "n")
testTerm4 = App xx xx
testTerm5 = App yComb (Var "foo")

-- ===
-- MY TESTS
-- 0. (\w.www)
www = Abs "w" (App (App (Var "w") (Var "w")) (Var "w"))
-- 1. (\w.www) (\w.www)
my1 = App www www
-- 2. (\x.z) [(\w.www) (\w.www)]
-- from WikiPedia
my2 = App (Abs "x" (Var "z")) my1
-- chorch 0
ch0 = Abs "f" (Abs "x" (Var "x"))
-- chorch increment
succ' = Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))
-- на удивление, каждому числу Чорча хвататет трёх beta-reduce у меня
-- то есть "beta_red ch25 3" выведет нормальное 25 как число Чорча 
ch1 = App succ' ch0
ch2 = App succ' ch1
ch3 = App succ' ch2
ch4 = App succ' ch3
ch5 = App succ' ch4
ch6 = App succ' ch5
ch7 = App succ' ch6
ch25 = App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' (App succ' ch0))))))))))))))))))))))))

-- ===
-- HELPING 

-- я сначала применяю внешнюю beta-reduce, поэтому бесконечность в примере my2 не появляется
reduceSubterms :: LambdaTerm -> LambdaTerm
reduceSubterms term = case term of 
    App (Abs str lambda) lambda2 -> (substitution (reduceSubterms lambda) str (reduceSubterms lambda2))
    Var str -> term
    Abs str lambda -> Abs str (reduceSubterms lambda)
    App a b -> App (reduceSubterms a) (reduceSubterms b)

-- bad reduce
reduceSubterms' :: LambdaTerm -> LambdaTerm
reduceSubterms' term = case term of 
    App (Abs str lambda) lambda2 -> if (canBeReduced lambda2) then (App (Abs str lambda) (reduceSubterms' lambda2)) else (substitution lambda str (reduceSubterms' lambda2))
    Var str -> term
    Abs str lambda -> Abs str (reduceSubterms' lambda)
    App a b -> App (reduceSubterms' a) (reduceSubterms' b)

substitution :: LambdaTerm -> String -> LambdaTerm -> LambdaTerm
substitution arg1 arg2 arg3 = case arg1 of 
     -- if \arg2.arg1 == (\arg2.arg2) then (id arg3) -> arg3 else (\arg2.arg1 -> arg1)
        Var a       -> if (a == arg2) then arg3 else arg1
     -- if arg1 == ((\arg2.\lx.term) lambda2) then \lx.(term[arg2->arg3])
        Abs lx term -> Abs lx (substitution term arg2 arg3)
     -- if arg1 == ((\arg2.(a b)) arg3) then a[arg2->arg3] b[arg2->arg3]
        App a b     -> App (substitution a arg2 arg3) (substitution b arg2 arg3)

-- ===
-- IF

canBeReduced :: LambdaTerm -> Bool
canBeReduced term = case term of
    App (Abs str lambda1) lambda2 -> True
    Var str -> False
    Abs str lambda -> canBeReduced lambda
    App lambda1 lambda2 -> (canBeReduced lambda1) || (canBeReduced lambda2)

-- ===
-- MAIN

-- eval term depth
beta_red :: LambdaTerm -> Int -> LambdaTerm
beta_red term 0 = term -- it would be better if instead "term 0" I check "term i" "if i <= 0", but you don't like brackets( 
beta_red term lvl = if (canBeReduced term) && (term /= a) -- I insert second condition because if I see (\x.xx)(\x.xx) I want exit recursion
                        then beta_red a (lvl - 1)
                        else term
                    where a = reduceSubterms term
-- my solution before I saw the 3rd task:
{-
beta_red term recursion_limit = if term == a
                                    then a 
                                    else beta_red a (recursion_limit - 1)
                                where a = reduceSubterms term 
-}

-- ===
