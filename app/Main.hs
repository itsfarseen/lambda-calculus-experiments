module Main where

import LC

-- Note: SExpr parser
-- (foo bar) -> App foo bar
-- (\\. x body) -> Lam x body
-- foo, (foo), ((foo)) -> Var foo
-- 1234 -> LCInt 1234

main :: IO ()
main = do
  let l1 = "((\\. x (\\. y x)) 100)" -- >> `const 100`
  let l2 = unwords ["(", l1, "200", ")"] -- >> `((const 100) 200)`
  let lam = read l2 :: LC Id
  putStrLn $ "Expr: " ++ show lam

  let env =
        [ (Id "x", EvInt 1)
        , (Id "y", EvInt 2)
        ] ::
          Env Id

  putStrLn $ "Env: " ++ show env
  let eval = evaluate env lam
  putStrLn $ "Eval: " ++ show eval
