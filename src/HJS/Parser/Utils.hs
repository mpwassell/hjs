module HJS.Parser.Utils where



processComments str = processComments' str 0


processComments' str flg = case str of
                                 ('/':'/':rs) -> let flg' = if flg == 0 then 1 else flg in (' ':' ':processComments' rs flg')
                                 ('/':'*':rs) -> (' ':' ':processComments' rs 2)
                                 ('*':'/':rs) -> (' ':' ':processComments' rs 0)
                                 ('\n':rs) -> let flg' = if flg == 1 then 0 else flg 
                                              in ('\n':processComments' rs flg')
                                 (c:cs) -> if flg /= 0 then (' ':processComments' cs flg) else (c:processComments' cs flg)
                                 [] -> []
                                               
                             
doit = do
           s <- readFile "c:/Mark/MyDevelopments/haskell/HJS/hjs-0.2/testsuite/parsingonly/5_comments.js"
           putStrLn $ processComments s
