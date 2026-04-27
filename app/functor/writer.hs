isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (a, log) f = (b, log ++ newLog)
    where (b, newLog) = f a

-- (3, "smallish gang.") `applyLog` isBigGang
-- (False,"smallish gang.Compared gang size to 9")
