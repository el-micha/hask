
data FSEntry = Folder String [FSEntry] | File String String

test = Folder "home" [Folder "work" [File "students.txt" "Alice, Bob", File "hint" "You can use fFSE!"], File "fun" "FMFP"]

fFSE :: (String -> [a] -> a) -> (String -> String -> a) -> FSEntry -> a
fFSE fFolder fFile (Folder name entries) = fFolder name (map (fFSE fFolder fFile) entries)
fFSE fFolder fFile (File name text) = fFile name text

number :: FSEntry -> Int
number = fFSE (\x ys -> sum ys + 1) (\x y -> 1)

count :: Char -> FSEntry -> Int
count c = fFSE (\x ys -> sum ys) (\x y -> length $ filter (c==) y)

paths :: FSEntry -> [String]
paths = fFSE (\x ys -> map (\s -> x ++ "/" ++ s) (concat ys)) (\x y -> [x])



