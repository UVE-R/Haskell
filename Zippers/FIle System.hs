import Data.List (break)

type Name = String
type Data = String
--a file has a name and data
--a folder has a name and a list of items(files)
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem  
myDisk = 
    Folder "root"   
        [ File "goat_yelling_like_man.wmv" "baaaaaa"  
        , File "pope_time.avi" "god bless"  
        , Folder "pics"  
            [ File "ape_throwing_up.jpg" "bleargh"  
            , File "watermelon_smash.gif" "smash!!"  
            , File "skull_man(scary).bmp" "Yikes!"  
            ]  
        , File "dijon_poupon.doc" "best mustard"  
        , Folder "programs"  
            [ File "fartwizard.exe" "10gotofart"  
            , File "owl_bandit.dmg" "mov eax, h00t"  
            , File "not_a_virus.exe" "really not a virus"  
            , Folder "source code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ]  

--the current folder, the folders before and the folders that come after
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

--zipper
type FSZipper = (FSItem, [FSCrumb])

--move up
fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs) --add the previous folder to its predecessors folders

--focuses on a file of a given name within the current folder
fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items,bs) = 
    let (ls, item:rs) = break (nameIs name) items --split the list where the folder is the folder we are looking for, item will be the folder we are looking for
    in (item, FSCrumb folderName ls rs:bs) --rebuild the zipper

--check if the filer/folder name matches with the name to find
nameIs :: Name -> FSItem -> Bool  
nameIs name (Folder folderName _) = name == folderName  
nameIs name (File fileName _) = name == fileName  

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

--change the name of a file or folder
fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)  


fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = 
    (Folder folderName (item:items), bs)

main = do

    --go to pics then skull man
    let newFocus = (myDisk,[]) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"  

    print(fst newFocus) -- =File "skull_man(scary).bmp" "Yikes!"  

    let newFocus2 = newFocus -: fsUp -: fsTo "watermelon_smash.gif" 
    print(fst newFocus2) -- =File "watermelon_smash.gif" "smash!!"  

    --rename a folder
    let newFocus = (myDisk,[]) -: fsTo "pics" -: fsRename "cspi" 

    print(fst newFocus) -- =Folder "cspi" [File "ape_throwing_up.jpg" "bleargh",File "watermelon_smash.gif" "smash!!",File "skull_man(scary).bmp" "Yikes!"]

    --add a new file
    let newFocus = (myDisk,[]) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") 
    print(fst newFocus) -- =Folder "pics" [File "heh.jpg" "lol",File "ape_throwing_up.jpg" "bleargh",File "watermelon_smash.gif" "smash!!",File "skull_man(scary).bmp" "Yikes!"]
