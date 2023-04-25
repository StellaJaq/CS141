--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Large Arithmetic Collider                                    --
--------------------------------------------------------------------------------

module Game where 

import Data.List (transpose)
--------------------------------------------------------------------------------

-- | Represents different actions (including their parameters) that a cell can 
-- have on a row or column total.
data Action 
    = Add Int 
    | Sub Int 
    deriving (Eq, Ord, Show)

-- | Represents a cell including whether it is enabled and its action.
data Cell = MkCell Bool Action
    deriving (Eq, Show)

-- | A row has a target number and consists of zero or more cells.
data Row = MkRow Int [Cell]
    deriving (Eq, Show)

-- | A grid is comprised of the target numbers for all columns and the rows.
data Grid = MkGrid [Int] [Row]
    deriving (Eq, Show)

-- | Enumerates directions in which lists can be rotated.
data Direction = L | R
    deriving (Eq, Show)

data PathGrid = MkPathGrid [Grid] Grid
    deriving (Eq, Show)
--------------------------------------------------------------------------------

-- | `eval` @action total@ applies @action@ to the running @total@. 
-- For example:
--
-- >>> eval (Add 5) 3
-- 8
--
-- >>> eval (Sub 1) 3
-- 2
--
---------------------------------------
-- eval pattern matches on the Action type to execute the appropiate mathematical operation
---------------------------------------
eval :: Action -> Int -> Int 
eval (Sub x) y = y - x
eval (Add x) y = y + x

-- | `apply` @cell total@ applies the action of @cell@ to the running @total@ 
-- if @cell@ is enabled. For example:
--
-- >>> apply (MkCell True (Add 5)) 3
-- 8
--
-- >>> apply (MkCell False (Add 5)) 3
-- 3
--
---------------------------------------
-- apply pattern matches on the Cell type to determine whether to evaluate the action associated with the cell or not
---------------------------------------
apply :: Cell -> Int -> Int 
apply (MkCell True action) y = eval action y
apply (MkCell False action) y = y


-- | `result` @cells@ calculates the total produced by the actions of all 
-- enabled cells in @cells@ starting from 0. For example:
--
-- >>> result []
-- 0
--
-- >>> result [MkCell True (Add 5), MkCell False (Add 5), MkCell True (Sub 1)]
-- 4
--
---------------------------------------
-- result pattern matches on a list of cells to extract the head of the list, and apply it recursively to the result of the tail of the list
---------------------------------------
result :: [Cell] -> Int 
result [] = 0
result (x:xs) = apply x (result xs)

-- | `states` @cell@ is a function which returns a list with _exactly_ two
-- elements that represent the two different states @cell@ can be in. For
-- example:
--
-- >>> states (MkCell False (Add 5))
-- [MkCell True (Add 5), MkCell False (Add 5)]
--
---------------------------------------
-- states pattern matches on a cell to extract the associated function, and create a list of two cells, each linked with a distinct boolean value
---------------------------------------
states :: Cell -> [Cell]
states (MkCell _ func) = [MkCell True func, MkCell False func]

-- | `candidates` @cells@ is a function which, given a list of cells in a row,
-- produces all possible combinations of states for those cells. For example:
-- 
-- >>> candidates [MkCell False (Add 5), MkCell False (Sub 1)]
-- [ [MkCell False (Add 5), MkCell False (Sub 1)]
-- , [MkCell False (Add 5), MkCell True (Sub 1)]
-- , [MkCell True (Add 5), MkCell False (Sub 1)]
-- , [MkCell True (Add 5), MkCell True (Sub 1)]
-- ]
--
---------------------------------------
-- candidates pattern matches on a list of cells to extract the head (cell) and the tail (cells) 
-- where it then basically cons the different states of cell onto all possible candidates for the the tail of the list (cells)
---------------------------------------
candidates :: [Cell] -> [[Cell]]
candidates [] = [[]]
candidates (cell:cells) = 
    [candidate: candidates' | candidate <-(states cell), candidates' <- (candidates cells)]


-- | `solveRow` @row@ finds solutions for @row@. For example:
--
-- >>> solveRow (MkRow 5 [MkCell False (Add 5), MkCell False (Sub 1)])
-- [ MkRow 5 [MkCell True (Add 5), MkCell False (Sub 1)]]
--
-- >>> solveRow (MkRow 5 [MkCell False (Add 5), MkCell False (Add 5)])
-- [ MkRow 5 [MkCell True (Add 5), MkCell False (Add 5)] 
-- , MkRow 5 [MkCell False (Add 5), MkCell True (Add 5)]
-- ]
--
---------------------------------------
-- solveRow pattern matches on a given row to extract a list of cells and the associated target result. 
-- then uses that list to find all possible candidate combinations of cells for that row
-- it then filters candidates cells to return only the correct list of cells s.t. the cells enabled all add up to the target int
-- from that it generates a list of rows with the correct combinations of cells and their associated target val
---------------------------------------
solveRow :: Row -> [Row]
solveRow row = 
    [MkRow target correctCells | correctCells <- (filter (\i -> (result i)==target) (candidates cells))] 
        where (MkRow target cells) = row

-- | `solve` @grid@ finds all solutions for @grid@. For example:
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
-- >>> solve (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5,2] [ MkRow 3 [MkCell True (Add 3), MkCell False (Add 5)]
--                , MkRow 4 [MkCell True (Add 2), MkCell True (Add 2)]
--                ]
-- ]
--
---------------------------------------
-- solve: First I define candidateGrids which esencially uses a list comprehension to find all possible grids from the grid given, 
-- where each row results in the associated target val, using solverow and recursively calling candidateGrids on the tail end of the list of rows.

-- then resultColumns iterates simultaneously using zip through the list of ints and tail of the list of rows (recursive call) in a given grid, 
-- and calculates the resulting total of a column with apply, and return a list of all those totals

-- solve then filters through candidateGrids by checking against resultColumns to produce a list of grids. 
---------------------------------------
candidateGrids :: Grid -> [Grid]
candidateGrids (MkGrid ints [row]) = [MkGrid ints [someRow] | someRow <- (solveRow row)]
candidateGrids (MkGrid ints (row:rows)) = 
    [MkGrid ints (someRow:restOfRows)| someRow <- (solveRow row), (MkGrid _ restOfRows) <- candidateGrids (MkGrid ints rows)]

resultColumns :: Grid -> [Int]
resultColumns (MkGrid ints [(MkRow _ cells)]) = [(apply cell 0) | cell <- cells]
resultColumns (MkGrid ints ((MkRow _ cells):rows)) = 
    [(apply x y) | (x,y) <- (zip cells (resultColumns (MkGrid ints rows))) ]

solve :: Grid -> [Grid]
solve (MkGrid [] []) = [MkGrid [] []]

solve grid = filter (\(MkGrid ints rows) -> ints == resultColumns (MkGrid ints rows)) (candidateGrids grid)


-- | `rotate` @direction list@ rotates the items in @list@ to the left or
-- right depending on the value of @direction@. For example:
--
-- >>> rotate L [1,2,3]
-- [2,3,1]
--
-- >>> rotate R [1,2,3]
-- [3,1,2]
--
-- >>> rotate L []
-- []
--
-- >>> rotate R [1]
-- [1]
--
---------------------------------------
-- rotate pattern matches on a given combination of the Direction type and a given list to send the head of a list to the back (if rotating left)
-- or if rotating right, will append the tail of a list to the front using the functions last and init
---------------------------------------
rotate :: Direction -> [a] -> [a]
rotate _ [] = []
rotate _ [head] = [head]
rotate L (head:xs) = xs ++ [head]
rotate R xs = [last xs] ++ init xs 


-- | `rotations` @grid@ returns a list of grids containing all possible ways 
-- to rotate @grid@. This means the resulting list should normally have 
-- rows + columns many elements. For example:
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
-- >>> rotations (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5,2] [ MkRow 3 [MkCell False (Add 5), MkCell False (Add 3)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 2), MkCell False (Add 5)]
--                , MkRow 4 [MkCell False (Add 3), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 3), MkCell False (Add 2)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 5)]
--                ]
-- ]
--
---------------------------------------
-- rotations below utilises various definitions reliant upon one another (that help extract necessary information), to esencially 
-- append the list of various rows possible by rotating the rows to that of rotating the columns, 
-- and using a list comprehension to take each list of rows and convert it to a grid as required.
---------------------------------------

rotateCells :: [Cell] -> [Cell]
rotateCells cells = rotate L cells

rotationsRowCells :: [[Cell]] -> [[[Cell]]]
rotationsRowCells [[]] = [[[]]]
rotationsRowCells cellss = [ [if (cellss!!i) == row then rotateCells row else row |  row <- cellss ] | i <- [0 .. (length cellss)-1] ]

rotationsColCells :: [[Cell]] -> [[[Cell]]]
rotationsColCells cellss = [ transpose cellss' | cellss' <- rotationsRowCells (transpose cellss) ]

rowToCells :: Row -> [Cell]
rowToCells (MkRow target cells) = cells

cellsToRow :: Int -> [Cell] -> Row
cellsToRow target cells = (MkRow target cells)

rowsToCellss :: [Row] -> [[Cell]]
rowsToCellss rows = [ (rowToCells row) | row <- rows ]

cellssToRows :: [Int] -> [[Cell]] -> [Row]
cellssToRows ints cellss = [(MkRow target cells) | (target, cells) <- (zip ints cellss)]

rowTarget :: Row -> Int
rowTarget (MkRow num _) = num

rowTargets :: [Row] -> [Int]
rowTargets rows = [(rowTarget row) | row <- rows]


rotations :: Grid -> [Grid]
rotations (MkGrid ints rows) = 
    let cellss = rowsToCellss rows
        targets = rowTargets rows
    in [ (MkGrid ints (cellssToRows targets cellss'))

        | cellss' <- ((rotationsRowCells cellss) ++ (rotationsColCells cellss))]



-- | `steps` @grid@ finds the sequence of rotations that lead to a solution 
-- for @grid@ in the fewest number of rotations. The resulting list includes 
-- the solution as the last element. You may assume that this function will
-- never be called on a @grid@ for which there are solutions returned by
-- `solve`. The states of intermediate grids in the resulting list
-- are irrelevant - only the ones of the final grid need to be set correctly.
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 2), MkCell False (Add 3)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 5), MkCell False (Add 2)]
-- >>> steps (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5, 2] [ MkRow 3 [ MkCell False (Add 5), MkCell False (Add 3)] 
--                 , MkRow 4 [ MkCell False (Add 2), MkCell False (Add 2)]
--                 ]  
-- , MkGrid [5, 2] [ MkRow 3 [ MkCell True (Add 3), MkCell False (Add 5)] 
--                 , MkRow 4 [ MkCell True (Add 2), MkCell True (Add 2)]
--                 ] 
-- ]
--
---------------------------------------
-- steps uses model of a tree with a given grid being the root, and the daughtter nodes being the possible rotations from that grid
-- at a certain depth, where the root has depth 0, the width of the tree = numofRotations ** depth
-- concatenates all nodes on a level and finds appropiate index of a given node where, if we see a level as a list of grids ([Grid])
-- uses findWayUp to determine path from the min depth (where min depth of solLevel contains a solution)
-- and edits this path to return necessary output for steps (i.e [Grid])
---------------------------------------

--addToPath :: Grid -> PathGrid -> PathGrid
--addToPath parentGrid (MkPathGrid path _) = [parentGrid]++path

containsSol :: [Grid] -> (Bool, Grid) 
containsSol [grid] = (True, grid)
containsSol (grid:grids) 
    | null (solve grid) = containsSol grids
    | otherwise = (True, head (solve grid))


numOfRotations :: Grid -> Int
numOfRotations (MkGrid ints rows) = (length ints) + (length rows)

recGridsDepthI :: Int -> [Grid] -> [Grid]   --recursive call for gridsDepthI
recGridsDepthI 0 levelBefore =  levelBefore
recGridsDepthI maxDepth levelBefore =  recGridsDepthI (maxDepth-1) (concat [ (rotations someGrid)| someGrid <- levelBefore] )

gridsDepthI :: Int -> Grid -> [Grid] 
gridsDepthI 0 grid = [grid]
gridsDepthI maxDepth grid = 
    let firstLevel = rotations grid 
    in if maxDepth == 1
       then rotations grid
       else (recGridsDepthI (maxDepth - 1) (rotations grid))


depthOfSol :: Grid -> Int -- min depth of a tree we have to go down to get to a solution (i.e a grid that can be solved)
depthOfSol grid = snd (head (filter (\(bool,x) -> bool) [ (fst(containsSol (gridsDepthI i grid)) , i) | i<- [0 ..]]))

gridsAllDepths :: Grid -> [[Grid]] -- returns all levels of a tree, where one level is :: [Grid]
gridsAllDepths grid = 
    let solDepth = depthOfSol grid
    in [ (gridsDepthI i grid) | i <- [0 .. solDepth]]

indexOf :: Grid -> [Grid] -> Int -- given some grid in a level of nodes in a tree will return associated index
indexOf someGrid (grid:grids) 
    | someGrid == grid = 0
    | otherwise = 1 + (indexOf someGrid grids)

indexOfNodeAbove :: Int -> Grid -> Int
indexOfNodeAbove currentIndex grid = 
    let width = numOfRotations grid
    in currentIndex `quot` width


recFindWayUp :: Grid -> [[Grid]] -> Int -> Int -> [Grid] --recursive call for findWayUp
recFindWayUp _ _ _ 0 = []
recFindWayUp grid allLevels currentIndex nextLevelDepth = 
    [(allLevels!!nextLevelDepth)!!nodeAboveIndex] ++ (recFindWayUp grid allLevels nodeAboveIndex (nextLevelDepth-1))
        where nodeAboveIndex = indexOfNodeAbove currentIndex grid

-- creates path (list of grids) by starting at solution on its level, and then following unique path upwards towards the original grid /root that we're given
findWayUp :: Grid -> [Grid] 
findWayUp grid = 
    let solDepth = depthOfSol grid
        solLevel = gridsDepthI solDepth grid
        (_,someSol) = containsSol solLevel
        solIndex = indexOf someSol solLevel
        allLevels = tail (gridsAllDepths someSol) --everything but the starting original grid

    in [someSol] ++ recFindWayUp grid allLevels solIndex (solDepth-1)
            

steps :: Grid -> [Grid]
steps grid = 
    let uneditedPath = reverse (findWayUp grid)
        middlePath = init uneditedPath
        finalSol = last uneditedPath

    in middlePath ++ [head (solve finalSol)]














