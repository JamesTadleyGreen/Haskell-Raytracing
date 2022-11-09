class NumVector v where
        (>+) :: Num a => v a -> v a -> v a
        (>-) :: Num a => v a -> v a -> v a
        prod :: Num a => v a -> v a -> v a
        (>*) :: Num a => a -> v a -> v a
        (>.) :: Num a => v a -> v a -> a
        neg  :: Num a => v a -> v a
        mag  :: v Float -> Float

data Vector a = Vec { vecList :: [a] }
              deriving (Show, Eq, Ord)

instance NumVector Vector where
        (>+) (Vec u) (Vec v) = Vec $ zipWith (+) u v
        (>-) (Vec u) (Vec v) = Vec $ zipWith (-) u v
        (>*) k (Vec v) = Vec $ map (k*) v
        prod (Vec u) (Vec v) = Vec $ zipWith (*) u v
        (>.) u v = sum $ vecList (u `prod` v)
        neg = (>*) (-1)
        mag v = sqrt $ sum $ map (**2) $ vecList v


cross :: Num a => Vector a -> Vector a -> Vector a
cross (Vec [a,b,c]) (Vec [x,y,z]) = Vec [b*z + c*y, -(a*z + c*x), a*y + b*x]

clamp :: (Num a, Ord a) => Vector a -> Vector a
clamp (Vec u) = Vec $ map (min 1) $ map (max 0) u

type Point = Vector
type Direction = Vector
type Time a = a
type Ray a = (Point a, Direction a) -- base and direction

position_at_time :: Num a => Ray a -> Time a -> Point a
position_at_time (base, dir) t = base >+ (t >* dir)

-- Colours
red     = Vec [1, 0, 0]
green   = Vec [0, 1, 0]
blue    = Vec [0, 0, 1]
white   = Vec [1, 1, 1]
black   = Vec [0, 0, 0]
midGrey = Vec [0.5, 0.5, 0.5]
nearlyWhite = Vec [0.8,0.8,0.8]

-- Materials
type Reflectivity = Float
type Diffuseness = Float
type Material a = (Vector a, Reflectivity, Diffuseness)

xor :: Bool -> Bool -> Bool
xor = (/=)

flatred, shinyred, semishinygreen, shinywhite :: Vector Float -> Material Float
flatred _ = (red, 0.0, 1.0)
shinyred _ = (red, 0.3, 0.9)
semishinygreen _ = (green, 0.5, 0.7)
shinywhite _ = (white, 0.3, 0.9)


checked_matt :: Point Float -> Material Float
checked_matt (Vec [x,y,z]) = 
            let xeven = even (truncate (x / 20.0))
                yeven = even (truncate (y / 20.0))
                zeven = even (truncate (z / 20.0)) 
                in if (xeven `xor` yeven `xor` zeven) then (white, 0, 1) else (black, 0, 1)


-- Shapes
type Radius = Float

data Shape a = Sphere (Point a) Radius ((Point a) -> (Material a))
           | Plane (Normal a) a ((Point a) -> (Material a)) 

-- Intersections
type Normal = Vector

type Intersection a = (Normal a, Point a, Ray a, Material a)


normalize :: Vector Float -> Vector Float
normalize v = mag v >* v

intersections :: Ray Float -> Shape Float -> (Time Float, Intersection Float)
intersections ray@(base, dir) (Plane normal d materialFunction) =
        let nnormal = normalize normal
            vd = nnormal >. dir
            v0 = neg (nnormal >. base) - d
        in if (vd <= 0) then []
                else let t = v0 / vd
                         point = position_at_time ray t
                     in if t > 0 then (t, (normal, point, ray, materialFunction point)) else []

closest :: [(Time a, Intersection a)] -> Intersection a
closest xs = foldl (\(t0, i0) (t1, i1) -> if (t0<t1) then i0 else i1) 0 xs


main = print $ closest [(1,(Vec [1,2,3], Vec [1,2,3], (Vec [1,2,3], Vec [1,2,3]), flatred (Vec [1])))]



