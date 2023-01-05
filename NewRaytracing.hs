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


roots :: Float -> Float -> Float -> [Float]
roots a b c = let discriminant = b*b - 4*a*c
          in if (discriminant < 0.0) then []
             else [ 0.5 * (-b + sqrt discriminant), 0.5 * (-b - sqrt discriminant) ]


normalize :: Vector Float -> Vector Float
normalize v@(Vec v')
    | mag v == 0 = Vec $ map (0*) v'
    | otherwise = (1 / mag v) >* v

intersect :: Ray Float -> Shape Float -> Maybe (Time Float, Intersection Float)
intersect ray@(base, dir) (Plane normal d materialFunction) =
        let nnormal = normalize normal
            vd = nnormal >. dir
            v0 = negate $ (nnormal >. base) + d
        in if (vd <= 0) then Nothing else
                let t = v0 / vd
                    point = position_at_time ray t
                in if t > 0 then Just (t, (normal, point, ray, materialFunction point)) else Nothing

intersect ray@(base, dir) (Sphere centre radius materialFunction) =
    let ndir = normalize dir
        b = 2 * (ndir >. (base >- centre))
        c = (mag (base >- centre))**2
        root = filter (>0) (roots 1 b c)
        normal_at_time t = normalize ((position_at_time ray t) >- centre)
    in case root of
        [] -> Nothing
        otherwise -> Just $ (t, (normal_at_time t, position_at_time ray t, ray, materialFunction (position_at_time ray t)))
            where t = minimum root


-- Lights:  We have a non-shadowable Directional light and a shadowable spotlight
data Light a = Directional (Vector a) (Vector a)
          | Spotlight (Point a) (Vector a)

-- Global bindings

-- If a ray doesn't hit an object, what color should we use?
backgroundColor :: Vector Float
backgroundColor = black

-- What lights are in our scene?

lights :: [Light Float]
lights = [Spotlight (Vec [100,-30,0]) nearlyWhite,
    Spotlight (Vec [-100,-100,150]) nearlyWhite]

-- What is the ambient lighting in the scene
ambient_light :: Vector Float
ambient_light =  Vec [0.1,0.1,0.1]

-- What Shapes are in our scene?
shapes :: [Shape Float]
shapes = [Plane (normalize (Vec [1,0,0])) 0 shinyred]

-- Is the light at 'lightpos' visible from point?
-- TODO We can halt this function early if we hit a False
point_is_lit :: Vector Float -> Vector Float -> Bool
point_is_lit point lightpos =
    let
        path = lightpos >- point
        time_at_light = mag path
        ray = (point, normalize path)
        hits = map (intersect ray) shapes
        minTime _ False = False
        minTime Nothing _ = True
        minTime (Just (t,_)) _ = t > time_at_light
    in foldr minTime True hits

-- Helper to calculate the diffuse light at the surface normal, given
-- the light direction (from light source to surface)
diffuse_coeff :: Vector Float -> Vector Float -> Float
diffuse_coeff light_dir normal = max 0 (negate ((normalize light_dir) >. (normalize normal)))

local_light :: Intersection Float -> Light Float -> Vector Float
-- Simple case of a non-shadowable directional light
local_light (normal,_,_,(materialcol,_,kd)) (Directional dir lightcol) =
  let mixed_color = materialcol `prod` lightcol
      diffuse = ((diffuse_coeff dir normal) * kd) >* mixed_color
  in diffuse

-- Spotlight - shadowable
local_light (normal, hitpoint,_,(materialcol,_,kd)) (Spotlight lightpos lightcol) =
  let mixed_color = materialcol `prod` lightcol
      diffuse = (kd * (diffuse_coeff (hitpoint >- lightpos) normal)) >* mixed_color
  in if (point_is_lit hitpoint lightpos) then diffuse else black




--main = print $ intersect (Vec [-1,1,0], Vec [1,1,0]) (Plane (normalize (Vec [1,0,0])) 0 shinyred)
main = print $ local_light (Vec [1,1,1], Vec[2,2,3], (Vec[3,2,3], Vec[4,2,3]), (Vec [5,2,3], 1, 1)) (Spotlight (Vec [6,2,3]) (Vec [7,2,3]))
