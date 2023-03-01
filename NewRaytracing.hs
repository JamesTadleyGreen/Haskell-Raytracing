import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

class NumVector v where
  (>+) :: Num a => v a -> v a -> v a
  (>-) :: Num a => v a -> v a -> v a
  prod :: Num a => v a -> v a -> v a
  (>*) :: Num a => a -> v a -> v a
  (>.) :: Num a => v a -> v a -> a
  neg :: Num a => v a -> v a
  mag :: v Float -> Float

data Vector a = Vec {vecList :: [a]}
  deriving (Show, Eq, Ord)

instance NumVector Vector where
  (>+) (Vec u) (Vec v) = Vec $ zipWith (+) u v
  (>-) (Vec u) (Vec v) = Vec $ zipWith (-) u v
  (>*) k (Vec v) = Vec $ map (k *) v
  prod (Vec u) (Vec v) = Vec $ zipWith (*) u v
  (>.) u v = sum $ vecList (u `prod` v)
  neg = (>*) (-1)
  mag v = sqrt $ sum $ map (** 2) $ vecList v

cross :: Num a => Vector a -> Vector a -> Vector a
cross (Vec [a, b, c]) (Vec [x, y, z]) = Vec [b * z + c * y, -(a * z + c * x), a * y + b * x]

clamp :: (Num a, Ord a) => Vector a -> Vector a
clamp (Vec u) = Vec $ map (min 1) $ map (max 0) u

type Point = Vector

type Direction = Vector

type Time a = a

type Ray a = (Point a, Direction a) -- base and direction

position_at_time :: Num a => Ray a -> Time a -> Point a
position_at_time (base, dir) t = base >+ (t >* dir)

-- Colours
red = Vec [1, 0, 0]

green = Vec [0, 1, 0]

blue = Vec [0, 0, 1]

white = Vec [1, 1, 1]

black = Vec [0, 0, 0]

midGrey = Vec [0.5, 0.5, 0.5]

nearlyWhite = Vec [0.8, 0.8, 0.8]

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
checked_matt (Vec [x, y, z]) =
  let xeven = even (truncate (x / 20.0))
      yeven = even (truncate (y / 20.0))
      zeven = even (truncate (z / 20.0))
   in if (xeven `xor` yeven `xor` zeven) then (white, 0, 1) else (black, 0, 1)

-- Shapes
type Radius = Float

data Shape a
  = Sphere (Point a) Radius ((Point a) -> (Material a))
  | Plane (Normal a) a ((Point a) -> (Material a))

-- Intersections
type Normal = Vector

type Intersection a = (Normal a, Point a, Ray a, Material a)

roots :: Float -> Float -> Float -> [Float]
roots a b c =
  let discriminant = b * b - 4 * a * c
   in if (discriminant < 0.0)
        then []
        else [0.5 * (-b + sqrt discriminant), 0.5 * (-b - sqrt discriminant)]

normalize :: Vector Float -> Vector Float
normalize v@(Vec v')
  | mag v == 0 = Vec $ map (0 *) v'
  | otherwise = (1 / mag v) >* v

intersect :: Ray Float -> Shape Float -> Maybe (Time Float, Intersection Float)
intersect ray@(base, dir) (Plane normal d materialFunction) =
  let nnormal = normalize normal
      vd = nnormal >. dir
      v0 = negate $ (nnormal >. base) + d
   in if (vd <= 0)
        then Nothing
        else
          let t = v0 / vd
              point = position_at_time ray t
           in if t > 0 then Just (t, (normal, point, ray, materialFunction point)) else Nothing
intersect ray@(base, dir) (Sphere centre radius materialFunction) =
  let ndir = normalize dir
      b = 2 * (ndir >. (base >- centre))
      c = (mag (base >- centre)) ** 2
      root = filter (> 0) (roots 1 b c)
      normal_at_time t = normalize ((position_at_time ray t) >- centre)
   in case root of
        [] -> Nothing
        otherwise -> Just $ (t, (normal_at_time t, position_at_time ray t, ray, materialFunction (position_at_time ray t)))
          where
            t = minimum root

-- Lights:  We have a non-shadowable Directional light and a shadowable spotlight
data Light a
  = Directional (Vector a) (Vector a)
  | Spotlight (Point a) (Vector a)

-- Global bindings

-- If a ray doesn't hit an object, what color should we use?
backgroundColor :: Vector Float
backgroundColor = black

-- What lights are in our scene?

lights :: [Light Float]
lights =
  [ Spotlight (Vec [100, -30, 0]) nearlyWhite,
    Spotlight (Vec [-100, -100, 150]) nearlyWhite
  ]

-- What is the ambient lighting in the scene
ambient_light :: Vector Float
ambient_light = Vec [1.1, 1.1, 1.1]

-- What Shapes are in our scene?
shapes :: [Shape Float]
shapes =
  [ Plane (normalize (Vec [0, -1, 0])) 50 shinyred,
    Sphere (Vec [50, 10, 100]) 40 semishinygreen,
    Sphere (Vec [-80, 0, 80]) 50 checked_matt
  ]

-- Is the light at 'lightpos' visible from point?
-- TODO We can halt this function early if we hit a False
point_is_lit :: Vector Float -> Vector Float -> Bool
point_is_lit point lightpos =
  let path = lightpos >- point
      time_at_light = mag path
      ray = (point, normalize path)
      hits = map (intersect ray) shapes
      minTime _ False = False
      minTime Nothing _ = True
      minTime _ _ = True
   in foldr minTime True hits

-- Helper to calculate the diffuse light at the surface normal, given
-- the light direction (from light source to surface)
diffuse_coeff :: Vector Float -> Vector Float -> Float
diffuse_coeff light_dir normal = max 0 (negate ((normalize light_dir) >. (normalize normal)))

local_light :: Intersection Float -> Light Float -> Vector Float
-- Simple case of a non-shadowable directional light
local_light (normal, _, _, (materialcol, _, kd)) (Directional dir lightcol) =
  let mixed_color = materialcol `prod` lightcol
      diffuse = ((diffuse_coeff dir normal) * kd) >* mixed_color
   in diffuse
-- Spotlight - shadowable
local_light (normal, hitpoint, _, (materialcol, _, kd)) (Spotlight lightpos lightcol) =
  let mixed_color = materialcol `prod` lightcol
      diffuse = (kd * (diffuse_coeff (hitpoint >- lightpos) normal)) >* mixed_color
   in if (point_is_lit hitpoint lightpos) then diffuse else black

-- Reflections (part of the global lighting model)

-- Ray trace the outgoing reflected ray from an intersection (depth is the level of recursion
-- which we're at in the ray tracing)
reflected_ray :: Integer -> Intersection Float -> Vector Float
reflected_ray depth (normal, hitpoint, (_, in_ray_dir), (color, kr, _))
  | kr == 0.0 = black
  | otherwise =
      let k = 2 * ((normalize normal) >. (normalize (neg in_ray_dir)))
          out_ray_dir = (k >* (normalize normal)) >- (neg in_ray_dir)
          reflected_col = raytrace (depth + 1) (hitpoint, out_ray_dir)
       in kr >* reflected_col

-- Image output: We can write a ppm (Portable Pixmap) file by converting a list of
-- colors (length is width * height) into a big string
make_pgm :: Integer -> Integer -> [Vector Float] -> String
make_pgm width height xs = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n" ++ stringify (xs)
  where
    stringify [] = ""
    stringify (Vec [r, g, b] : xs) =
      show (round (r * 255))
        ++ " "
        ++ show (round (g * 255))
        ++ " "
        ++ show (round (b * 255))
        ++ " "
        ++ stringify xs

-- Viewing screen and camera functions:  We define the camera position and the point which
-- we're looking at.  We also define an 'up' vector and a distance to the screen.  The
-- following functions generate a list of points (in raster order - a fact which is relied
-- upon when we write the image out as a ppm) which correspond to the 3d positions of the
-- pixels on our virtual screen.

-- Camera position, distance to screen, "Looking at" position, up vector
type View = (Point Float, Float, Point Float, Vector Float)

pixel_grid :: View -> Float -> Float -> [Point Float]
pixel_grid (camerapos, viewdist, lookingat, viewup) width height =
  let grid = [Vec [x, y, 0] | y <- [0 .. width - 1], x <- [0 .. height - 1]]
      centering_offset = Vec [-width / 2.0, -height / 2.0, 0]
      pixel_offsets = map ((>+) centering_offset) grid
      viewdir = normalize (lookingat >- camerapos)
      screen_center = camerapos >+ (viewdist >* viewdir)
      viewright = viewdir `cross` viewup
      transform (Vec [x, y, _]) = screen_center >+ (x >* viewright) >+ (y >* neg viewup)
   in map transform pixel_offsets

-- Parallel projection function which creates rays parallel to the viewing screen
parallel_projection :: View -> Point Float -> Ray Float
parallel_projection (camerapos, _, lookingat, _) point = (point, normalize (lookingat >- camerapos))

-- Perspective projection which creates rays through (0,0,-distance) through the point
perspective_projection :: View -> Point Float -> Ray Float
perspective_projection (camerapos, _, _, _) point = (point, normalize (point >- camerapos))

-- Main rendering functions

-- Calculate the overall color of a ray/shape intersection, taking into account
-- local lighting (diffuse only) and global lighting (reflections only, to a depth
-- of 2 bounces)
overall_lighting :: Integer -> Intersection Float -> Vector Float
overall_lighting depth hit =
  let sum_colors = foldr (>+) black
      local_lighting = ambient_light >+ sum_colors (map (local_light hit) lights)
      global_lighting = if (depth < 2) then (reflected_ray depth hit) else black
   in clamp (local_lighting >+ global_lighting)

-- Trace a ray through the scene and work out what color it should be.
-- Takes a 'depth' argument which is 0 for top level viewing rays increases
-- by one for each level of recursive raytracing we do (as a result of reflections
-- or transmissions)
raytrace :: Integer -> Ray Float -> Vector Float -- uses global 'shapes'
raytrace depth ray =
  let hits = mapMaybe (intersect ray) shapes
   in if null hits
        then backgroundColor
        else overall_lighting depth (snd (minimumBy (comparing fst) hits))

-- Testing: We define a function 'test' which renders a fixed view and writes
-- out the result as c:/test.ppm
-- This writes out a pgm of our trivial rendering, given the screen width and height
-- render_to_pgm :: Float -> Float -> String
render_to_pgm width height =
  let view = (Vec [0, 0, -100], 100, Vec [0, 0, 100], Vec [0, -1, 0])
      projection = perspective_projection view
      ray_collection = map projection (pixel_grid view width height)
      color_collection = map (raytrace 0) ray_collection
   in make_pgm (round width) (round height) color_collection

main = writeFile "test.ppm" (render_to_pgm 500 500)

-- main = print (filter ((/=) (Vec [1, 1, 1])) (render_to_pgm 500 500)) -- writeFile "test.ppm" (render_to_pgm 500 500)
