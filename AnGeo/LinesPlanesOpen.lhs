{-# UnicodeSyntax #-}
Именно эти задания подлежат проверке и обязательному исполнению в 2022 году


\begin{code}

module LinesPlanesOpen where

import AnGeo
import Lines
import LinesPlanes
\end{code}


---------------------------------------------------------------------------------------------------
При решении задач, имея плоскость, представленную нормалью и точкой, все равно придется
строить каноническое уравнение плоскости, поэтому во многих задачах далее я переводил плоскость 
в канонический вид и использовал функцию для канонического уравнения
---------------------------------------------------------------------------------------------------

Преобразование типов плоскостей:

\begin{code}
planeToCPlane :: Plane -> CPlane
planeToCPlane (Pl (Vc x0 y0 z0) (Vc xa ya za)) = 
            (CPl xa ya za (- x0 * xa - y0 * ya - z0 * za))

cplaneToPlane :: CPlane -> Plane
cplaneToPlane (CPl a b c d) | c /= 0    = Pl (Vc 0 0 (- d / c)) (Vc a b c)
                            | b /= 0    = Pl (Vc 0 (- d / b) 0) (Vc a b c)
                            | a /= 0    = Pl (Vc (- d / a) 0 0) (Vc a b c)
                            | d /= 0    = error "Impossible plane"
                            | otherwise = error "Plane is the whole universe"
\end{code}

Красивое отображение канонической плоскости в виде уравнения (Ax + By + Cz + D = 0):

\begin{code}
instance Show CPlane where
  show (CPl a b c d) = "( " ++ show a ++ "x"
                        ++ showNum b ++ "y" 
                        ++ showNum c ++ "z"
                        ++ showNum d ++ " = 0 )"
    where showNum :: Double -> String
          showNum x | x >= 0   = " + " ++ show x
                    | x < 0    = " - " ++ show (abs x)
\end{code}

отображение плоскости в каком-либо приемлемом виде :

\begin{code}
instance Show Plane where
    show (Pl (Vc x0 y0 z0) (Vc xa ya za)) = 
                    "( (" ++ show x0 ++ " " ++ show y0 ++ " " ++ show z0 ++ ") " 
                   ++ "(" ++ show xa ++ " " ++ show ya ++ " " ++ show za ++ ") )"

\end{code}


Проверка принадлежности точки плоскости (в обеих формах)

\begin{code}
pointOnPlane :: Point -> Plane -> Bool
pointOnPlane point plane = pointOnCPlane point (planeToCPlane plane)


pointOnCPlane :: Point -> CPlane -> Bool
pointOnCPlane (Pt x0 y0 z0) (CPl a b c d) = a * x0 + b * y0 + c * z0 + d == 0
\end{code}

Проверка принадлежности прямой плоскости

\begin{code}
lineOnPlane  :: Line -> Plane -> Bool
lineOnPlane line plane = lineOnCPlane line (planeToCPlane plane)

lineOnCPlane :: Line -> CPlane -> Bool
lineOnCPlane (Ln (Vc x0 y0 z0) (Vc xa ya za)) (CPl a b c d) = 
    a * x0 + b * y0 + c * z0 + d == 0 
    &&
    a * xa + b * ya + c * za == 0
\end{code}

Проверка совпадения двух плоскостей

\begin{code}

instance Eq CPlane where 
  (CPl a1 b1 c1 d1) == (CPl a2 b2 c2 d2) =
      (a1 / a2) == (b1 / b2) 
    && (b1 / b2) == (c1 / c2) 
    && (c1 / c2) == (d1 / d2)

instance Eq Plane where
  pl1 == pl2  =  planeToCPlane pl1 == planeToCPlane pl2
      

\end{code}

Проверка параллельности двух плоскостей

\begin{code}
planeParall :: Plane -> Plane -> Bool
planeParall pl1 pl2 = cplaneParall (planeToCPlane pl1) (planeToCPlane pl2)
    

cplaneParall :: CPlane -> CPlane -> Bool
cplaneParall (CPl a1 b1 c1 d1) (CPl a2 b2 c2 d2) = 
  ((a1 / a2) == (b1 / b2) && (b1 / b2) == (c1 / c2) 
  && (c1 / c2) /= (d1 / d2))
\end{code}

Проверка перпендикулярности двух плоскостей

\begin{code}
planePerp :: Plane -> Plane -> Bool
planePerp p1 p2 = (normal p1) `perp` (normal p2)
\end{code}

\begin{code}
cplanePerp :: CPlane -> CPlane -> Bool
cplanePerp (CPl a1 b1 c1 d1) (CPl a2 b2 c2 d2) = 
  (a1 * a2 + b1 * b2 + c1 * c2 == 0)


\end{code}

Проверка параллельности прямой и плоскости

\begin{code}
lineAndPlaneParall :: Line -> Plane -> Bool
lineAndPlaneParall line plane = (dir line) ┴ (normal plane)
\end{code}

\begin{code}
lineAndCPlaneParall :: Line -> CPlane -> Bool
lineAndCPlaneParall (Ln r0 (Vc x0 y0 z0)) (CPl a b c d) = 
    a * x0 + b * y0 + c * z0 == 0
\end{code}

Проверка перпендикулярности прямой и плоскости

\begin{code}
linePlanePerp  :: Line -> Plane -> Bool
linePlanePerp (Ln r0 (Vc x0 y0 z0)) (Pl _ (Vc a b c)) = 
  x0 / a == y0 / b && y0 / b == z0 / c

lineCPlanePerp :: Line -> CPlane -> Bool
lineCPlanePerp (Ln r0 (Vc x0 y0 z0)) (CPl a b c d) = 
  x0 / a == y0 / b && y0 / b == z0 / c
\end{code}

Нахождение угла между плоскостями (в градусах бы)...

\begin{code}
planeAngle  :: Plane -> Plane  -> Double
planeAngle (Pl _ (Vc a1 b1 c1)) (Pl _ (Vc a2 b2 c2)) = 
  acos (abs(a1 * a2 + b1 * b2 + c1 * c2) / 
          (sqrt(a1 ^ 2 + b1 ^ 2 + c1 ^ 2) * sqrt(a2 ^ 2 + b2 ^ 2 + c2 ^ 2)))

cplaneAngle :: CPlane -> CPlane  -> Double
cplaneAngle (CPl a1 b1 c1 d1) (CPl a2 b2 c2 d2) = 
  acos (abs(a1 * a2 + b1 * b2 + c1 * c2) / 
          (sqrt(a1 ^ 2 + b1 ^ 2 + c1 ^ 2) * sqrt(a2 ^ 2 + b2 ^ 2 + c2 ^ 2)))

\end{code}

Нахождение угла между прямой и плоскостью (в градусах бы)...

\begin{code}

lineAndPlaneAngle ::  Line -> Plane -> Double
lineAndPlaneAngle (Ln r0 (Vc a1 b1 c1)) (Pl _ (Vc a2 b2 c2)) = 
  asin (abs(a1 * a2 + b1 * b2 + c1 * c2) / 
    (sqrt(a1 ^ 2 + b1 ^ 2 + c1 ^ 2) * sqrt(a2 ^ 2 + b2 ^ 2 + c2 ^ 2)))

-- https://imgur.com/a/OiBL8cA
-- объяснение почему синус
lineAndCPlaneAngle :: Line -> CPlane  -> Double
lineAndCPlaneAngle (Ln r0 (Vc a1 b1 c1)) (CPl a2 b2 c2 d2) = 
  asin (abs(a1 * a2 + b1 * b2 + c1 * c2) / 
    (sqrt(a1 ^ 2 + b1 ^ 2 + c1 ^ 2) * sqrt(a2 ^ 2 + b2 ^ 2 + c2 ^ 2)))

\end{code}

Нахождение расстояния между точкой и плоскостью

\begin{code}
pointToPLaneDistance :: Point -> Plane -> Double
pointToPLaneDistance point plane = pointToCPLaneDistance point (planeToCPlane plane)

pointToCPLaneDistance :: Point -> CPlane -> Double
pointToCPLaneDistance (Pt x0 y0 z0) (CPl a b c d) = 
  abs (a * x0 + b * y0 + c * z0 + d) / sqrt (a ^ 2 + b ^ 2 + c ^ 2)
\end{code}

Нахождение линии пересечения двух плоскостей

\begin{code}

instance Show Line where
    show (Ln (Vc x0 y0 z0) (Vc xa ya za)) = "Point (" ++ show x0 ++ " " ++ show y0 ++ " " ++ show z0 ++ ") "
                                        ++ " Vec (" ++ show xa ++ " " ++ show ya ++ " " ++ show za ++ ")"


lineIntersectionOf2Planes :: Plane -> Plane -> Line
lineIntersectionOf2Planes pl1 pl2 
            = lineIntersectionOf2CPlanes (planeToCPlane pl1) (planeToCPlane pl2)

--http://www.cleverstudents.ru/line_and_plane/equations_of_line_as_equations_of_two_planes.html
--метод решения взял отсюда
--и примеры для проверки тоже

--lineIntersectionOf2CPlanes (CPl 1 0 3 7) (CPl 2 3 3 2)
--Point (-7.0 4.0 0.0)  Vec (-9.0 3.0 3.0)

--lineIntersectionOf2CPlanes (CPl 1 2 (-3) (-2)) (CPl 1 0 (-1) 4)
--Point (-4.0 3.0 0.0)  Vec (-2.0 -2.0 -2.0)

lineIntersectionOf2CPlanes :: CPlane -> CPlane -> Line
lineIntersectionOf2CPlanes (CPl a1 b1 c1 d1) (CPl a2 b2 c2 d2) = do
        let detZ = a1 * b2 - b1 * a2
        let detY = a1 * c2 - c1 * a2 
        let detX = b1 * c2 - c1 * b2
        
        let i = b1 * c2 - c1 * b2
        let j = - (a1 * c2 - c1 * a2)
        let k = a1 * b2 - b1 * a2

        if (detZ /= 0) then do

            let x_y = twoVariablesSolver (a1, b1, (-d1)) (a2, b2, (-d2))
            Ln (Vc (fst x_y) (snd x_y) 0) (Vc i j k)
        
        else if (detY /= 0) then do
        
            let x_z = twoVariablesSolver (a1, (-d1), c1) (a2, (-d2), c2)
            Ln (Vc (fst x_z) 0 (snd x_z)) (Vc i j k)
        
        else if (detX /= 0) then do
        
            let y_z = twoVariablesSolver ((-d1), b1, c1) ((-d2), b2, c2)
            Ln (Vc 0 (fst y_z) (snd y_z)) (Vc i j k)
        
        else error "Planes have no intersection"


--решение системы уравнений методом Крамера

twoVariablesSolver :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double)
twoVariablesSolver (a1, b1, y1) (a2, b2, y2) = do
        let det = a1 * b2 - b1 * a2
        let deltaX = y1 * b2 - b1 * y2
        let deltaY = a1 * y2 - y1 * a2
        (deltaX / det, deltaY / det)



\end{code}
