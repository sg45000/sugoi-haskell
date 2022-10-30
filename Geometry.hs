module Geometry (
    sphereVolume,
    sphereArea,
    cubeVolume,
    cubeArea,
    cuboidArea,
    cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side
cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea h w d = rectArea h w * 2 + rectArea h d * 2 + rectArea w d * 2
cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume h w d = rectArea d $ rectArea h w

rectArea :: Float -> Float -> Float
rectArea a b = a * b