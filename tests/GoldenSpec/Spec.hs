{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-type-defaults        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GoldenSpec.Spec (spec) where

import GoldenSpec.Util (golden, goldenAllFormats, goldenFormat2)
import Graphics.Implicit
import Graphics.Implicit.Export.OutputFormat (OutputFormat (PNG))
import Prelude
import Test.Hspec ( describe, Spec )
import Graphics.Implicit.Primitives (torus, ellipsoid, cone)

default (Int)

spec :: Spec
spec = describe "golden tests" $ do
  golden "box" 1 $
    cube True (V3 5 5 5)

  goldenAllFormats "boxCylinder" 1 $
    union
      [ cube False (V3 5 5 5)
      , translate (V3 2.5 2.5 5) $ cylinder2 2 1 3
      ]

  golden "example13" 1 $
    union [ rect3 (V3 0 0 0) (V3 20 20 20)
          , translate (V3 20 20 20) (sphere 15)
          ]

  golden "example16" 1 $
    implicit
      (\(V3 x y z) -> x^4 + y^4 + z^4 - 15000)
      ( V3 (-20) (-20) (-20) ,V3 20 20 20)

  golden "example17" 1 $
    let
      squarePipe :: ℝ3 -> ℝ -> ℝ -> SymbolicObj3
      squarePipe (V3 x y z) diameter precision =
            union
            ((\(a, b, c) -> translate (V3 a b c)
               $ rect3 (V3 0 0 0) (V3 diameter diameter diameter)
             )
             <$>
              zip3 (fmap (\n->(fromIntegral n/precision)*x) [0..100])
                   (fmap (\n->(fromIntegral n/precision)*y) [0..100])
                   (fmap (\n->(fromIntegral n/precision)*z) [0..100]))
     in squarePipe (V3 10 10 10) 1 100

  golden "wheel-well" 1 $
    differenceR 0.0
      (translate (V3 6.0 (-0.0) (-44.5))
        (unionR 0.0
          [ translate (V3 (-17.0) 0.0 0.0)
              (translate (V3 0.0 0.0 38.0)
                (rotate3 (V3 0 (pi / 2) 0)
                  (translate (V3 3.0 (-0.0) (-17.0))
                    (unionR 0.0
                      [ translate (V3 0.0 0.0 (-0.0))
                          (intersectR 0.0
                            [ shell 2.0
                                (outset 5.0
                                  (cylinder 35.0 28.0))
                            , cylinder 50.0 28.0
                            , translate (V3 11.0 0.0 0.0)
                                (translate (V3 (-50.0) 0.0 0.0)
                                  (translate (V3 0.0 0.0 14.0)
                                    (translate (V3 (-50.0) (-50.0) (-14.0))
                                      (cube False (V3 100.0 100.0 28.0)))))
                            ])
                      ]))))
          , translate (V3 (-2.0) 0.0 0.0)
              (translate (V3 12.0 0.0 0.0)
                (translate (V3 0.0 0.0 24.0)
                  (translate (V3 0.0 0.0 (-0.0))
                    (unionR 0.0
                      [ intersectR 0.0
                        [ translate (V3 0.0 0.0 31.5)
                            (shell 2.0
                              (scale (V3 1.1578947368421053 1.1363636363636365 1.0307692307692307)
                                (translate (V3 (-9.5) (-11.0) (-32.5))
                                  (cube False (V3 19.0 22.0 65.0)))))
                        , translate (V3 (-12.0) (-13.500000000000002) 0.0)
                            (cube False (V3 24.0 27.0 2.0))
                        ]
                      ]))))
          ]))
      [ translate (V3 6.0 (-0.0) (-44.5))
          (unionR 0.0
            [ translate (V3 (-17.0) 0.0 0.0)
                (translate (V3 0.0 0.0 38.0)
                  (rotate3 (V3 0 (pi / 2) 0)
                    (translate (V3 3.0 (-0.0) (-17.0))
                      (unionR 0.0
                        [ translate (V3 0.0 0.0 0.0)
                            (translate (V3 0.0 0.0 17.0)
                              (translate (V3 (-0.0) (-0.0) 11.0)
                                (unionR 0.0
                                  [ translate (V3 0.0 0.0 (-0.0))
                                      (cylinder 3.0 6.0)
                                  , translate (V3 0.0 0.0 0.0)
                                      (translate (V3 0.0 0.0 (-28.0))
                                        (cylinder 35.0 28.0))
                                  ])))
                        ]))))
            , translate (V3 (-2.0) 0.0 0.0)
                (translate (V3 12.0 0.0 0.0)
                  (translate (V3 0.0 0.0 24.0)
                    (translate (V3 0.0 0.0 (-0.0))
                      (translate (V3 0.0 0.0 32.5)
                        (translate (V3 (-9.5) (-11.0) (-32.5))
                          (cube False (V3 19.0 22.0 65.0)))))))
            ])
      ]

  -- These tests were generated by the Arbitrary instance
  golden "arbitrary1" 1 $
    cylinder 16.76324 21.02933

  golden "arbitrary2" 1 $
    translate (V3 24.07554 26.31483 24.96913)
     . scale (V3 3.6096 4.9768 2.9848)
     . translate (V3 (-1.2054) (-0.4034) (-0.725975))
     $ withRounding 0.45186 $ cube False (V3 2.41095 0.8068 1.45195)

  golden "arbitrary3" 1 $
    differenceR 1.8 (sphere 4.6)
      [ rotate3 (V3 (-0.3)  0.4  0.36)
          $ scale (V3 1  1.3  1.4)
          $ cylinder2 0.6 0.74 1
      , sphere 1.2
      , rotate3 (V3 0.54  (-0.45)  (-0.58))
          $ withRounding 1.4
          $ cube True (V3 1.5  1.81  1.82)
      , cylinder2 1.7 1.5 3.5
      , sphere 1.54
      ]

  golden "arbitrary4" 1 $
    unionR 1.8
      [ sphere 4.6
      , rotate3 (V3 (-0.3)  0.4  0.36)
          $ scale (V3 1  1.3  1.4)
          $ cylinder2 0.6 0.74 1
      , sphere 1.2
      , rotate3 (V3 0.54  (-0.45)  (-0.58))
          $ withRounding 1.4
          $ cube True (V3 1.5  1.81  1.82)
      , cylinder2 1.7 1.5 3.5
      , sphere 1.54
      ]

  golden "hook" 2 $
    union
      [ translate (V3 0 60 0) $
          rotateExtrude (3 * pi / 2) (Left 0) (Left 0) $
            translate (V2 40 0) $
              circle 10
      , rotateExtrude (pi / 2) (Left 0) (Left 0) $
          translate (V2 20 0) $
            circle 10
      , translate (V3 20 0 0) $
          rotate3 (V3 (pi / 2) 0 0) $
            cylinder 10 80
      ]

  golden "torusEllipsoidCone" 2 $
    union
      [ torus 40 15
      , ellipsoid 10 15 20
      , translate (V3 0 0 25) $ cone 20 20
      ]

  golden "closing-paths-1" 0.5 $
    extrudeM
      (Left 0)
      (C1 1)
      (Left 0)
      (circle 1)
      -- limit height or this races off to infinity
      -- and makes the tests take forever, eating all
      -- your RAM while it tries to calculate the surface
      $ Right $ \(V2 x y) -> min 100 $ 1 / sqrt (x ^ 2 + y ^ 2)
  golden "closing-paths-2" 1 $
    extrudeM
      -- Note, this have a gap from the base plane and the extruded
      -- object, and the base of the extruded object is going to be
      -- missing. This is because we are directly using haskell
      -- functions rather than the clamped / Infinity / NaN checked
      -- versions that the parser is set up to provide.
      -- However, this is still useful in that it doesn't crash on
      -- unclosed loops.
      (pure $ \h -> 35 * log (h*2*pi/30))
      (C1 1)
      (Left 0)
      (union [circle 10])
      $ Left 40

  -- These two should be equal, but internally when sampled at (V2 (-1) 0)
  -- the sign of the SDF differs yet they both get rendered correctly.
  let funPoly = polygon [V2 0 0, V2 0 (-0.1), V2 (-2) 0, V2 0 (-1)]
      rotFunPoly = rotate (2*pi) funPoly
  --
  -- > getImplicit funPoly (V2 (-1) 0)
  -- -4.993761694389224e-2
  -- > getBox funPoly
  -- (V2 (-2.0) (-1.0),V2 0.0 0.0)
  --
  -- vs
  --
  -- > getImplicit rotFunPoly (V2 (-1) 0))
  -- 4.9937616943891996e-2
  -- > getBox rotFunPoly
  -- (V2 (-2.0000000000000004) (-1.0),V2 0.0 4.898587196589413e-16)
  --
  -- TODO(srk): investigate, see also #449

  describe "2d" $ do
    goldenFormat2 PNG "troublesome-polygon" 1 funPoly
    goldenFormat2 PNG "troublesome-polygon-under-rotation" 1 rotFunPoly

  golden "shell" 0.5 $
    let radius :: ℝ = 10
        radius2 = radius * 2
        shellWidth :: ℝ = 1
    in
      union
        -- make a shell and slice the bottom off so we can inspect the wall
        [ differenceR 0 (shell shellWidth $ sphere radius)
          [ translate (V3 (-radius) (-radius) (-radius)) $ cube False (V3 radius2 radius2 radius)
          ]
        -- Make a cube with the same radius as the sphere and moved upwards
        -- so that it is just touching the top of the sphere. This lets us
        -- easily check if the radius is being messed with for some reason.
        -- The render quality will need to be increased a lot to actually see
        -- if this is working, but you will get a feel for when it is correct
        -- and the STL is just showing resolution limits
        , translate (V3 (-radius) (-radius) radius) . cube False $ V3 radius2 radius2 radius
        -- Make a cube with the same dimentions as the shell thickness
        -- and place it on the lip of the shell so we can check if the thickness
        -- is actually correct
        , translate (V3 (radius-shellWidth) (-(shellWidth/2)) (-shellWidth)) . cube False $
          V3 shellWidth shellWidth shellWidth
        ]
