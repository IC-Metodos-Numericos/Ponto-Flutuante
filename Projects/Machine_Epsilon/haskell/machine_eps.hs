import Text.Printf (printf)

machinePrecisionSingle :: Float -> Float
machinePrecisionSingle val = loop 1.0
  where
    loop a =
      let s = val + a
      in if s > val
         then loop (a / 2)
         else 2 * a

machinePrecisionDouble :: Double -> Double
machinePrecisionDouble val = loop 1.0
  where
    loop a =
      let s = val + a
      in if s > val
         then loop (a / 2)
         else 2 * a

values :: [Double]
values = [1,10,17,100,184,1000,1575,10000,17893]

main :: IO ()
main = do
    putStrLn "Machine Epsilon Single (float32):"
    mapM_ (\val -> let eps = machinePrecisionSingle (realToFrac val :: Float)
                   in printf "VAL = %d -> Epsilon = %.10e\n" (round val :: Int) eps
           ) values

    putStrLn "\nMachine Epsilon Double (float64):"
    mapM_ (\val -> let eps = machinePrecisionDouble val
                   in printf "VAL = %d -> Epsilon = %.10e\n" (round val :: Int) eps
           ) values

-- Results:
-- Machine Epsilon Single (float32):
-- VAL = 1 -> Epsilon = 1.1920929000e-7
-- VAL = 10 -> Epsilon = 9.5367430000e-7
-- VAL = 17 -> Epsilon = 1.9073486000e-6
-- VAL = 100 -> Epsilon = 7.6293945000e-6
-- VAL = 184 -> Epsilon = 1.5258789000e-5
-- VAL = 1000 -> Epsilon = 6.1035156000e-5
-- VAL = 1575 -> Epsilon = 1.2207031000e-4
-- VAL = 10000 -> Epsilon = 9.7656250000e-4
-- VAL = 17893 -> Epsilon = 1.9531250000e-3

-- Machine Epsilon Double (float64):
-- VAL = 1 -> Epsilon = 2.2204460493e-16
-- VAL = 10 -> Epsilon = 1.7763568394e-15
-- VAL = 17 -> Epsilon = 3.5527136788e-15
-- VAL = 100 -> Epsilon = 1.4210854715e-14
-- VAL = 184 -> Epsilon = 2.8421709430e-14
-- VAL = 1000 -> Epsilon = 1.1368683772e-13
-- VAL = 1575 -> Epsilon = 2.2737367544e-13
-- VAL = 10000 -> Epsilon = 1.8189894035e-12
-- VAL = 17893 -> Epsilon = 3.6379788071e-12