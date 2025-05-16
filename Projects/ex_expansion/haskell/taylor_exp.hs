import Text.Printf (printf)

-- Função para calcular e^x via série de Taylor até que o termo seja menor que epsilon
taylorExp :: Double -> Int -> Double -> Double
taylorExp x maxTerms epsilon = go 1 1 1
  where
    go :: Int -> Double -> Double -> Double
    go k term sum
      | k >= maxTerms || abs term < epsilon * abs sum = sum
      | otherwise =
          let term' = term * x / fromIntegral k
              sum'  = sum + term'
          in go (k + 1) term' sum'

-- Função para calcular o valor de e^x e os erros diretos e invertidos (caso x < 0)
calculateEx :: Double -> Int -> Int -> (Double, Maybe Double, Double, Maybe Double, Double)
calculateEx x n dps =
  let epsilon = 10 ** (-fromIntegral dps)
      realValue = exp x
      direct = taylorExp x n epsilon
      directError = abs (realValue - direct)
      (inverted, invertedError) =
        if x < 0
        then
          let inv = 1 / taylorExp (-x) n epsilon
              err = abs (realValue - inv)
          in (Just inv, Just err)
        else (Nothing, Nothing)
  in (direct, inverted, directError, invertedError, realValue)

-- Função para exibir os resultados
showEx :: Double -> Int -> Int -> IO ()
showEx x n dps = do
  let (direct, inverted, directError, inverseError, realValue) = calculateEx x n dps
  putStrLn "\n--------------------------------------------\n"
  printf "e^x for x = %.5f with %d terms and precision %d:\n\n" x n dps
  printf "Real value:                  %.20f\n" realValue
  printf "Direct approximation:        %.20f\n" direct
  printf "Direct error:                %.20f\n" directError
  case inverted of
    Just inv -> do
      printf "Inverse approximation (1/e^-x): %.20f\n" inv
      case inverseError of
        Just err -> printf "Inverse error:               %.20f\n" err
        Nothing  -> return ()
    Nothing -> return ()
  putStrLn "\n--------------------------------------------\n"

-- Exemplos de chamada
main :: IO ()
main = do
  showEx 1 5 2
  showEx (-1) 5 15
