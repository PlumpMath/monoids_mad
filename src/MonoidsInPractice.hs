module MonoidsInPractice where

import Text.Printf

data OrderLine = OrderLine {
  productCode :: String,
  quantity :: Int,
  total :: Float
}


instance Show OrderLine where
  show orderLine =
    printf "%-10s %5i %6g" olCode olQuantity olTotal
    where
      OrderLine olCode olQuantity olTotal  = orderLine


sampleLines :: [OrderLine]
sampleLines =
  [ OrderLine { productCode = "AAA", quantity = 2, total = 2.99 },
    OrderLine { productCode = "BBB", quantity = 1, total = 1.99 },
    OrderLine { productCode = "CCC", quantity = 3, total = 3.99 }
  ]


moreSampleLines :: [OrderLine]
moreSampleLines =
  [ OrderLine { productCode = "DDD", quantity = 4, total = 4.99 },
    OrderLine { productCode = "BBB", quantity = 1, total = 1.99 },
    OrderLine { productCode = "EEE", quantity = 2, total = 2.00 }
  ]


main = do
  mapM_ print sampleLines
  mapM_ print moreSampleLines
