
module Date (Date, Month (..), daysInMonth, months, createDate) where

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Show, Eq, Ord, Enum, Bounded)

data Date = Date {day :: Int, month :: Month, year :: Int} deriving (Show, Eq)

createDate :: Int -> Month -> Int -> Maybe Date
createDate day month year =
  let between start end num = (start <= num) && (num <= end)
      lastDay = daysInMonth month
  in 
    if not (between 1 lastDay day) || not (between 1970 3000 year) then 
      Nothing
    else 
      Just (Date {day = day, month = month, year = year})

daysInMonth :: Month -> Int
daysInMonth month =
  case month of
    Jan -> 31
    Feb -> 28
    Mar -> 31
    Apr -> 30
    May -> 31
    Jun -> 30
    Jul -> 31
    Aug -> 31
    Sep -> 30
    Oct -> 31
    Nov -> 30
    Dec -> 31

months :: [Month]
months = [Jan .. Dec]

