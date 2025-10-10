module CreatePhoneNumber where
import Data.List.Split (splitPlaces)

createPhoneNumber :: [Int] -> String
createPhoneNumber numbers = 
  let (areaCode:telephonePrefix:lineNumber:_) = splitPlaces [3, 3, 4] $ concatMap show numbers
  in "(" ++ areaCode ++ ") " ++ telephonePrefix ++ "-" ++ lineNumber
