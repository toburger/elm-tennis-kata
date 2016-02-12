module Models (..) where


type Player
  = PlayerOne
  | PlayerTwo


type Point
  = Love
  | Fifteen
  | Thirty


type alias PointsData =
  { playerOnePoint : Point
  , playerTwoPoint : Point
  }


type alias FortyData =
  { player : Player
  , otherPlayerPoint : Point
  }


type Score
  = Points PointsData
  | Forty FortyData
  | Deuce
  | Advantage Player
  | Game Player


type alias Model =
  Score


initialModel : Model
initialModel =
  Points
    { playerOnePoint = Love
    , playerTwoPoint = Love
    }


toInt : Point -> Int
toInt point =
  case point of
    Love ->
      0

    Fifteen ->
      15

    Thirty ->
      30


comparePoint : Point -> Point -> Order
comparePoint point1 point2 =
  compare (toInt point1) (toInt point2)
  
