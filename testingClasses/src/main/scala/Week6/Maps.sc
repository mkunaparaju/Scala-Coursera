val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

capitalOfCountry("US")
//capitalOfCountry("aa")
capitalOfCountry get "AA"
capitalOfCountry get "US"

//since options are case classes e can decompose with pattern matching
def showCapital(country : String) = capitalOfCountry.get(country)  match {
  case Some(capital) => capital
  case None => "mmissing data"
 }

showCapital("US")
showCapital("ANDORRA")


