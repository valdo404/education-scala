package io.univalence.education

import monocle.syntax.all._

object optics {
  @main
  def opticsForTheWin(): Unit = {
    case class Guitar(make: String, model: String)
    case class Guitarist(name: String, favoriteGuitar: Guitar)
    case class RockBand(name: String, yearFormed: Int, leadGuitarist: Guitarist)

    import monocle.macros.GenLens
    val streetNumber = GenLens[RockBand](_.leadGuitarist)

    val band = RockBand("The Who", 1960, Guitarist("John", Guitar("ma guitare", "white")))

    // sans monocole
    val modifiedBand = band
      .copy(leadGuitarist = 
        band.leadGuitarist
          .copy(favoriteGuitar = 
            band.leadGuitarist.favoriteGuitar.copy(make = "black")))
    
    println(modifiedBand)

    val value1 = band.focus(_.leadGuitarist.favoriteGuitar.make)
    // avec monocle
    val value: RockBand = value1.replace("black")
    
    println(value)

  }
}
