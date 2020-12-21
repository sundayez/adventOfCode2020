package com.bernar.adventofcode2020

object Day21 extends App {
  val lines = FileInput.readLinesFromFile("input21.txt")
  val foods = lines.map(foodFromString)
  foods.foreach(println)

  val ingredients = foods.flatMap(_.ingredients).distinct.map(ingredient => (ingredient, "")).toMap
  val allergens = foods.flatMap(_.allergens).distinct.toSet
  println(ingredients)
  println(allergens)
  val ingredientsWithAllergen = allergens.flatMap(allergen =>
    foods.filter(food => food.allergens.contains(allergen))
      .map(_.ingredients)
      .reduce((ingredients1, ingredients2) => ingredients1.intersect(ingredients2))
  )

  println(foods.map(food => food.ingredients.count(ingredient => !ingredientsWithAllergen.contains(ingredient))).sum)

  println(ingredientsWithAllergen)

  //Part 2
  val ingredientsByAllergen: Map[String, List[String]] = allergens.zip(allergens.map(allergen =>
    foods.filter(food => food.allergens.contains(allergen))
      .map(_.ingredients)
      .reduce((ingredients1, ingredients2) => ingredients1.intersect(ingredients2))
  )).toMap

  /**
   * HashSet(soy, peanuts, dairy, wheat, sesame, fish, nuts, shellfish)
   * HashSet(List(cpbzbx, kqprv), List(bdvmx, drbm), List(cfnt, bsqh, kqprv), List(drbm, lmzg, bdvmx, bsqh), List(bdvmx), List(cfnt, kqprv, drbm), List(cxk, kqprv, bdvmx), List(bdvmx, cfnt))
   *
   * bdvmx -> sesame
   * HashSet(soy, peanuts, dairy, wheat, fish, nuts, shellfish)
   * HashSet(List(cpbzbx, kqprv), List(drbm), List(cfnt, bsqh, kqprv), List(drbm, lmzg, bsqh), List(cfnt, kqprv, drbm), List(cxk, kqprv), List(cfnt))
   * drbm -> peanuts
   * cfnt -> shellfish
   *
   * HashSet(soy, dairy, wheat, fish, nuts)
   * HashSet(List(cpbzbx, kqprv), List(bsqh, kqprv), List(lmzg, bsqh), List(kqprv), List(cxk, kqprv))
   * kqprv -> fish
   *
   * HashSet(soy, dairy, wheat, nuts)
   * HashSet(List(cpbzbx), List(bsqh), List(lmzg, bsqh), List(cxk))
   * cpbzbx -> soy
   * cxk -> nuts
   * bsqh -> dairy
   *
   * HashSet(wheat)
   * HashSet(List(lmzg))
   * lmzg -> wheat
   */

  /**
   * bsqh -> dairy
   * kqprv -> fish
   * cxk -> nuts
   * drbm -> peanuts
   * bdvmx -> sesame
   * cfnt -> shellfish
   * cpbzbx -> soy
   * lmzg -> wheat
   *
   * bsqh,kqprv,cxk,drbm,bdvmx,cfnt,cpbzbx,lmzg
   */

  println(ingredientsByAllergen)
  val currentKey = ingredientsByAllergen.keys.filter(key => ingredientsByAllergen(key).size == 1)
  def foodFromString(line: String): Food = {
    val ingredientsAllergens = line.split(" \\(")
    val ingredients = ingredientsAllergens.head.split(" ").toList
    val allergens = ingredientsAllergens(1).replaceAll("contains ", "").replaceAll("\\)", "").split(", ").toList
      Food(ingredients, allergens)
  }

  case class Food(ingredients: List[String], allergens: List[String])
}

