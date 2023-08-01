package com.example

import cats.effect.IOApp
import cats.effect.IO
import cats.data.State

case class Product(name: String, price: BigDecimal)

case class ShoppingCart(cartItems: Map[Product, Int]) {

  def addItems(product: Product, quantity: Int): ShoppingCart = {
    if (!cartItems.contains(product)) ShoppingCart(cartItems.updated(product, quantity))
    else ShoppingCart(cartItems.updated(product, cartItems(product) + quantity))
  }

  def showCart: IO[Unit] = IO {
    cartItems.foreach { case (product, qty) =>
      println("Product name:" + product.name + ", " + "Quantity:" + qty)
    }
  }

  def subtotal: IO[BigDecimal] = IO {
    cartItems.map { case (product, qty) =>
      product.price * qty
    }
      .sum
      .setScale(2, BigDecimal.RoundingMode.HALF_UP)
  }

  def taxPayable(total: BigDecimal): IO[BigDecimal] = IO(
    (total * .125)
      .setScale(2, BigDecimal.RoundingMode.HALF_UP)
  )

  def totalPayable(total: BigDecimal, taxPayable: BigDecimal): IO[BigDecimal] = IO(total + taxPayable)

}

object HttpsClient {
  def sendRequest(url: String): ujson.Value = {
    val response = requests.get(url)
    val json = ujson.read(response.text)
    json
  }
}

object Main extends IOApp.Simple {

  val productMapping: Map[String, Product] = Seq("cornflakes","weetabix").foldLeft(Map.empty[String, Product]) { (acc, product) =>
    val uriString = s"https://raw.githubusercontent.com/mattjanks16/shopping-cart-test-data/main/$product.json"
    val response = HttpsClient.sendRequest(uriString)
    acc.updated(response("title").str,  Product(response("title").str, response("price").num))
  }

  val prepareCart = for {
    _ <- State.modify[ShoppingCart](_.addItems(productMapping("Corn Flakes"),2))
    _ <- State.modify[ShoppingCart](_.addItems(productMapping("Weetabix"), 1))
    cart <- State.get[ShoppingCart]
  } yield cart


  val (preparedCart, _ ) = prepareCart.run(ShoppingCart(Map.empty[Product, Int])).value

  override def run: IO[Unit] = for {
    _ <- IO(println("Items in Cart.."))
    _ <- preparedCart.showCart
    subtotal <- preparedCart.subtotal
    _ <- IO(println("Total Price: "+ subtotal))
    taxPayable <- preparedCart.taxPayable(subtotal)
    _ <- IO(println("Tax Payable: " + taxPayable))
    totalPayable <- preparedCart.totalPayable(subtotal, taxPayable)
    _ <- IO(println("Final Price: " + totalPayable))
  } yield ()
}
