
import com.example.{Product, ShoppingCart}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.unsafe.IORuntime

class ShoppingCartTest extends AnyFlatSpec with Matchers  {

  implicit val ioRuntime: IORuntime = cats.effect.unsafe.IORuntime.global

  "Shopping Cart" should "add items as expected" in {
    val product1 = Product("product1", 1)
    val product2 = Product("product2",2)

    val shoppingCart = ShoppingCart(Map.empty)
    val update1 = shoppingCart.addItems(product1,1)
    val update2 = update1.addItems(product2, 2)
    val update3 = update2.addItems(product1,100)

    update3 should be (ShoppingCart(Map(product1 -> 101, product2 ->2)))
  }

  it should "calculate total price(without tax) correctly" in {
    val shoppingCart = ShoppingCart(
                          Map(Product("product1", 10.00) ->  2,
                              Product("product2", 20.00) ->  5)
                        )
    shoppingCart.subtotal.unsafeRunSync() should be (120.00)
  }

  it should "calculate tax payable correctly" in {
    val shoppingCart = ShoppingCart(Map.empty)
    val total = BigDecimal(1000.00)

    shoppingCart.taxPayable(total).unsafeRunSync() should be (BigDecimal(125.00))
  }

  it should "calculate final price correctly" in {
    val shoppingCart = ShoppingCart(Map.empty)
    val total = BigDecimal(100.00)
    val taxPayable = BigDecimal(12.50)

    shoppingCart.totalPayable(total, taxPayable).unsafeRunSync() should be (BigDecimal(112.50))
  }

}
