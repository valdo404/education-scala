package io.univalence.education

import monocle.{Focus, Lens}
import monocle.macros.GenLens
import monocle.syntax.all._
import scala.util.chaining._
import io.univalence.education.internal.exercise_tools.*

/**
 * = Optics Tutorial =
 * 
 * Optics are a powerful way to manipulate immutable data structures in a composable way.
 * This tutorial will cover the basics of using Monocle, a popular optics library for Scala.
 * 
 * We'll cover:
 * 1. Lenses - For working with product types (case classes)
 * 2. Prisms - For working with sum types (sealed traits/enums)
 * 3. Optional - For working with optional values
 * 4. Traversal - For working with collections
 */
@main
def optics(): Unit = {
  section("PART 1 - Basic Lenses") {
    /**
     * Let's start with a simple example using nested case classes
     */
    case class Address(street: String, city: String, country: String)
    case class Person(name: String, age: Int, address: Address)

    exercise("Creating and using lenses") {
      // Create a sample person
      val john = Person("John", 30, Address("123 Main St", "New York", "USA"))

      // Create a lens for the age field using GenLens
      val ageLens: Lens[Person, Int] = GenLens[Person](_.age)

      // Create a lens for the name field using Focus
      val nameLens = Focus[Person](_.name)

      // Create a lens for the address field manually
      val addressLens = Lens[Person, Address](_.address)(a => p => p.copy(address = a))

      // Create a lens for the street field and compose it with addressLens
      val streetLens = GenLens[Address](_.street)
      val personStreetLens = addressLens.andThen(streetLens)

      // Test your lenses
      check(ageLens.get(john) == 30)
      check(nameLens.get(john) == "John")
      check(personStreetLens.get(john) == "123 Main St")

      // Use ageLens to increment John's age by 1
      val olderJohn = ageLens.modify(_ + 1)(john)
      check(olderJohn.age == 31)

      // Use the focus syntax to move John to a new address
      val movedJohn = john
        .focus(_.address.street).replace("456 Park Ave")
        .focus(_.address.city).replace("Boston")

      check(movedJohn.address.street == "456 Park Ave")
      check(movedJohn.address.city == "Boston")
    }
  }

  section("PART 2 - Prisms for Sum Types") {
    /**
     * Prisms are useful when working with sealed traits and enums
     */
    enum PaymentMethod:
      case CreditCard(number: String, expiry: String)
      case PayPal(email: String)
      case Cash

    import PaymentMethod.*

    exercise("Working with Prisms") {
      val payment1 = CreditCard("1234-5678", "12/25")
      val payment2 = PayPal("john@example.com")

      // Create a prism for the CreditCard case
      val creditCardPrism = monocle.Prism[PaymentMethod, (String, String)] {
        case CreditCard(num, exp) => Some((num, exp))
        case _ => None
      }((t: (String, String)) => CreditCard(t._1, t._2))

      // Create a prism for the PayPal case
      val paypalPrism = monocle.Prism[PaymentMethod, String] {
        case PayPal(email) => Some(email)
        case _ => None
      }(PayPal.apply)

      // Test your prisms
      check(creditCardPrism.getOption(payment1).isDefined)
      check(creditCardPrism.getOption(payment2).isEmpty)
      check(paypalPrism.getOption(payment2).contains("john@example.com"))

      // Create a new credit card by reversing the card number
      val reversedCard = creditCardPrism.modify { case (num, exp) => (num.reverse, exp) }(payment1)
      check(creditCardPrism.getOption(reversedCard).exists(_._1 == "8765-4321"))
    }
  }

  section("PART 3 - Optionals") {
    /**
     * Optionals combine the power of Lenses and Prisms
     */
    case class User(id: Int, name: String, email: Option[String])

    exercise("Working with Optionals") {
      val user1 = User(1, "John", Some("john@example.com"))
      val user2 = User(2, "Jane", None)

      // Create an Optional for the email field
      val emailOptional = Focus[User](_.email).some

      // Modify the email to uppercase if it exists
      val updatedUser1 = emailOptional.modify(_.toUpperCase)(user1)
      val updatedUser2 = emailOptional.modify(_.toUpperCase)(user2)

      check(updatedUser1.email.contains("JOHN@EXAMPLE.COM"))
      check(updatedUser2.email.isEmpty)

      // Create a function that safely gets the domain part of the email (after @)
      val domainOptional = emailOptional.andThen(monocle.Optional[String, String](s => Some(s.split('@')(1)))(domain => email => email.split('@')(0) + "@" + domain))
      check(domainOptional.getOption(user1).contains("example.com"))
      check(domainOptional.getOption(user2).isEmpty)
    }
  }

  section("PART 4 - Traversals") {
    /**
     * Traversals allow you to modify multiple values at once
     */
    case class Company(departments: List[Department])
    case class Department(name: String, employees: List[Employee])
    case class Employee(name: String, salary: Double)

    exercise("Working with Traversals") {
      val company = Company(List(
        Department("Engineering", List(
          Employee("John", 100000),
          Employee("Jane", 110000)
        )),
        Department("Sales", List(
          Employee("Bob", 90000),
          Employee("Alice", 95000)
        ))
      ))

      // Create a traversal for all employee salaries
      val allSalaries = Focus[Company](_.departments)
        .each
        .andThen(Focus[Department](_.employees))
        .each
        .andThen(Focus[Employee](_.salary))

      // Give everyone a 10% raise
      val updatedCompany = allSalaries.modify(_ * 1.1)(company)

      // Test the changes
      val originalSum = company.departments.flatMap(_.employees).map(_.salary).sum
      val updatedSum = updatedCompany.departments.flatMap(_.employees).map(_.salary).sum
      
      check(updatedSum > originalSum)
      check(math.abs(updatedSum - (originalSum * 1.1)) < 0.001)

      // Create a traversal that only affects employees in the Engineering department
      val engineeringSalaries = Focus[Company](_.departments)
        .each
        .filter(d => d.name == "Engineering")
        .andThen(Focus[Department](_.employees))
        .each
        .andThen(Focus[Employee](_.salary))
      val engineeringRaise = engineeringSalaries.modify(_ * 1.2)(company)
      check(engineeringRaise.departments.find(_.name == "Engineering").get.employees.forall(_.salary > 100000))
      check(engineeringRaise.departments.find(_.name == "Sales").get.employees.forall(_.salary < 100000))
    }
  }

  section("PART 5 - Real World Example") {
    /**
     * Let's combine everything we've learned in a real-world example
     */
    case class OrderItem(productId: String, quantity: Int, price: Double)
    case class Order(id: String, customer: Option[Customer], items: List[OrderItem], status: OrderStatus)
    case class Customer(id: String, name: String, address: Option[Address])
    case class Address(street: String, city: String, country: String)
    
    enum OrderStatus:
      case Pending, Processing, Shipped, Delivered, Cancelled

    exercise("Complex Order Management") {
      import OrderStatus.*
      
      val order = Order(
        "ord-123",
        Some(Customer("cust-1", "John Doe", Some(Address("123 Main St", "New York", "USA")))),
        List(
          OrderItem("prod-1", 2, 29.99),
          OrderItem("prod-2", 1, 49.99)
        ),
        Pending
      )

      // Create an optics composition to access the customer's city
      val customerAddress = Focus[Order](_.customer).some
        .andThen(Focus[Customer](_.address)).some
        .andThen(Focus[Address](_.city))

      // Create a traversal for all order item prices
      val orderItems = Focus[Order](_.items)
        .each
        .andThen(Focus[OrderItem](_.price))

      // Implement the following order updates:
      // 1. Change status to Processing
      // 2. Update city to "Boston"
      // 3. Apply 10% discount to all items
      val updatedOrder = order
        .focus(_.status).replace(Processing)
        .pipe(customerAddress.replace("Boston"))
        .pipe(orderItems.modify(_ * 0.9))

      // Verify changes
      check(updatedOrder.status == Processing)
      check(updatedOrder.customer.flatMap(_.address).map(_.city).contains("Boston"))
      
      val originalTotal = order.items.map(_.price).sum
      val discountedTotal = updatedOrder.items.map(_.price).sum
      check(math.abs(discountedTotal - (originalTotal * 0.9)) < 0.001)

      // BONUS: Create a function that safely calculates the total price for orders with status != Cancelled
      def safeTotal(order: Order): Option[Double] = 
        val statusPrism = monocle.Prism[OrderStatus, OrderStatus](s => if s != Cancelled then Some(s) else None)(identity)
        statusPrism
          .getOption(order.status)
          .map(_ => order.items.map(i => i.price * i.quantity).sum)
      check(safeTotal(order).contains(109.97))
      check(safeTotal(order.copy(status = Cancelled)).isEmpty)
    }
  }
}
