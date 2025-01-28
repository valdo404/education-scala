package io.univalence.education

import monocle.{Focus, Lens}
import monocle.macros.GenLens
import monocle.syntax.all._

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
object optics:
  section("PART 1 - Basic Lenses") {
    /**
     * Let's start with a simple example using nested case classes
     */
    case class Address(street: String, city: String, country: String)
    case class Person(name: String, age: Int, address: Address)

    exercise("Creating and using lenses") {
      // Create a sample person
      val john = Person("John", 30, Address("123 Main St", "New York", "USA"))

      // Create lenses using different approaches
      val ageLens = GenLens[Person](_.age)
      val nameLens = Focus[Person](_.name)
      val addressLens = Lens[Person, Address](_.address)(a => p => p.copy(address = a))
      val streetLens = GenLens[Address](_.street)

      // Compose lenses to access nested fields
      val personStreetLens = addressLens.andThen(streetLens)

      // Use lenses to get values
      check(ageLens.get(john) == 30)
      check(nameLens.get(john) == "John")
      check(personStreetLens.get(john) == "123 Main St")

      // Use lenses to modify values
      val olderJohn = ageLens.modify(_ + 1)(john)
      check(olderJohn.age == 31)

      // Chain modifications using function composition
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

    import PaymentMethod._

    exercise("Working with Prisms") {
      val payment1 = CreditCard("1234-5678", "12/25")
      val payment2 = PayPal("john@example.com")

      // Create prisms for each case
      val creditCardPrism = monocle.Prism[PaymentMethod, (String, String)] {
        case CreditCard(num, exp) => Some((num, exp))
        case _ => None
      }((t: (String, String)) => CreditCard(t._1, t._2))

      val paypalPrism = monocle.Prism[PaymentMethod, String] {
        case PayPal(email) => Some(email)
        case _ => None
      }(PayPal.apply)

      // Use prisms to extract data
      check(creditCardPrism.getOption(payment1).isDefined)
      check(creditCardPrism.getOption(payment2).isEmpty)
      check(paypalPrism.getOption(payment2).contains("john@example.com"))
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

      // Modify the email if it exists
      val updatedUser1 = emailOptional.modify(_.toUpperCase)(user1)
      val updatedUser2 = emailOptional.modify(_.toUpperCase)(user2)

      check(updatedUser1.email.contains("JOHN@EXAMPLE.COM"))
      check(updatedUser2.email.isEmpty)
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
        .andThen(monocle.Focus.traversal[List[Department]])
        .andThen(Focus[Department](_.employees))
        .andThen(monocle.Focus.traversal[List[Employee]])
        .andThen(Focus[Employee](_.salary))

      // Give everyone a 10% raise
      val updatedCompany = allSalaries.modify(_ * 1.1)(company)

      // Check that all salaries were increased
      val originalSum = company.departments.flatMap(_.employees).map(_.salary).sum
      val updatedSum = updatedCompany.departments.flatMap(_.employees).map(_.salary).sum
      
      check(updatedSum > originalSum)
      check(math.abs(updatedSum - (originalSum * 1.1)) < 0.001)
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
      import OrderStatus._
      
      val order = Order(
        "ord-123",
        Some(Customer("cust-1", "John Doe", Some(Address("123 Main St", "New York", "USA")))),
        List(
          OrderItem("prod-1", 2, 29.99),
          OrderItem("prod-2", 1, 49.99)
        ),
        Pending
      )

      // Create various optics
      val customerAddress = Focus[Order](_.customer).some
        .andThen(Focus[Customer](_.address)).some
        .andThen(Focus[Address](_.city))

      val orderItems = Focus[Order](_.items)
        .andThen(monocle.Focus.traversal[List[OrderItem]])
        .andThen(Focus[OrderItem](_.price))

      // Modify the order
      val updatedOrder = order
        .focus(_.status).replace(Processing)
        .pipe(customerAddress.replace("Boston"))
        .pipe(orderItems.modify(_ * 0.9)) // Apply 10% discount

      // Verify changes
      check(updatedOrder.status == Processing)
      check(updatedOrder.customer.flatMap(_.address).map(_.city).contains("Boston"))
      
      val originalTotal = order.items.map(_.price).sum
      val discountedTotal = updatedOrder.items.map(_.price).sum
      check(math.abs(discountedTotal - (originalTotal * 0.9)) < 0.001)
    }
  }
