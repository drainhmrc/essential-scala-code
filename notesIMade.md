# ADT

- Collection of data types
- made up of ANDs and ORs

- In Scala:
    - ANDS are case classes (and case objects)
    - ORS are sealed traits (or sealed abstract classes)
 
- A Colour is:
    - a red value of type Double AND
    - a green value of type Double AND
    - a blue value is a Double

- A Shape is:
    - a Circle OR
    - a Rectangle
    
where: 
    - A Circle is:
        - a radius of type Double AND
        - a color of type Color
    - A Rect is:
        - a width of type Double AND
        - a height of type Double AND
        - a colour of type Color

```scala
    
    case class Color( // simple case class
      red: Double,
      green: Double,
      blue: Double,
      )

    sealed trait Shape { // making a sealed trait we stop new things from being added, we can use exhaustive matching
    def perimeter: Double
    }
    
    case class Circle(
      radius: Double,
      color: Color,
      ) extends Shape {
                            def perimeter: Double = ???
                      }
      
    case class Rectangle(
      width: Double,
      height: Double,
      color: Color,
      ) extends Shape {
                           def perimeter: Double = ???
                       }
      
     def area(shape: Shape): Double = 
      shape match {
        case Circle(r, c)       => ???
        case Rectangle(w, h, c) => ???
      }
      def diameter(shape: Shape): Double = 
      shape match {
        case Circle(r, c)       => ???
        case Rectangle(w, h, c) => ???
      }

```