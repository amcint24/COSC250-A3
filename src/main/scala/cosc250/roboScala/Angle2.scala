package cosc250.roboScala

object Angle2 {

  /** Object to contain some common angle operations in 2D space */



  /** Get the angle from one position to another (as vectors) */
  def angleTo(start:Vec2, end:Vec2):Double = {
    (start-end).theta + Math.PI
  }

  /** This is probably a long way from the best way to do this but I had issues with the signs of the
    * angles flipping and turrets tracking the wrong way to deal with it. I've invoked the relationship between
    * angles and sine/cosine which are sign agnostic to get some consistent turret tracking working.
    *
    * The function takes the start angle (generally the current turret angle) and the end angle (generally
    * the angle from the tank to a target) and provides a string, CW or ACW, for the actor to decided which
    * turret rotate command to send up.
    * */
  def getDirection(start:Double, end:Double):String = {

    println("getting direction")



    // Gets the sign of a number, for the purposes of direction finding zero could be either so it's set to negative
    def getSign(number: Double) = {
      if (number <= 0) {
        "neg"
      } else {
        "pos"
      }
    }

    // Finds the quadrant an angle is in based on the relationship between its sine and cosine values
    def getQuadrant(angle:Double):Int = {
      val sine = Math.sin(angle)
      val cosine = Math.cos(angle)
      (getSign(sine), getSign(cosine)) match {
        case ("neg","pos") => 1
        case ("neg","neg") => 2
        case ("pos","neg") => 3
        case ("pos","pos") => 4
      }
    }

    // Provides the response to move the angles to the same quadrant if they are not already
    def matchQuadrant (startQuadrant:Int, endQuadrant:Int):String = {
      val ACW = List((1,2), (2,3), (3,4), (4,1), (1,3), (2,4))
      val CW = List((1,4), (2,1), (3,2), (4,3), (3,1), (4,2))
      if (ACW.contains((startQuadrant,endQuadrant))) {
        "ACW"
      } else if (CW.contains((startQuadrant,endQuadrant))){
        "CW"
        // Edge case if something goes very wrong, the turret will just move CW which should clear the issue for next tick
      } else {
        "CW"
      }
    }

    // Provides the direction the start angle needs to move to track onto the end angle within the same quadrant
    def track (startAngle:Double, endAngle:Double, quadrant:Int):String = {
      val startSine = Math.sin(startAngle)
      val endSine = Math.sin(endAngle)
      if (quadrant == 4 || quadrant == 1) {
        if (startSine <= endSine) {"CW"} else {"ACW"}
      } else if (quadrant == 2 || quadrant == 3) {
        if (startSine >= endSine) {"CW"} else {"ACW"}
        // Edge case if something goes very wrong, the turret will just move CW which should clear the issue for next tick
      } else {
        "CW"
      }
    }

    // Determine if the two angles are in the same quadrant, if not provide the direction to track
    // start into end's quadrant
    if (!(getQuadrant(start) == getQuadrant(end))) {
      matchQuadrant(getQuadrant(start),getQuadrant(end))
      // If already in the same quadrant track onto target
    } else {
      track(start,end, getQuadrant(start))
    }
  }
}


