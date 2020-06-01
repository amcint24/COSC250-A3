package cosc250.roboScala

import java.awt.Color

import scala.util.Random
import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

/** This tank attempts to move erratically around the game, searching for tanks as it goes and attempting to shoot them
  * prioritising erratic movement over accuracy and target seeking in an attempt to last the longest by not getting
  * hit but wearing down opposing tanks with lucky shots. Emphasis on lucky shots, this tank struggles to hit anything
  * that is employing its own hit avoidance tech.
  */
class MyVeryOwnTank extends Actor {
  import context.dispatcher
  implicit val timeout:Timeout = 1.seconds
  // Give our tank a unique name
  val name = s"Striker Eureka ${Random.alphanumeric.take(4).mkString}"
  val log = Logging(context.system, this)

  // List of all insults the tank has recieved
  var insults:Seq[String] = Seq()

  // As soon as the tank is created, register it
  Main.gameActor ! Register(name, Color.orange)

  /** Variables to track current behaviour */
  // Current target heading for patrolling
  var newHeading: Double = 0
  // Next left/right movement for patrolling
  var nextTurn:Int = 1
  // Counter for not turning every game tick for larger curves
  var turnCounter = 5
  // Last known coordinates of target (-1,-1) represents no target
  var currentTarget:Vec2 = Vec2(-1,-1)
  // Target time to live (for disengaging lost targets) in game ticks
  var tgtTTL:Int = 700
  // Flag for edge turning behaviour
  var edgeTurn:Boolean = false
  // Tuple to store turret tracking information (Tracking direction, delta angle)
  var lastTurretTick:(Int,Double) = (1,0)


  def receive:PartialFunction[Any, Unit] = {


    // Every game tick, we receive our tank state, and should send commands to
    // Main.gameActor to say what we want to do this tick
    case TankState(me) =>

      // Patrol the area conducting a radar search if no target is registered
      patrol(me)
      if (currentTarget == Vec2(-1,-1)) radarSearch(me)

      // Decrement the target if it is lost and reset after 700 ticks
      tgtTTL -= 1
      if (tgtTTL == 0) currentTarget = Vec2(-1,-1)

      // If there's an active target track it with the turret and fire if able
      if (!(currentTarget == Vec2(-1,-1))) {
        turretTrack(me)
        Main.gameActor ! RadarPing
      }


    // If we successfully Ping the radar, we'll get a message containing the
    // states of any tanks we see
    case RadarResult(me, seenTanks) =>

      // If a tank is identified register it as a target and reset the TTL
      if (seenTanks.nonEmpty) {
        currentTarget = seenTanks.head.position
        tgtTTL = 700
      }

    // If an insult is received log it and respond in kind
    case Insulted(insult) =>
      val s = sender()
      val blow = insult
      log.info("{} sent me the insult {}", s, blow)

      // Ask for the correct retort and send it
      for {
        responseFuture <- (Main.insultsActor ? WhatsTheRetortFor(blow)).mapTo[Retort]
      } {
        log.info("I responded to {} with {}",s,responseFuture)
        s ! responseFuture
      }

      // Keep a list of unique insults received
      if (!insults.contains(insult)) insults = insults :+ insult
  }
  /** Provides commands for patrolling behaviour
    * Patrolling concept is to serpentine across the game area, switching between left and right curved paths.
    * When an edge is approached turn harder to come around in a new direction. The intent is to make the tank
    * hard to hit with shells while looking for other tanks by moving unpredictably.
    */
  def patrol (me:Tank)= {

    /** Function to set a new heading on edge approach. Desired result is that when an edge is approached the tank
      * turns the 'shorter' direction to come around to a new angle.
      */
    def edgeApproach:Unit = {
      // Helper function to test if angle i is between angles a._1 a._2
      val between = (i:Double ,a:(Double, Double)) => {
        (i >= a._1) && (i < a._2)
      }
      // The angle the tank is moving when close to an edge can determine what edge it is close to and which is the
      // 'short' turn to make, this alternates every quarter pi radians. The function is only called if close to the
      // edge so if we don't need a right turn we need a left turn
      val rightTurns = List((0.0,Math.PI/4),(2*Math.PI/4, 3*Math.PI/4), (4*Math.PI/4,5*Math.PI/4),(6*Math.PI/4,7*Math.PI/4))
      val rightResult = rightTurns.map(turn => between(me.facing, turn))
      if (rightResult.contains(true)) {
        newHeading = (newHeading + Math.PI/2) % 2*Math.PI
      } else {
        newHeading = (newHeading - Math.PI/2) % 2*Math.PI
      }
    }

    /** Provides the left/right swapping of the curved path by tracking delta to the current desired heading, and
      * setting a new heading when it is reached
      */
    def turnToHeading = {

      val turnDelta = newHeading - me.facing
      // Check if current heading is close to next heading, if not continue turning
      if (Math.abs(turnDelta) > 0.4) {
        // Determine turn direction based on facing compare to next heading
        if (turnDelta > 0) {
          Main.gameActor ! TurnClockwise
        } else {
          Main.gameActor ! TurnAnticlockwise
        }
        // If current heading is close next heading change direction and set a new heading then swap the
        // direction for the next turn and clear the edge approach flag
      } else {
        newHeading = (newHeading + (Math.PI/2*nextTurn)) % 2*Math.PI
        nextTurn = nextTurn * (-1)
        edgeTurn = false
      }
    }

    // Flag to track if getting too close to an edge to allow normal turning behaviour
    val aheadInGame = GameState.inBounds(Vec2.fromRTheta(me.velocity*3, me.facing) + me.position)

    // If approaching an edge initiate edge turning behaviour. The desired effect is to maintain a normal
    // movement curve if possible, turn sharper if necessary to avoid the edge
    if (!edgeTurn && !aheadInGame) {
      edgeTurn = true
      edgeApproach
    } else if (edgeTurn && !aheadInGame) {
      turnToHeading
    }

    // Normal movement curve. Only turn every few game ticks to make a wider curve while patrolling.
    if (turnCounter == 0){
      turnCounter = 5
      turnToHeading
      Main.gameActor ! FullSpeedAhead
    } else {
      turnCounter -= 1
      Main.gameActor ! FullSpeedAhead
    }
  }

  /** Spins the turret (and thus radar) around in a circle to seek targets. Relies on the erratic movement of the
    * tank to get good coverage as it goes around. Doesn't spin if there's no energy for a radar pulse to avoid missing
    * spots.
    * */
  def radarSearch (me:Tank) = {
    // Spin the turret (and radar) and ping looking for tanks
    if (me.energy > 90) {
      Main.gameActor ! RadarPing
      Main.gameActor ! TurretClockwise
    }

  }

  /** Attempts to keep the turret tracked onto the current target. If the target is within a margin of error where
    * a hit is expected it will fire. Turret tracking is interfered with by the tank turning but the erratic
    * movement to avoid hits is prioritised over hit accuracy.
    */
  def turretTrack (me:Tank) = {
    val angleToTGT = Angle2.angleTo(me.position, currentTarget)
    val turnDirection = Angle2.getDirection(me.turretFacing, angleToTGT)
    println(turnDirection)
    if (turnDirection == "CW") {
      println("tracking cw")
      Main.gameActor ! TurretClockwise
    } else {
      println("tracking acw")
      Main.gameActor ! TurretAnticlockwise
    }

    val tgtDelta = me.turretFacing - angleToTGT
    if (Math.abs(tgtDelta) <0.2 && me.energy > 10) Main.gameActor ! Fire
  }
}
