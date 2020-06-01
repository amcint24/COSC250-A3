package cosc250.roboScala

import java.awt.Color

import akka.actor.Actor

import scala.util.Random

/** Wall Patroler moves around the outside of the map, keeping its turret facing inwards and firing if it
  * sees another tank.
  */
class WallPatroler extends Actor {

  // Give our tank a unique name
  val name = s"Wall Patroler ${Random.alphanumeric.take(4).mkString}"

  // As soon as the tank is created, register it
  Main.gameActor ! Register(name, Color.orange)

  // Tuple to store turret tracking information (Tracking direction, delta angle)
  var lastTurretTick:(Int,Double) = (1,0)

  def receive:PartialFunction[Any, Unit] = {

    // Every game tick, we receive our tank state, and should send commands to
    // Main.gameActor to say what we want to do this tick
    case TankState(me) =>
      // Calculate position 'in front' of the tank using heading and velocity
      val forwardPos = Vec2.fromRTheta(me.velocity,me.facing) + me.position
      // Turn right if approaching the boundary. This produces a long curved turn.
      val turning = !GameState.inBounds(forwardPos)
      if (turning) {
        Main.gameActor ! TurnClockwise
      } else {
        Main.gameActor ! FullSpeedAhead
      }


      // Calculate the angle to to middle of the field from the tanks current position and spin turret accordingly
      val angleToCentre = Angle2.angleTo(me.position, GameState.middle)
      val direction = Angle2.getDirection(me.turretFacing,angleToCentre)
      if (direction == "CW") Main.gameActor ! TurretClockwise
      if (direction == "ACW") Main.gameActor ! TurretAnticlockwise
      // println("turret" + me.turretFacing + " to center " + angleToCentre)
      /*
      // Register the middle as the turret target on tick 1
      if (lastTurretTick._2 == 0) lastTurretTick = (lastTurretTick._1, turretDeltaAngle)

      // The idea here is to keep track of wh
      if (lastTurretTick._1 == 1) Main.gameActor ! TurretClockwise
      if (lastTurretTick._1 == 2) Main.gameActor ! TurretAnticlockwise
      //if (turretDeltaAngle > 3){
      println("turret angle: " + me.turretAngle +
              " angle to center: " +angleToCentre +
              " delta angle: " + turretDeltaAngle +
              " tank face: " + me.facing
      )
      */
      // Ping radar if energy is full
      if (me.fullEnergy) Main.gameActor ! RadarPing

    // If we successfully Ping the radar, we'll get a message containing the
    // states of any tanks we see
    case RadarResult(me, seenTanks) =>
      if (seenTanks.nonEmpty) {
        sender() ! Fire
      }

  }

}
