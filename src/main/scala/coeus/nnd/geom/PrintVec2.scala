package coeus.nnd.geom

/**
 * Created by IntelliJ IDEA.
 * User: coeuszhanghui
 * Date: 7/24/11
 * Time: 10:55 PM
 * To change this template use File | Settings | File Templates.
 */

object PrintVec2 extends App{
  println(Vec2.ZERO)
  println(Vec2(0,0).hashCode() == Vec2.ZERO.hashCode())

  val v1 = Vec2(21,3)
  val v2 = Vec2(24,3)
  val v3 = Vec2(12.5,3.4)
  val list = List(v1,v2,v3)

  println(v1 + v2) //45.0,6.0
  println(Vec2.getAverageDistance(list:_*)) //7.67
  println(Vec2.computeAveragePoint(v1,v2,v3)) //19.16 3.13
  println(Vec2.mix(0.3,v1,v2)) //21.9 2.999
}