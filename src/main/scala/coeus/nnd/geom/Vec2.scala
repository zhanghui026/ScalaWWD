package coeus.nnd.geom

import scala.None

/**
 * Created by IntelliJ IDEA.
 * User: coeuszhanghui
 * Date: 7/24/11
 * Time: 9:33 PM
 * To change this template use File | Settings | File Templates.
 */

/**
 * 二维向量
 */
case class Vec2(val x: Double, val y: Double) {
  def fromLine(orign:Vec2,t:Double,direction:Vec2) = Vec2(orign.x+(direction.x*t),orign.y+(direction.y*t))
  def +(vec2:Vec2) = Vec2(this.x+vec2.x,this.y+vec2.y)
  def +(x:Double,y:Double) = Vec2(this.x+x,this.y+y)
  def -(vec2:Vec2)=Vec2(this.x-vec2.x,this.y-vec2.y)
  def -(x:Double,y:Double):Vec2=this.-(Vec2(x,y))
  def *(value:Double) = Vec2(this.x*value,this.y*value)
  def *(vec2:Vec2)=Vec2(this.x*vec2.x,this.y*vec2.y)
  def /(value:Double)={
    require(value!=0,"value值不为空")
    Vec2(this.x/value,this.y/value)
  }
  lazy val unary_- = Vec2(-this.x,-this.y)
  lazy val abs = Vec2(this.x.abs,this.y.abs)
  lazy val lengthSquare = this.x*this.x+this.y*this.y
  lazy val length=scala.math.sqrt(lengthSquare)
  lazy val normalize={
    val length = this.length
    if (length == 0) this else Vec2(this.x/length,this.y/length)
  }
  def distanceTo(vec2:Vec2) = scala.math.sqrt(this.distanceToSquare(vec2))
  def distanceToSquare(vec2:Vec2) = {
   var tmp = 0.0
    var result = 0.0
    tmp=this.x-vec2.x
    result+=tmp*tmp
    tmp = this.y-vec2.y
    result+=tmp*tmp
    result
  }

  def dot(vec2:Vec2) = this.x*vec2.x+this.y*vec2.y

  def dotSelf() = this.dot(this)
  def cross(vec2:Vec2) = this.x*vec2.y-this.y*vec2.x
  def projectOnto(vec2:Vec2) = {
    var dot = this dot vec2
    val length = vec2.length
    if (!(length == 0) && (length != 1.0))
      dot /= (length*length)

    vec2 * dot
  }

  def perpendicularTo(vec2:Vec2) = this - (projectOnto(vec2))



}
object Vec2 {
    lazy val INFINITY = Vec2(Double.PositiveInfinity, Double.PositiveInfinity)
  lazy val ZERO = Vec2(0, 0)
 lazy  val ONE = Vec2(1,1)
 lazy  val UNIT_X = Vec2(1,0)
 lazy  val UNIT_NEGATIVE_X = Vec2(-1,0)
 lazy  val UNIT_Y=Vec2(0,1)
 lazy  val UNIT_NEGATIVE_Y=Vec2(0,-1)

 def min(value1:Vec2,value2:Vec2) = {
   Vec2(scala.math.min(value1.x,value2.x),scala.math.min(value1.y,value2.y))
 }

  def max(value1:Vec2,value2:Vec2) = {
    Vec2(scala.math.max(value1.x,value2.x),scala.math.max(value1.y,value2.y))
  }

  def clamp(vec2:Vec2,min:Double,max:Double) = {
     val vecx=if (vec2.x<min) min else if (vec2.x>max) max else vec2.x
     val vecy=if (vec2.y<min) min else if (vec2.y>max) max else vec2.y
    Vec2(vecx,vecy)
  }

  def mix(amount:Double,value1:Vec2,value2:Vec2) = {
    if (amount<0.0) value1
    else if (amount>1.0) value2

    val t1= 1.0-amount
    Vec2((value1.x*t1+value2.x*amount),(value2.y*t1+value2.y*amount))

  }

  def computeAveragePoint[T <: Vec2](vec2s:T*)={
    if (vec2s.size == 0) None
    val all = (Vec2.ZERO /: vec2s){(sum:Vec2,v:Vec2) => sum + v}
    Vec2(all.x/vec2s.size,all.y/vec2s.size)

  }


  /**
   *  see parameter repeater
   */
  def getAverageDistance[T <: Vec2](vec2s:T*) ={
    var totalDistance = 0.0d
    var count:Int = 0
    for (p1 <- vec2s;p2 <- vec2s; if(p1!=p2)){
        totalDistance += p1.distanceTo(p2)
        count+=1
      }
    if (count==0.0) 0.0 else totalDistance/count
  }

}
