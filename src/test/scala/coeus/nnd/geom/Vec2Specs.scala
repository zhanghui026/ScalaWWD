package coeus.nnd.geom
import org.specs2.mutable._
/**
 * Created by IntelliJ IDEA.
 * User: coeuszhanghui
 * Date: 7/27/11
 * Time: 9:01 PM
 * To change this template use File | Settings | File Templates.
 */

class Vec2Specs extends Specification {
    "vec opp" should {
      "vec add" in {
        val vec = Vec2(21,3) + Vec2(24,3)
        vec must_==  Vec2(45.0,6.0)
      }
      "get vec average distance" in {
        val list = List(Vec2(21,3),Vec2(24,3),Vec2(12.5,3.4))
        Vec2.getAverageDistance(list:_*) must_==7.672120326253641
      }
      "compute average point" in {
        Vec2.computeAveragePoint(Vec2(21,3),Vec2(24,3),Vec2(12.5,3.4))  must_== Vec2(19.166666666666668,3.1333333333333333)
      }
      "mix" in {
        Vec2.mix(0.3,Vec2(21,3),Vec2(24,3)) must_== Vec2(21.9,2.9999999999999996)
      }
    }

}