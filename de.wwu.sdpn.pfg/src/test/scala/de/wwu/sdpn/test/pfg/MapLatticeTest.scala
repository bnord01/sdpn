package de.wwu.sdpn.test.pfg
import org.junit._
import org.junit.Assert._

import de.wwu.sdpn.pfg.lattices._

class MapLatticeTest {

    @Test
    def testBottomLTEQTop {
        val lat = implicitly[Lattice[LMap[Int, Boolean]]]
        println("Bottom: " + lat.bottom)
        println("Top: " + lat.top)
        assertTrue("bottom <= top", lat.bottom <= lat.top)
    }

    @Test
    def testBottomLTTop {
        val lat = implicitly[Lattice[LMap[Int, Boolean]]]
        assertTrue("bottom < top", lat.bottom < lat.top)
    }
    @Test
    def testBottomLTEQ1 {
        val x = BottomMap(Map(1 -> true))
        val lat = implicitly[Lattice[LMap[Int, Boolean]]]
        assertTrue("bottom <= 1 -> true", lat.bottom <= x)
        assertTrue("bottom < 1 -> true", lat.bottom < x)
    }
    @Test
    def testLTEQ1 {
        val x:LMap[Int,Boolean] = BottomMap(Map(1 -> true))
        val y = BottomMap(Map(1 -> true,2 -> true))
        assertTrue("bottom <= 1 -> true", x <= y)
        assertTrue("bottom < 1 -> true", x < y)
    }
    
     @Test
    def testJoin1 {
        val x:LMap[Int,Boolean] = BottomMap(Map(1 -> true))
        val y:LMap[Int,Boolean] = BottomMap(Map(1 -> true,2 -> true))
        val jn = x ⊔ y
        assert(x ⊔ y equiv y)        
        assertEquals(true,(x ⊔ y)(1))
        assertEquals(true,(x ⊔ y)(2))
        assertEquals(false,(x ⊔ y)(3))
    }

}