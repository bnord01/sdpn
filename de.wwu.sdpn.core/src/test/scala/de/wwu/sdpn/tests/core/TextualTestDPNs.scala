package de.wwu.sdpn.tests.core

object TextualTestDPNs {

    def dpn1 = """dpn{
	initial(1,1) 

	rules {
		(1,1) --1--> (1,2)
  		(1,2) --2--> (1,3,10)
  		(1,3) --3--> (1,4:>1,10)
  		(1,4) --4--> (1)
  	}
}
	    """

    def dpn2 = """dpn{
	initial(1,1) 

	rules {
		(1,1) --1--> (1,2)
  		(1,2) --2--> (1,3)
  		(1,3) --3--> (1,4)
  		(1,4) --4--> (1,4)
  	}
}
	    """
    def dpn3 = """dpn{
	initial(1,a1) 

	rules {
		(1,a1) --1--> (1,a2)
  		(1,a2) --2--> (1,b1,a3)
  		(1,a3) --3--> (1,a4)
  		(1,a4) --4--> (1,a4)
        
        (1,b1) --b--> (1,b2)
        (1,b2) --r--> (1)
        
        (1,c1) --n--> (1,d1,c2)
        (1,c2) --r--> (1,c3)
        (1,c3) --r--> (1,c3)
        
        (1,d1) --n--> (1,d1,d2)
        (1,d2) --r--> (1,d3)
        (1,d3) --r--> (1)
  	}
}
	    """

    def dpn4 = """dpn{
	initial(1,a1) 

	rules {
		(1,a1) --1--> (1,a2)
  		(1,a2) --2--> (1,b1,a3)
  		(1,a3) --3--> (1,a4)
  		(1,a4) --4--> (1,a4)
        
        (1,b1) --b--> (1,b2)
        (1,b2) --spwn-->(1,b3:>1,c1)
        (1,b3) --r--> (1)
        
        (1,c1) --n--> (1,d1,c2)
        (1,c2) --r--> (1,c3)
        (1,c3) --r--> (1,c3)
        
        (1,d1) --n--> (1,d1,d2)
        (1,d2) --r--> (1,d3)
        (1,d3) --r--> (1)
  	}
}
	    """
    def dpn5 = """dpn{
	initial(1,a1) 

	rules {
		(1,a1) --1--> (1,a2)
  		(1,a2) --2--> (1,b1,a3)
  		(1,a3) --3--> (1,a4)
  		(1,a4) --4--> (1,a4)
        
        (1,b1) --b--> (1,b2)
        (1,b2) --spwn-->(1,b3:>1,c1)
        (1,b3) --r--> (1)
        
        (1,c1) --n--> (1,d1,c2)
        (1,c2) --r--> (1,c3)
        (1,c3) --r--> (1,c3)
        
        (1,d1) --n--> (1,d2)
        (1,d2) --r--> (1,d3)
        (1,d3) --r--> (1)
  	}
}
	    """

    def dpn6 = """dpn{
	initial(1,a1) 

	rules {
		(1,a1) --callspwn--> (1,b1,a2)
  		(1,a2) --2--> (1,a3)
  		(1,a3) --3--> (1,a4)
  		(1,a4) --4--> (1,a4)
            
        (1,b1) --spwn--> (1,b2:>1,c1)
        (1,b2) --r--> (1)
        
        (1,c1) --n--> (1,c2)
        (1,c2) --r--> (1,c2)
  	}
}
	    """

    def dpn7 = """dpn{
	initial(a1) 

	rules {
		(a1) --callspwn-l--> (b1,a2)
  		(a2) --2--> (a3)
  		(a3) --3--> (a4)
  		(a4) --4--> (a4)
            
        (b1) --spwn--> (b2:>c1)
        (b2) --r--> ()
        
        (c1) --n-l--> (d1,c2)
        (c2) --r--> (c2)
        
        (d1) --a--> (d2)
        (d2) --r--> ()
  	}
}
	    """
        
    def dpn8 = """dpn{
	initial(a0) 

	rules {
        (a0) --0--> (a1)
		(a1) --1-l--> (b1,a2)
  		(a2) --2--> (a3)
  		(a3) --4--> (a3)
        
        (b1) --4--> (b2)
        (b2) --r--> ()
  	}
}
	    """
        
     def dpn9 = """dpn{
	initial(a0) 

	rules {
        (a0) --spawn--> (a1:>c1)
		(a1) --use_l_b-l--> (b1,a2)
  		(a2) --after_use_b_l--> (a3)
  		(a3) --loop_a--> (a3)
        
        (b1) --base_b--> (b2)
        (b2) --retb--> ()
         
        (c1) --use_l_d-l-->(d1,c2)
        (c2) --loop_c--> (c2)
         
        (d1) --base_d--> (d2)
        (d2) --retd--> ()
  	}
}
	    """
        
    def dpn = Map(
        1 -> dpn1,
        2 -> dpn2,
        3 -> dpn3,
        4 -> dpn4,
        5 -> dpn5,
        6 -> dpn6,
        7 -> dpn7,
        8 -> dpn8,
        9 -> dpn9)
}