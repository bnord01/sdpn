package de.wwu.sdpn.tests.core

object TextualTestDPNs {
    val dpn1 = """dpn{
	initial(1,1) 

	rules {
		(1,1) --1--> (1,2)
  		(1,2) --2--> (1,3,10)
  		(1,3) --3--> (1,4:>1,10)
  		(1,4) --4--> (1)
  	}
}
	    """

    val dpn2 = """dpn{
	initial(1,1) 

	rules {
		(1,1) --1--> (1,2)
  		(1,2) --2--> (1,3)
  		(1,3) --3--> (1,4)
  		(1,4) --4--> (1,4)
  	}
}
	    """
}