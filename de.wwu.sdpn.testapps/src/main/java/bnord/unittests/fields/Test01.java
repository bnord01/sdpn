package bnord.unittests.fields;

public class Test01 {
	static boolean nd = false;
	public static void main(String[] args) {
		BaseClass x = nd ? new BaseClass() : nd ? new SubClass() : nd ? new SubSubClass() : new SubClass2() ;
		x.tut();
	}

}

class BaseClass {
	public BaseClass field;
	void tut () {
		this.field.hashCode();
		this.field = this;
	}
}
class SubClass extends BaseClass {
	public SubClass field;
	void tut () {
		this.field.hashCode();
		this.field = this;
	}
}

class SubSubClass extends SubClass {
	void tut () {
		this.field.hashCode();
		this.field = this;
	}
}

class SubClass2 extends BaseClass {
	void tut () {
		this.field.hashCode();
		this.field = this;
	}
}