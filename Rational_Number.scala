object Rational_Number {
  def main(args: Array[String]): Unit = {
    val x=new Rational(3,4);
    val y=new Rational(5,8);
    val z=new Rational(2,7);

    val r=x-y-z;
    println("the value of x-y-z is: "+r);
    val a=x.neg;
    println("value of negative of x: "+a);
  }


}

class Rational(n:Int, d:Int){
  require(d>0,"d must be greater that zero");
  def numer = n / gcd(Math.abs(n), d);
  def denom = d / gcd(Math.abs(n), d);
  def this(n: Int) = this(n, 1);
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b);
  def +(r:Rational) = new Rational(this.numer *
    r.denom +r.numer * this.denom, denom * r.denom);

  def neg = new Rational(-this.numer,this.denom);
  def -(r:Rational) = this+r.neg;

  override def toString=numer+"/"+denom;

}
