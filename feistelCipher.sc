def s0(n1: String): String= n1 match{
  case "0" => "1"
  case "1" => "9"
  case "2" => "2"
  case "3" => "9"
  case "4" => "a"
  case "5" => "7"
  case "6" => "f"
  case "7" => "e"
  case "8" => "6"
  case "9" => "b"
  case "a" => "c"
  case "b" => "8"
  case "c" => "4"
  case "d" => "3"
  case "e" => "2"
  case "f" => "0"
}

def s1(n2: String): String= n2 match{
  case "0" => "6"
  case "1" => "b"
  case "2" => "b"
  case "3" => "a"
  case "4" => "a"
  case "5" => "c"
  case "6" => "c"
  case "7" => "d"
  case "8" => "d"
  case "9" => "1"
  case "a" => "1"
  case "b" => "2"
  case "c" => "2"
  case "d" => "4"
  case "e" => "5"
  case "f" => "7"
}

def binhex3(s : String): Int= s match{
  case "000" => 0
  case "001" => 1
  case "010" => 2
  case "011" => 3
  case "100" => 4
  case "101" => 5
  case "110" => 6
  case "111" => 7
}

def binhex4(s : String): Char= s match{
  case "0000" => '0'
  case "0001" => '1'
  case "0010" => '2'
  case "0011" => '3'
  case "0100" => '4'
  case "0101" => '5'
  case "0110" => '6'
  case "0111" => '7'
  case "1000" => '8'
  case "1001" => '9'
  case "1010" => 'a'
  case "1011" => 'b'
  case "1100" => 'c'
  case "1101" => 'd'
  case "1110" => 'e'
  case "1111" => 'f'
}

def hexbin4(s : Char): String= s match{
  case '0' => "0000"
  case '1' => "0001"
  case '2' => "0010"
  case '3' => "0011"
  case '4' => "0100"
  case '5' => "0101"
  case '6' => "0110"
  case '7' => "0111"
  case '8' => "1000"
  case '9' => "1001"
  case 'a' => "1010"
  case 'b' => "1011"
  case 'c' => "1100"
  case 'd' => "1101"
  case 'e' => "1110"
  case 'f' => "1111"
}


def zbyte(z : Array[String]): String ={
  var res : String = ""
  var i = 0
  while(i < z.length){
    val c = z(i).toCharArray
    res += hexbin4(c(0)).substring(0,1)
    i += 1
  }
  res
}

def k(zstar : Int): Int = zstar match{
  case 0 => 5
  case 1 => 7
  case 2 => 3
  case 3 => 4
  case 4 => 4
  case 5 => 1
  case 6 => 3
  case 7 => 4
}

def k1(zstar : Int): Int = zstar match{
  case 0 => 4
  case 1 => 1
  case 2 => 2
  case 3 => 2
  case 4 => 3
  case 5 => 4
  case 6 => 5
  case 7 => 6
}

def xor(arg1 : String, arg2 : String): String ={
  val l1 = arg1.toArray
  val l2 = arg2.toArray
  var res : String = ""
  var i : Int = 0
  while(i < l1.length){
    if((l1(i) == '0' && l2(i) == '1') || ((l1(i) == '1' && l2(i) == '0'))) res += '1'
    else res += '0'
    i += 1
  }
  res
}

def f(m1 : String): String= {
  val z = Array(m1.substring(0, 2), m1.substring(2, 4), m1.substring(4))
  println("z: " + z(0) + " " + z(1) + " " + z(2))
  var i = 0
  val p1array = Array.ofDim[String](3)
  val p2array = Array.ofDim[String](3)
  while (i < z.length) {
    val n1 = z(i).substring(0, 1)
    println("n1" + (i+1) + ": " + n1)
    val p1 = s0(n1)
    p1array(i) = p1
    // println("p1" + (i+1) + ": " + p1)
    val n2 = z(i).substring(1)
    println("n2" + (i+1) + ": " + n2)
    val p2 = s1(n2)
    p2array(i) = p2
    // println("p2" + (i+1) + ": " + p2)
    i += 1
  }

  print("p1array: ")
  p1array.foreach(print(_))
  print("\n")

  print("p2array: ")
  p2array.foreach(print(_))
  print("\n")

  val zstar: Int = binhex3(zbyte(z))
  println("z* : " + zstar)
  var t = k1(zstar).toBinaryString
  while(t.length < 3) t = "0" + t
  t.toList
  println("t: " + t(0) + t(1) + t(2))

  var j = 0
  while (j < t.length) {
    if (t(j) == '1') {
      val p1 = p1array(j)
      val p2 = p2array(j)
      p1array(j) = p2
      p2array(j) = p1
    }
    j += 1
  }

  print("p1array: ")
  p1array.foreach(print(_))
  print("\n")

  print("p2array: ")
  p2array.foreach(print(_))
  print("\n")

  var temp = p1array(0)
  p1array.update(0, p1array(1))
  p1array.update(1, temp)
  temp = p2array(0)
  p2array.update(0, p2array(2))
  p2array.update(2, temp)
  temp = p2array(1)
  p2array.update(1, p1array(2))
  p1array.update(2, temp)

  print("p1array: ")
  p1array.foreach(print(_))
  print("\n")

  print("p2array: ")
  p2array.foreach(print(_))
  print("\n")

  val res : String = p1array(0) + p2array(0) + p1array(1) + p2array(1) + p1array(2) + p2array(2)
  res
}


val m = "000000000000"
val mid = m.length / 2
val m0 = m.substring(0, mid)
val m1 = m.substring(mid)
val m0bin = m0.map(hexbin4(_)).mkString("")
val m1bin = m1.map(hexbin4(_)).mkString("")
println("-----ROUND1-----")
val f1 = f(m1).map(hexbin4(_)).mkString("")
val m2bin = xor(m0bin,f1)
val m2binlist = m2bin.grouped(4).toList
val m2 = m2binlist.map(binhex4(_)).mkString("")
println("-----ROUND2-----")
val f2 = f(m2).map(hexbin4(_)).mkString("")
val m3bin = xor(m1bin,f2).grouped(4).toList
m3bin.foreach(print(_))
val m3 = m3bin.map(binhex4(_)).mkString("")
println("-----ROUND3-----")
val f3 = f(m3).map(hexbin4(_)).mkString("")
val m4bin = xor(m2bin,f3).grouped(4).toList
val m4 = m4bin.map(binhex4(_)).mkString("")
val c : String = m3 + m4


val c1 = "222222444444"
val m33 = "222222"
val m44 = "444444"
val m22 = xor(m44, f(m33))
val m11 = xor(m33, f(m22))
val m00 = xor(m22, f(m11))


