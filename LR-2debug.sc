type Set = Int => Boolean


def contains(S:Set, elem:Int):Boolean = S(elem)


def singletonSet (elem: Int): Set = x=> x==elem



def union ( s : Set , t : Set ) : Set = (x:Int) => s(x)||t(x)


def intersect ( s : Set , t : Set ) : Set = (x:Int) => s(x)&&t(x)


def diff( s : Set , t : Set ) : Set = (x:Int) => s(x)&&(!t(x))


def filter ( s : Set , p : Int => Boolean ) : Set = intersect(s,p)



def forall ( s : Set , p : Int => Boolean ) : Boolean = {


  def iter ( a : Int ) : Boolean = {

    if (a>1000) true //condition of checking all values and got to THE END of set

    else if ( s(a) && !p(a) ) false //founded failing element

    else iter (a+1)

  }



  iter (-1000)

}



var setOfOne = singletonSet(1)


var universum = union(x=>1==1, x=>0==0)



def negatePredicate [A] (p: A => Boolean ): A => Boolean = ( x : A ) => !p(x)




def exist(s:Set, p: Int => Boolean): Boolean = ! forall( p, x => contains(diff(universum,s),x));




def map( s : Set , f : Int => Int ) : Set = {


  var proceedingSet = diff(universum,universum)

  //starting point is empty set


  def iter ( a : Int , pS:Set ) : Set = {

    if (a>1000) pS

    else if  (s(a))

      iter( a+1, union(pS, singletonSet(f(a)) ) )

    else iter (a+1, pS)

  }

  iter (-1000, proceedingSet)

}



var set1 = singletonSet(1)

set1(1)

set1(2)

set1(-20)


var set2 = singletonSet(2)

set2(1)

set2(2)

set2(-20)



var set12 = union(set1, set2)


set12(1)

set12(2)

set12(-20)


var set123 = union(set12, singletonSet(3))


set123(1)

set123(2)

set123(3)

set123(-20)


var set13 = diff(set123, set2)


set13(1)

set13(2)

set13(3)

set13(-20)


var setAgain1 = intersect(set123, set1)


setAgain1(1)

setAgain1(2)

setAgain1(3)

setAgain1(-20)


var set1234 = union(set123, singletonSet(4))


var set24 = filter(set1234, x=> x%2==0)


set24(1)

set24(2)

set24(3)

set24(4)

set24(-21)


forall(set123, x=>x>0)


forall(set1234, x=> x%2 == 0)


exist(set123, x=> x<0 )


exist(set123, x => (x%3 == 0) )

exist(set123, x => x == 3 )

exist(set123, x => (x%4 == 0) )

exist(set123, x => x == -20 )




var squaresOf12 = map(set12, x=> x*x)


squaresOf12(1)

squaresOf12(2)

squaresOf12(3)

squaresOf12(4)

squaresOf12(-20)



var not1= diff(universum, set1)


not1(1)

not1(2)

not1(3)

not1(-20)


var notPredicateWithNot1 = diff ( universum, set123)


notPredicateWithNot1(-1)

notPredicateWithNot1(1)

notPredicateWithNot1(3)

notPredicateWithNot1(2)


forall(notPredicateWithNot1, x => x>=0)


exist(notPredicateWithNot1, x => x==0)