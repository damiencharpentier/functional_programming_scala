package chapter_3

import scala.Int;
import scala.annotation.tailrec

/**
  * Created by damiencharpentier on 17-11-20.
  */


sealed trait Tree[+A]
case object NilTree extends Tree[Nothing]
case class Leaf[A](value : A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object MyTree {

  /**
    * exercise 3.25 function size that counts number of nodes
    */
  def count[A](tree: Tree[A]): Int = {
    tree match {
        case tree:Leaf[A] => 1
        case (branch: Branch[A]) => 1 + count(branch.left) + count(branch.right)
    }
  }


  def mainCount(args : Array[String]): Unit ={
    val myLeaf1 = new Leaf[Int](5);
    val myleaf2 = new Leaf[Int](7);

    val myLeaf3 = new Leaf[Int](12);
    val myleaf4 = new Leaf[Int](14);

    val myBranch1 = new Branch[Int](myLeaf1,myleaf2);
    val myBranch2 = new Branch[Int](myLeaf3,myleaf4);

    val myRoot = new Branch[Int](myBranch1,myBranch2);

    println(count(myRoot));
  }

  /**
    * exercise 3.26 function maximum that returns the maximum element
    */
  def maximum[A](tree: Tree[A])(f: (A,A) => A): A ={

    tree match {
        case l: Leaf[A] => l.value
        case (branch: Branch[A]) => f(maximum(branch.left)(f), maximum(branch.right)(f))
    }
  }

  def mainMaximum(args : Array[String]): Unit ={
    val myLeaf1 = new Leaf[scala.Int](5);
    val myleaf2 = new Leaf[scala.Int](42);

    val myLeaf3 = new Leaf[scala.Int](12);


    val myleaf5 = new Leaf[scala.Int](142);
    val myleaf6 = new Leaf[scala.Int](32);
    val myleaf7 = new Leaf[scala.Int](42);

    val myBranch1 = new Branch[scala.Int](myLeaf1,myleaf2);

    val myBranch3 = new Branch(myleaf5,myleaf6)

    val myBranch4 = new Branch(myleaf7,myBranch3)

    val myBranch2 = new Branch[scala.Int](myLeaf3,myBranch4);

    val myRoot = new Branch[scala.Int](myBranch1,myBranch2);


    def max[Int](left: scala.Int, right: scala.Int): scala.Int =  left.max(right);

    println(maximum(myRoot)(max));
  }


  /**
    * exercise 3.27 function depth that returns the maximum path length between root to any leaf
    */
  def depth[A](tree: Tree[A]): Int ={

    tree match {
      case l: Leaf[A] => 1
      case (branch: Branch[A]) => ((depth(branch.left)).max((depth(branch.right)))) + 1
    }
  }

  def mainDepth(args : Array[String]): Unit ={
    val myLeaf1 = new Leaf[scala.Int](5);
    val myleaf2 = new Leaf[scala.Int](42);

    val myLeaf3 = new Leaf[scala.Int](12);


    val myleaf5 = new Leaf[scala.Int](142);
    val myleaf6 = new Leaf[scala.Int](32);
    val myleaf7 = new Leaf[scala.Int](42);

    val myBranch1 = new Branch[scala.Int](myLeaf1,myleaf2);

    val myBranch3 = new Branch(myleaf5,myleaf6)

    val myBranch4 = new Branch(myleaf7,myBranch3)

    val myBranch2 = new Branch[scala.Int](myLeaf3,myBranch4);

    val myRoot = new Branch[scala.Int](myBranch1,myBranch2);

    println(depth(myRoot)); // 5 = root + branch 2 + branch 4 + branch 3 + leaf 1
  }



  /**
    * exercise 3.28 function map that transforms each element of the tree
    */
  def map[A,B](tree: Tree[A])(f: Leaf[A] => Leaf[B] ): Tree[B] ={

    tree match {
      case l: Leaf[A] => f(l)
      case (branch: Branch[A]) =>  new Branch(map(branch.left)(f), map(branch.right)(f))
    }
  }


  def mainMap(args : Array[String]): Unit ={
    val myLeaf1 = new Leaf[scala.Int](5);
    val myleaf2 = new Leaf[scala.Int](42);

    val myLeaf3 = new Leaf[scala.Int](12);


    val myleaf5 = new Leaf[scala.Int](142);
    val myleaf6 = new Leaf[scala.Int](32);
    val myleaf7 = new Leaf[scala.Int](42);

    val myBranch1 = new Branch[scala.Int](myLeaf1,myleaf2);

    val myBranch3 = new Branch(myleaf5,myleaf6)

    val myBranch4 = new Branch(myleaf7,myBranch3)

    val myBranch2 = new Branch[scala.Int](myLeaf3,myBranch4);

    val myRoot = new Branch[scala.Int](myBranch1,myBranch2);

    def increase(leaf: Leaf[scala.Int]): Leaf[scala.Int] =  new Leaf(leaf.value+1);

    println(myRoot) //before
    println(map(myRoot)(increase)); //after
  }



  /**
    * exercise 3.29 generalize previous functions
    */
   def fold[A,B](tree: Tree[A],default: B)(f: A => B)(g: (B,B) => B) : B ={
    tree match {
      case a: Leaf[A] => f(a.value)
      case Branch(left: Tree[A], right: Tree[A]) => g(fold(left,default)(f)(g),fold(right,default)(f)(g))
    }
  }


  def main(args : Array[String]): Unit ={
    val myLeaf1 = new Leaf[scala.Int](5);
    val myleaf2 = new Leaf[scala.Int](42);

    val myLeaf3 = new Leaf[scala.Int](12);


    val myleaf5 = new Leaf[scala.Int](142);
    val myleaf6 = new Leaf[scala.Int](32);
    val myleaf7 = new Leaf[scala.Int](42);

    val myBranch1 = new Branch[Int](myLeaf1,myleaf2);

    val myBranch3 = new Branch(myleaf5,myleaf6)

    val myBranch4 = new Branch(myleaf7,myBranch3)

    val myBranch2 = new Branch[scala.Int](myLeaf3,myBranch4);

    val myRoot = new Branch[scala.Int](myBranch1,myBranch2);


    println(myRoot) //before
    println(fold(myRoot,0)((x) => x)((x,y) => x.max(y))); // reimplement maximum function
    println(fold(myRoot,1)((x) => 1)((x,y) => if (x>=y) x+1 else y+1)); // reimplement depth function, compare with mainDepth
    println(fold(myRoot,1)((x) => 1)((x,y) => x + y + 1)); // reimplement count; = 11
    //TODO fold map

  }


}
