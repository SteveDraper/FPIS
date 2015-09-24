import exercises.fold.{MyNil, Cons, MyList, Tree, None}

Nil
MyNil
val One: MyList[Int] = Cons(1,MyNil)
val intList = MyList(1,2,3, 4)
val intList2 = MyList(4,3,2,1)

MyList.foldLeft(intList, 0)(_ + _)
MyList.foldLeft(intList, 1)(_ * _)
MyList.foldLeft(intList, 0)((a,l) => l + 1)
MyList.reverse(intList)
MyList.foldRight(intList, 0)(_ + _)
MyList.foldRight(intList, 1)(_ * _)
MyList.foldRight(intList, 0)((a,l) => l + 1)
MyList.foldRight(intList, MyNil: MyList[Int])(Cons(_,_))
MyList.append(intList,intList2)
MyList.append(intList,intList)

val intLists = MyList(intList, intList2)
val flattened = MyList.flatten(intLists)
val joined = MyList.append(intList,intList2)
flattened == joined

MyList.map(intList)(_+1)

MyList.filter(intList)(_%2==0)

MyList.flatmap(intList)(x => MyList(x+1,x))

MyList.zipWith(intList, intList2)(_*_)

MyList.hasSubSequence(intList, MyList(3,4,5))

val tree1 = Tree(Tree(Tree(1),Tree(2)),Tree(Tree(3),Tree(4)))
val tree2 = Tree(Tree(Tree(1),Tree(2)),Tree(3))

tree1 size

tree1 depth

tree2.size

tree2.depth

Tree.max(tree1)(Math.max)

tree1.map(x => "Value is " + x)

Tree.fold(tree1, 0)((el,acc) => acc+1)((acc1,acc2) => acc1+acc2+1)

Tree.fold(tree1,0)((x,acc) => 0)((l,r)=>Math.max(l,r)+1)

Tree.fold(tree1,None: Tree[String])((x,_)=>Tree("Value is "+x))((l,r)=>Tree(l,r))