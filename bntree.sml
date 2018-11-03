datatype 'a tree = LEAF | NODE of 'a tree * 'a * 'a tree

val mytree = NODE(NODE(LEAF, 9, LEAF), 8, NODE (NODE(LEAF, 3 , LEAF), 5,
NODE(LEAF, 7, LEAF)))

fun getlft LEAF = raise Empty
  | getlft (NODE (LEAF,_, LEAF)) = raise Empty
  | getlft (NODE (lft, _, rht)) = lft 

fun getrht LEAF = raise Empty
  | getrht (NODE (LEAF, _, LEAF)) = raise Empty
  | getrht (NODE (lft, _, rht)) = rht 

val lefttree = getlft(mytree)
val righttree = getrht(mytree)

fun in_order LEAF = []
  | in_order (NODE(c1, a, c2)) = in_order c1 @ [a] @ in_order c2

fun pre_order LEAF = []
  | pre_order (NODE(c1, a, c2)) = [a] @ pre_order c1 @ pre_order c2 

val l1 = in_order(mytree)
val l2 = pre_order(mytree)
