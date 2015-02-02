object AnimalsGame {
  import scala.io.Source
  import java.io._
  
  def main(args: Array[String]) {
    println("Computer: Think of an animal. Hit \"Return\" when ready.")
    println("User: (Hits the Return key).")
    var input = readLine
    while(input != ""){
      println("User: (Hits the Return key).")
      input = readLine     
    }
    play
    println("Thank you for playing!")  
  }
  
  def play{
    // file input - read information of binary tree as type of array from the file
    val filename = "pool.txt"
    val lines = Source.fromFile(filename).getLines().toArray
    var array = new Array[String](lines.length)
    for(i<-0 until array.length){
      if(lines(i)!="") array(i) = lines(i)
    }
    
    // convert array to binary tree
    val testtree = new ArraytoBinaryTree(array)
    val root = testtree.toBinaryTree

    // play game
    playgame(root)
    
    // update array
    val arraytree = new BinaryTreeArray(root)
    array = arraytree.toArray
    
    // file output - put the changed information of binary tree as type of array into the file
    val writer = new PrintWriter(new File(filename))
    for(i<-0 until array.length){
      if(array(i)==null) writer.write(""+"\n")
      else writer.write(array(i)+"\n")
    }
    writer.close()
    
    // play again?
    println("Computer: Thanks, I'll remember that. Would you like to play again?")
    print("User: ")
    val input = readLine.toLowerCase()
    if(input == "yes") play
    else return
  }

  def playgame(node:Option[BinaryTree[String]]) {
    var input = " "
    node match{
      case Some(node) => {
        if(node.left == None && node.right == None){
          println("Computer: OK, I've got it. Is the animal you're thinking of a "+node.value+"?")
          print("User: ")
          input = readLine.toLowerCase()
          if(input == "yes"){
            println("Computer: Haha! I am clever!")
          }
          else{
            println("Computer: What was your animal?")
            print("User: ")
            val answer = readLine.toLowerCase()
            println("Computer: What question would you ask to distinguish between a "+node.value+" and a "+answer+"?")
            print("User: ")
            val question = readLine
            println("Computer: What would the answer be for a "+answer+"?")
            print("User: ")
            val choose = readLine
            if(choose == "yes"){
              node.modify(question, Some(new BinaryTree[String](answer)), Some(new BinaryTree[String](node.value)))
            }else{
              node.modify(question, Some(new BinaryTree[String](node.value)), Some(new BinaryTree[String](answer)))
            }
          }
        }
        else{
          println("Computer: "+node.value)
          print("User: ")
          input = readLine.toLowerCase()
          if(input == "yes"){
            playgame(node.left)
          }else{
            playgame(node.right)
          }
        }
      }
    }  
  } 
}

class BinaryTree[T](var value: T, var left: Option[BinaryTree[T]], var right: Option[BinaryTree[T]]){
  def this(value: T){
    this(value, None, None)
  }
  def modify(newvalue: T, newleft: Option[BinaryTree[T]], newright: Option[BinaryTree[T]]){
    this.value = newvalue
    this.left = newleft
    this.right = newright
  }
  def examine(node: Option[BinaryTree[T]]): Boolean = {
    examine(Some(this), node)
  }
  def examine(root: Option[BinaryTree[T]], node: Option[BinaryTree[T]]): Boolean = {
    root match{
      case None => false
      case Some(root) => {
        node match{
          case None => false
          case Some(node) => {
            if(root == node) true
            else {
              examine(root.left, Some(node)) || examine(root.right, Some(node))
            }
          }
        }
      }
    }    
  }
  def navigate = toString
  override def toString = value + " " + left + " " + right;
}

class BinaryTreeArray[T: ClassManifest](val root:Option[BinaryTree[T]]){
  var treeArray:Array[T] = new Array[T](2)
  def toArray:Array[T] = {
    toArray(root, 0)
  }
  def toArray(root:Option[BinaryTree[T]], index:Int):Array[T] =  {
    root match{
      case None => {}
      case Some(root) => {
        if(index>=treeArray.length) doubleArray
        treeArray(index) = root.value
        toArray(root.left, 2*index+1)
        toArray(root.right, 2*index+2)
      }      
    }
    treeArray
  }
  def doubleArray {
    val buffer = new Array[T](treeArray.length*8)
    for(i<-0 until treeArray.length){
      buffer(i) = treeArray(i)
    }
    treeArray = buffer
  }
}

class ArraytoBinaryTree[T: ClassManifest](val treeArray:Array[T]){
  def toBinaryTree:Option[BinaryTree[T]]={
    toBinaryTree(0)
  }
  def toBinaryTree(index:Int):Option[BinaryTree[T]] = {
    if (index>=treeArray.length) return None
    else if (treeArray(index)!=null){
      return Some(new BinaryTree(treeArray(index), toBinaryTree(2*index+1), toBinaryTree(2*index+2)))
    }
    else{
      return None
    }
  }
}