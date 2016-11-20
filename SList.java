// Linked Lists and Dequeues

public class SList{
	private class SNode {
		public int item;
		public SNode next;
		public SNode (int item, SNode next){
			this.item = item;
			this.next = next;
		}
	}

	private SNode front;
	//private SNode back;

	public void insertFront(int x){
		front  = new SNode(x,front);
	}

	public void insertFront(int x, SNode back){
		back  = new SNode(x,back);
	}

	public int returnFront(){
		//spit out the first item and shift stack to the left
		int top = front.item;
		front = front.next;
		return top;
	}
	// public int returnFront(SNode back){
	// 	//spit out the first item and shift stack to the left
	// 	int top = back.item;
	// 	back = back.next;
	// 	//back.item = back.next.item;
	// 	//back.next = back.next.next;
	// 	return top;
	// }

	public void insert(int item, int position){
		//new item =  (SNode[0:position-1], item, SNode[position:end])

		//pop out front and store until you reach the position you want insert,
		//then, pop the stored items back in.

		//...oh! I can use the same list-type to store this! Ha.

		//pop out until position (count = ?)

		// 0 1 2 3 4
		// position = 2 = inserting to the right of position 2
		int[] holder = int[position];
		SNode current = front;
		for (int i=0; i<position; i++) {
			holder[position-i] = current.item;
			current = current.next;
		}

		// if position = 2, then you're now at node 2

		insertFront(item);

		//This still has the problem of popping everything back in, though...

		for (int i=0;i<position;i++){
			insertFront(holder[i],current)
			//I guess I do need the generalized variant to make this work.

		}

		

		// SNode newnode = new SNode(item, current.next);
		// current.next = newnode;

		// SNode track = insertFront(returnFront(),null);

		// for (int i=1; i<position; i++){
		// 	track = new SNode(returnFront(), track);
		// }

		// insertFront(item);
		// for (int i=1; i<position; i++){
		// 	front = new SNode(returnFront(track), front);
		// }
		
		

	}

}




//Scrap

	//bluh = new SNode(item, front[position:])
	//	= front.next
	//	front = new SNode(,bluh)
	//	position = position-1
		//for (int i=position; i>=0; i--){
		//for i in 0 to position{ //wait, no, that reverses it...
			//insertFront([i])
			//front = new SNode(, front)
			//I overwrote front, that may have been a mistake. Think...
			//