package net.thenobody.codinginterviews.linkedlists;

import java.util.Arrays;
import java.util.Collection;

/**
 * Created by antonvanco on 11/02/2016.
 */
public class LinkedLists {

    public static <T> LinkedNode<T> removeAllOccurrences(LinkedNode<T> item, LinkedNode<T> head) {
        if (head == null) {
            return null;
        }

        LinkedNode<T> tail = head.getNextNode();

        tail = removeAllOccurrences(item, tail);
        if (item.equals(head)) {
            return tail;
        } else {
            head.setNextNode(tail);
            return head;
        }
    }

    public static <T> void removeDuplicates(LinkedNode<T> head) {
        if (head == null) {
            return;
        }

        LinkedNode<T> tail = head.getNextNode();

        tail = removeAllOccurrences(head, tail);
        removeDuplicates(tail);
        head.setNextNode(tail);
     }

    public static void partitionNumbers(Integer pivot, LinkedNode<Integer> numbers) {
        LinkedNode<Integer> ltNumbers = new LinkedNode<>(null);
        LinkedNode<Integer> gteNumbers = new LinkedNode<>(null);

        LinkedNode<Integer> next = null;
        do {
            next = numbers.getNextNode();
            if (pivot < numbers.value) {
                numbers.setNextNode(ltNumbers);
                ltNumbers = numbers;
            } else {
                gteNumbers.setNextNode(numbers);
                numbers.setNextNode(null);
            }
        } while (next != null);
        ltNumbers.setNextNode(gteNumbers.getNextNode());
    }


    public static void main(String[] args) {
        String input = "FOLLOW UPP FOLLOW UP XX YYY";
        LinkedNode<String> build = LinkedList.build(input);

        System.out.println(build);
        removeDuplicates(build);
        System.out.println(build);
    }


}

class LinkedList {

    public static <T> LinkedNode<T> build(Collection<T> input) {
        LinkedNode<T> result = new LinkedNode<>(null);
        LinkedNode<T> currentNode = result;
        for (T value : input) {
            currentNode.setNextNode(new LinkedNode<>(value));
            currentNode = currentNode.getNextNode();
        }

        return result.getNextNode();
    }

    public static LinkedNode<String> build(String input) {
        return build(Arrays.asList(input.split("")));
    }
}

class LinkedNode<T> {

    public final T value;
    private LinkedNode<T> nextNode = null;

    public LinkedNode(T value) {
        this.value = value;
    }

    public LinkedNode<T> getNextNode() {
        return nextNode;
    }

    public void setNextNode(LinkedNode<T> nextNode) {
        this.nextNode = nextNode;
    }

    public boolean hasNext() {
        return this.nextNode != null;
    }

    public LinkedNode<T> getNode(int index) {
        if (index == 0) {
            return this;
        } else {
            return getNextNode().getNode(index - 1);
        }
    }

    public LinkedNode<T> deleteNode(int index) {
        if (index == 0) {
            return getNextNode();
        } else {
            getNextNode().delete(index - 1, this);
            return this;
        }
    }

    private void delete(int index, LinkedNode<T> parent) {
        if (index == 0) {
            parent.setNextNode(getNextNode());
            this.setNextNode(null);
        } else {
            getNextNode().delete(index - 1, this);
        }
    }

    public int size() {
        int result = 1;
        LinkedNode<T> node = this;
        while (node.hasNext()) {
            node = node.getNextNode();
            result++;
        }

        return result;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(value);
        LinkedNode<T> node = this;
        do {
            sb.append("-");
            node = node.getNextNode();
            sb.append(node.value.toString());
        } while (node.hasNext());

        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        LinkedNode<?> that = (LinkedNode<?>) o;

        return value.equals(that.value);

    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }
}
