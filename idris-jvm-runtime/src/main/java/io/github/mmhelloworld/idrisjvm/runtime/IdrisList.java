package io.github.mmhelloworld.idrisjvm.runtime;

import java.util.AbstractSequentialList;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;

import static java.util.Collections.emptyListIterator;

public abstract class IdrisList extends AbstractSequentialList<Object> implements IdrisObject {
    private IdrisList() {
    }

    public static IdrisList fromIterable(Iterable<?> iterable) {
        List<Object> items = new ArrayList<>();
        iterable.forEach(items::add);
        return fromArray(items.toArray());
    }

    public static IdrisList fromArray(boolean[] values) {
        IdrisList list = Nil.INSTANCE;
        for (int index = values.length - 1; index >= 0; index--) {
            list = new Cons(values[index], list);
        }
        return list;
    }

    public static IdrisList fromArray(byte[] values) {
        IdrisList list = Nil.INSTANCE;
        for (int index = values.length - 1; index >= 0; index--) {
            list = new Cons(values[index], list);
        }
        return list;
    }

    public static IdrisList fromArray(char[] values) {
        IdrisList list = Nil.INSTANCE;
        for (int index = values.length - 1; index >= 0; index--) {
            list = new Cons(values[index], list);
        }
        return list;
    }

    public static IdrisList fromArray(short[] values) {
        IdrisList list = Nil.INSTANCE;
        for (int index = values.length - 1; index >= 0; index--) {
            list = new Cons(values[index], list);
        }
        return list;
    }

    public static IdrisList fromArray(int[] values) {
        IdrisList list = Nil.INSTANCE;
        for (int index = values.length - 1; index >= 0; index--) {
            list = new Cons(values[index], list);
        }
        return list;
    }

    public static IdrisList fromArray(long[] values) {
        IdrisList list = Nil.INSTANCE;
        for (int index = values.length - 1; index >= 0; index--) {
            list = new Cons(values[index], list);
        }
        return list;
    }

    public static IdrisList fromArray(float[] values) {
        IdrisList list = Nil.INSTANCE;
        for (int index = values.length - 1; index >= 0; index--) {
            list = new Cons(values[index], list);
        }
        return list;
    }

    public static IdrisList fromArray(double[] values) {
        IdrisList list = Nil.INSTANCE;
        for (int index = values.length - 1; index >= 0; index--) {
            list = new Cons(values[index], list);
        }
        return list;
    }

    public static IdrisList fromArray(Object[] values) {
        IdrisList list = Nil.INSTANCE;
        for (int index = values.length - 1; index >= 0; index--) {
            list = new Cons(values[index], list);
        }
        return list;
    }

    public static IdrisList reverse(IdrisList list) {
        IdrisList current = list;
        IdrisList result = Nil.INSTANCE;
        while (current != Nil.INSTANCE) {
            Cons cons = ((Cons) current);
            result = new Cons(cons.head, result);
            current = (IdrisList) cons.tail;
        }
        return result;
    }

    public abstract int getConstructorId();

    public static final class Nil extends IdrisList {
        public static final Nil INSTANCE = new Nil();

        private Nil() {
        }

        @Override
        public int size() {
            return 0;
        }

        @Override
        public Object get(int index) {
            throw new IndexOutOfBoundsException("Index: " + index);
        }

        @Override
        public ListIterator<Object> listIterator(int i) {
            return emptyListIterator();
        }

        @Override
        public int getConstructorId() {
            return 0;
        }
    }

    public static final class Cons extends IdrisList {
        private final Object head;
        private final Object tail;

        public Cons(Object head, Object tail) {
            this.head = head;
            this.tail = tail;
        }

        public Cons(Object head, IdrisList tail) {
            this(head, (Object) tail);
        }

        @Override
        public int getConstructorId() {
            return 1;
        }

        @Override
        public Object getProperty(int index) {
            switch (index) {
                case 0:
                    return head;
                case 1:
                    return tail;
                default:
                    throw new NoSuchElementException("No property at " + index + " for an Idris list");
            }
        }

        @Override
        public ListIterator<Object> listIterator(int index) {
            int size = size();
            if (index < 0 || index > size) {
                throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size);
            }
            return new Iterator(index, this);
        }

        @Override
        public int size() {
            return 1 + ((IdrisList) tail).size();
        }

        private static final class Node {
            private final Object element;
            private Node previous;
            private Node next;

            private Node(Node previous, Object element, Node next) {
                this.previous = previous;
                this.element = element;
                this.next = next;
            }

            private Node(Object element) {
                this(null, element, null);
            }
        }

        private static final class Iterator implements ListIterator<Object> {
            private static final Object EMPTY_ELEMENT = new Object();
            private int index;
            private Node node;

            private Iterator(int index, IdrisList idrisList) {
                this.index = index;
                this.node = createNode(index, idrisList);
            }

            private static Node createNode(int index, IdrisList idrisList) {
                if (idrisList == Nil.INSTANCE) {
                    return null;
                }
                Cons cons = (Cons) idrisList;
                Node currNode = new Node(cons.head);
                int startIndex = -1;
                Node start = new Node(null, EMPTY_ELEMENT, currNode);
                for (IdrisList currList = (IdrisList) cons.tail; currList != Nil.INSTANCE; currList =
                    (IdrisList) ((Cons) currList).tail) {
                    Cons newCons = (Cons) currList;
                    Node newNode = new Node(newCons.head);
                    currNode.next = newNode;
                    newNode.previous = currNode;
                    currNode = currNode.next;
                    if (startIndex + 1 < index) {
                        start = newNode.previous;
                        startIndex++;
                    }
                }
                return startIndex + 1 != index ? null : start;
            }

            @Override
            public boolean hasNext() {
                return node != null && node.next != null;
            }

            @Override
            public Object next() {
                if (node == null || node.next == null) {
                    throw new NoSuchElementException();
                }
                Object element = node.next.element;
                node = node.next;
                index++;
                return element;
            }

            @Override
            public boolean hasPrevious() {
                return index > 0;
            }

            @Override
            public Object previous() {
                if (node == null || node.element == EMPTY_ELEMENT) {
                    throw new NoSuchElementException();
                }
                Object element = node.element;
                node = node.previous;
                index--;
                return element;
            }

            @Override
            public int nextIndex() {
                return index;
            }

            @Override
            public int previousIndex() {
                return index - 1;
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException("immutable list");
            }

            @Override
            public void set(Object o) {
                throw new UnsupportedOperationException("immutable list");
            }

            @Override
            public void add(Object o) {
                throw new UnsupportedOperationException("immutable list");
            }
        }
    }
}
