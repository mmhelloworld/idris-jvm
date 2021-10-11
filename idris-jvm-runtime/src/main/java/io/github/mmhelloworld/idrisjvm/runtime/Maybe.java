package io.github.mmhelloworld.idrisjvm.runtime;

public abstract class Maybe implements IdrisObject {
    private Maybe() {
    }

    public abstract int getConstructorId();

    public static final class Nothing extends Maybe {
        public static final Nothing INSTANCE = new Nothing();

        private Nothing() {
        }

        @Override
        public int getConstructorId() {
            return 0;
        }

        @Override
        public String toString() {
            return "Nothing";
        }
    }

    public static final class Just extends Maybe {
        private final Object value;

        public Just(Object value) {
            this.value = value;
        }

        @Override
        public int getConstructorId() {
            return 1;
        }

        @Override
        public Object getProperty(int index) {
            return index == 0 ? value : null;
        }

        @Override
        public String toString() {
            return "Just(" + value + ")";
        }
    }
}
