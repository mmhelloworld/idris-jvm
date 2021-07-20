package io.github.mmhelloworld.idris2.jvmassembler;

public abstract class JBsmArg {
    private final BsmArgType type;

    private JBsmArg(final BsmArgType type) {
        this.type = type;
    }

    public BsmArgType getType() {
        return type;
    }

    public enum BsmArgType {
        BsmArgGetType,
        BsmArgHandle
    }

    public static final class JBsmArgGetType extends JBsmArg {
        private final String desc;

        public JBsmArgGetType(final String desc) {
            super(BsmArgType.BsmArgGetType);
            this.desc = desc;
        }

        public String getDesc() {
            return desc;
        }

        @Override
        public String toString() {
            return "JBsmArgGetType{" +
                "type=" + getType() +
                ", desc='" + desc + '\'' +
                '}';
        }
    }

    public static final class JBsmArgHandle extends JBsmArg {
        private final JHandle handle;

        public JBsmArgHandle(final JHandle handle) {
            super(BsmArgType.BsmArgHandle);
            this.handle = handle;
        }

        public JHandle getHandle() {
            return handle;
        }

        @Override
        public String toString() {
            return "JBsmArgHandle{" +
                "type=" + getType() +
                ", handle=" + handle +
                '}';
        }
    }

    @Override
    public String toString() {
        return "JBsmArg{" +
            "type=" + type +
            '}';
    }
}