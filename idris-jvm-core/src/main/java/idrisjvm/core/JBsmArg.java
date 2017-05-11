package idrisjvm.core;

import static idrisjvm.core.JBsmArg.BsmArgType.BsmArgGetType;
import static idrisjvm.core.JBsmArg.BsmArgType.BsmArgHandle;

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
            super(BsmArgGetType);
            this.desc = desc;
        }

        public String getDesc() {
            return desc;
        }
    }

    public static final class JBsmArgHandle extends JBsmArg {
        private final JHandle handle;

        public JBsmArgHandle(final JHandle handle) {
            super(BsmArgHandle);
            this.handle = handle;
        }

        public JHandle getHandle() {
            return handle;
        }
    }

}