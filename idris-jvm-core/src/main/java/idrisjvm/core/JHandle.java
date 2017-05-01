package idrisjvm.core;

public final class JHandle {
    private final int tag;
    private final String cname;
    private final String mname;
    private final String desc;
    private final boolean isIntf;

    public JHandle(final int tag,
                   final String cname,
                   final String mname,
                   final String desc,
                   final boolean isIntf) {
        this.tag = tag;
        this.cname = cname;
        this.mname = mname;
        this.desc = desc;
        this.isIntf = isIntf;
    }

    public int getTag() {
        return tag;
    }

    public String getCname() {
        return cname;
    }

    public String getMname() {
        return mname;
    }

    public String getDesc() {
        return desc;
    }

    public boolean isIntf() {
        return isIntf;
    }
}
