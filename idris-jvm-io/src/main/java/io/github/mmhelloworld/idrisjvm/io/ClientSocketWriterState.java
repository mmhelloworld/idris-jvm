package io.github.mmhelloworld.idrisjvm.io;

import java.nio.ByteBuffer;
import java.util.concurrent.CountDownLatch;

final class ClientSocketWriterState {
    private final ByteBuffer buffer;
    private final CountDownLatch doneSignal;
    private int bytesWritten;

    ClientSocketWriterState(ByteBuffer buffer, CountDownLatch doneSignal) {
        this.buffer = buffer;
        this.doneSignal = doneSignal;
    }

    ByteBuffer getBuffer() {
        return buffer;
    }

    CountDownLatch getDoneSignal() {
        return doneSignal;
    }

    int getBytesWritten() {
        return bytesWritten;
    }

    void setBytesWritten(int bytesWritten) {
        this.bytesWritten = bytesWritten;
    }
}
