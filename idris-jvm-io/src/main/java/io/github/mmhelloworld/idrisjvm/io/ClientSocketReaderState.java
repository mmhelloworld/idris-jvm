package io.github.mmhelloworld.idrisjvm.io;

import java.nio.ByteBuffer;
import java.util.concurrent.CountDownLatch;

final class ClientSocketReaderState {
    private final ByteBuffer buffer;
    private final CountDownLatch doneSignal;
    private int bytesRead;

    ClientSocketReaderState(ByteBuffer buffer, CountDownLatch doneSignal) {
        this.buffer = buffer;
        this.doneSignal = doneSignal;
    }

    ByteBuffer getBuffer() {
        return buffer;
    }

    CountDownLatch getDoneSignal() {
        return doneSignal;
    }

    int getBytesRead() {
        return bytesRead;
    }

    void setBytesRead(int bytesRead) {
        this.bytesRead = bytesRead;
    }
}
