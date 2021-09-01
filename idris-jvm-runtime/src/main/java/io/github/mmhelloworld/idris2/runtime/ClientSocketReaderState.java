package io.github.mmhelloworld.idris2.runtime;

import java.nio.ByteBuffer;

final class ClientSocketReaderState {
    private ByteBuffer buffer;
    private final ResettableCountDownLatch doneSignal;
    private int bytesRead;

    ClientSocketReaderState(ByteBuffer buffer, ResettableCountDownLatch doneSignal) {
        this.buffer = buffer;
        this.doneSignal = doneSignal;
    }

    ByteBuffer getBuffer() {
        return buffer;
    }

    ResettableCountDownLatch getDoneSignal() {
        return doneSignal;
    }

    int getBytesRead() {
        return bytesRead;
    }

    void setBytesRead(int bytesRead) {
        this.bytesRead = bytesRead;
    }
}
