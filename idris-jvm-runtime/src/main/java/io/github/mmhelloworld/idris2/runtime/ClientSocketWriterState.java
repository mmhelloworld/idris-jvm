package io.github.mmhelloworld.idris2.runtime;

import java.nio.ByteBuffer;

final class ClientSocketWriterState {
    private ByteBuffer buffer;
    private final ResettableCountDownLatch doneSignal;
    private int bytesWritten;

    ClientSocketWriterState(ByteBuffer buffer, ResettableCountDownLatch doneSignal) {
        this.buffer = buffer;
        this.doneSignal = doneSignal;
    }

    void reset() {
        doneSignal.reset();
    }

    ByteBuffer getBuffer() {
        return buffer;
    }

    ResettableCountDownLatch getDoneSignal() {
        return doneSignal;
    }

    int getBytesWritten() {
        return bytesWritten;
    }

    void setBytesWritten(int bytesWritten) {
        this.bytesWritten = bytesWritten;
    }
}
