package io.github.mmhelloworld.idrisjvm.runtime;

import java.math.BigInteger;

import static java.lang.System.currentTimeMillis;
import static java.lang.System.nanoTime;
import static java.util.concurrent.TimeUnit.MILLISECONDS;

public class IdrisSystem {
    public static IdrisObject getIdrisClock() {
        BigInteger seconds = BigInteger.valueOf(MILLISECONDS.toSeconds(currentTimeMillis()));
        BigInteger nanoSeconds = BigInteger.valueOf(nanoTime());
        return new IdrisObject(0, seconds, nanoSeconds);
    }
}
