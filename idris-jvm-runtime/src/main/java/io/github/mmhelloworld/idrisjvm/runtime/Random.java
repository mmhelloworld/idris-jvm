package io.github.mmhelloworld.idrisjvm.runtime;

public final class Random {
    private static final java.util.Random RANDOM = new java.util.Random();

    private Random() {
    }

    public static int nextInt(int bound) {
        return RANDOM.nextInt(bound);
    }

    public static double nextDouble() {
        return RANDOM.nextDouble();
    }

    public static void setSeed(long seed) {
        RANDOM.setSeed(seed);
    }
}
