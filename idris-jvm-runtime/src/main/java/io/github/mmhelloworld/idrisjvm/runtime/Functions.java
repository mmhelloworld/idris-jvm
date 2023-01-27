package io.github.mmhelloworld.idrisjvm.runtime;

import java.util.function.BiFunction;
import java.util.function.Function;

public final class Functions {
    public static final Function<?, ?> IDENTITY = a -> a;
    public static final Function<?, Function<?, ?>> IDENTITY_1 = c -> IDENTITY;
    public static final Function<?, Function<?, Function<?, ?>>> IDENTITY_2 = d -> IDENTITY_1;
    public static final Function<?, Function<?, ?>> CONSTANT = a -> b -> a;
    public static final Function<?, Function<?, Function<?, ?>>> CONSTANT_1 = c -> CONSTANT;

    private Functions() {
    }

    public static <T1, T2, R> Function<T1, Function<T2, R>> curry(BiFunction<T1, T2, R> f) {
        return t1 -> t2 -> f.apply(t1, t2);
    }

    public static <T1, T2, T3, R> Function<T1, Function<T2, Function<T3, R>>> curry(Function3<T1, T2, T3, R> f) {
        return t1 -> t2 -> t3 -> f.apply(t1, t2, t3);
    }

    public static <T1, T2, T3, T4, R> Function<T1, Function<T2, Function<T3, Function<T4, R>>>> curry(
        Function4<T1, T2, T3, T4, R> f) {
        return t1 -> t2 -> t3 -> t4 -> f.apply(t1, t2, t3, t4);
    }

    public static <T1, T2, T3, T4, T5, R> Function<T1, Function<T2, Function<T3, Function<T4, Function<T5, R>>>>> curry(
        Function5<T1, T2, T3, T4, T5, R> f) {
        return t1 -> t2 -> t3 -> t4 -> t5 -> f.apply(t1, t2, t3, t4, t5);
    }
}
