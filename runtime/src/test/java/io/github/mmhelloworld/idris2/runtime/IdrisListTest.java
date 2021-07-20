package io.github.mmhelloworld.idris2.runtime;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class IdrisListTest {

    @Test
    void reverse() {
        assertThat(IdrisList.reverse(IdrisList.Nil.INSTANCE)).isEqualTo(IdrisList.Nil.INSTANCE);
        assertThat(IdrisList.reverse(new IdrisList.Cons(1, new IdrisList.Cons(2, new IdrisList.Cons(3, IdrisList.Nil.INSTANCE)))))
            .isEqualTo(new IdrisList.Cons(3, new IdrisList.Cons(2, new IdrisList.Cons(1, IdrisList.Nil.INSTANCE))));
    }

    @Test
    void fastPack() {
        assertThat(IdrisList.fastPack(IdrisList.Nil.INSTANCE)).isEmpty();
        assertThat(IdrisList.fastPack(new IdrisList.Cons(1, new IdrisList.Cons(2, IdrisList.Nil.INSTANCE))))
            .isEqualTo("12");
        assertThat(IdrisList.fastPack(new IdrisList.Cons(1, new IdrisList.Cons(2, new IdrisList.Cons(3, IdrisList.Nil.INSTANCE)))))
            .isEqualTo("123");
    }
}