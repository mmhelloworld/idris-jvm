package io.github.mmhelloworld.idrisjvm.runtime;

import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.util.Arrays.asList;

class IdrisListTest {

    @Test
    void reverse() {
        assertThat(IdrisList.reverse(IdrisList.Nil.INSTANCE)).isEqualTo(IdrisList.Nil.INSTANCE);
        assertThat(IdrisList.reverse(new IdrisList.Cons(1, new IdrisList.Cons(2,
            new IdrisList.Cons(3, IdrisList.Nil.INSTANCE)))))
            .isEqualTo(new IdrisList.Cons(3, new IdrisList.Cons(2,
                new IdrisList.Cons(1, IdrisList.Nil.INSTANCE))));
    }

    @Test
    void fromArray_char() {
        assertThat(IdrisList.fromArray(new String[]{})).isEmpty();
        assertThat(IdrisList.fromArray("helloworld".toCharArray()))
            .containsExactlyElementsOf(asList("helloworld".toCharArray()));
    }

    @Test
    void fromIterable() {
        assertThat(IdrisList.fromIterable(emptyList())).isEmpty();
        assertThat(IdrisList.fromIterable(singletonList("A"))).containsOnly("A");
        assertThat(IdrisList.fromIterable(asList("helloworld".toCharArray())))
            .containsExactlyElementsOf(asList("helloworld".toCharArray()));
    }
}
