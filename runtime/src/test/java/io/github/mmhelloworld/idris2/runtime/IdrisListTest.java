package io.github.mmhelloworld.idris2.runtime;

import io.github.mmhelloworld.idris2.runtime.IdrisList.Cons;
import io.github.mmhelloworld.idris2.runtime.IdrisList.Nil;
import org.junit.jupiter.api.Test;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.util.Arrays.asList;

class IdrisListTest {

    @Test
    void reverse() {
        assertThat(IdrisList.reverse(Nil.INSTANCE)).isEqualTo(Nil.INSTANCE);
        assertThat(IdrisList.reverse(new Cons(1, new Cons(2, new Cons(3, Nil.INSTANCE)))))
            .isEqualTo(new Cons(3, new Cons(2, new Cons(1, Nil.INSTANCE))));
    }

    @Test
    void fromArray_char() {
        assertThat(IdrisList.fromArray(new String[] {})).isEmpty();
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