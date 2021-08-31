package io.github.mmhelloworld.idris2.runtime;

import io.github.mmhelloworld.idris2.runtime.IdrisList.Nil;
import org.junit.jupiter.api.Test;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;

class StringsTest {

    @Test
    void fastPack() {
        assertThat(Strings.pack(Nil.INSTANCE)).isEmpty();
        assertThat(Strings.pack(new IdrisList.Cons('1', new IdrisList.Cons('2', Nil.INSTANCE))))
            .isEqualTo("12");
        assertThat(Strings.pack(new IdrisList.Cons('1', new IdrisList.Cons('2', new IdrisList.Cons('3',
            Nil.INSTANCE)))))
            .isEqualTo("123");
    }

    @Test
    void concat() {
        assertThat(Strings.concat(Nil.INSTANCE)).isEmpty();
        assertThat(Strings.concat(IdrisList.fromIterable(asList("foo", "bar", "baz"))))
            .isEqualTo("foobarbaz");
    }

    @Test
    void pack() {
        assertThat(Strings.pack(Nil.INSTANCE)).isEmpty();
        assertThat(Strings.pack(IdrisList.fromIterable(asList('a', 'b', 'c'))))
            .isEqualTo("abc");
    }
}