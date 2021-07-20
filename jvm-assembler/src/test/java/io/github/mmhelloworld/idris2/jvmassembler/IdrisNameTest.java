package io.github.mmhelloworld.idris2.jvmassembler;

import io.github.mmhelloworld.idris2.runtime.IdrisList;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.arguments;

public class IdrisNameTest {

    @ParameterizedTest
    @MethodSource
    void getFunctionName(String moduleName, String functionName, IdrisList idrisClassFunctionName) {
        assertThat(IdrisName.getIdrisFunctionName("main", moduleName, functionName))
            .isEqualTo(idrisClassFunctionName);
    }

    @ParameterizedTest
    @MethodSource
    void getConstructorClassName(String idrisConstructorName, String transformedConstructorName) {
        assertThat(IdrisName.getIdrisConstructorClassName(idrisConstructorName))
            .isEqualTo(transformedConstructorName);
    }

    static Stream<Arguments> getFunctionName() {
        return Stream.of(
            arguments("Data/List", "take", IdrisList.fromIterable(asList("M$Data/List","take"))),
            arguments("Main", "bar", IdrisList.fromIterable(asList("main/Main","bar"))),
            arguments("Foo", "bar", IdrisList.fromIterable(asList("main/Foo","bar"))),
            arguments("Main/Foo", "bar", IdrisList.fromIterable(asList("M$Main/Foo","bar"))),
            arguments("Main/Foo/Bar/Baz", "bar", IdrisList.fromIterable(asList("M$Main/M$Foo/M$Bar/Baz","bar"))));
    }

    static Stream<Arguments> getConstructorClassName() {
        return Stream.of(
            arguments("Data/List/Take", "M$Data/M$List/Take"),
            arguments("Prelude/Foo", "M$Prelude/Foo"),
            arguments("Prelude", "main/Prelude")
        );
    }
}
