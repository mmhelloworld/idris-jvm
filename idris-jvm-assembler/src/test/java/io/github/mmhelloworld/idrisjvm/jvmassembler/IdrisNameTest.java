package io.github.mmhelloworld.idrisjvm.jvmassembler;

import io.github.mmhelloworld.idrisjvm.assembler.IdrisName;
import io.github.mmhelloworld.idrisjvm.runtime.IdrisList;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.params.provider.Arguments.arguments;

public final class IdrisNameTest {

    static Stream<Arguments> getFunctionName() {
        return Stream.of(
            arguments("Data/List", "take", IdrisList.fromIterable(asList("M_Data/List", "take"))),
            arguments("Main", "bar", IdrisList.fromIterable(asList("main/Main", "bar"))),
            arguments("Foo", "bar", IdrisList.fromIterable(asList("main/Foo", "bar"))),
            arguments("Main/Foo", "bar", IdrisList.fromIterable(asList("M_Main/Foo", "bar"))),
            arguments("Main/Foo/Bar/Baz", "bar", IdrisList.fromIterable(asList("M_Main/M_Foo/M_Bar/Baz", "bar"))));
    }

    static Stream<Arguments> getConstructorClassName() {
        return Stream.of(
            arguments("Data/List/Take", "M_Data/M_List/Take"),
            arguments("Prelude/Foo", "M_Prelude/Foo"),
            arguments("Prelude", "main/Prelude")
        );
    }

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
}
