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

    static Stream<Arguments> getIdrisFunctionName() {
      return Stream.of(
        arguments("Core/Name/Namespace/ModuleIdent", "toPath", IdrisList.fromIterable(asList(
          "M_Core/M_Name/M_Namespace/M_ModuleIdent", "toPath"))),
        arguments("Foo", "bar", IdrisList.fromIterable(asList("main/Foo", "bar"))),
        arguments("Main/Foo", "bar", IdrisList.fromIterable(asList("M_Main/M_Foo", "bar"))),
        arguments("Main/Foo/Bar/Baz", "bar", IdrisList.fromIterable(asList("M_Main/M_Foo/M_Bar/M_Baz", "bar"))));
    }

    static Stream<Arguments> getConstructorClassName() {
      return Stream.of(
        arguments("Core/Name/Namespace/ModuleIdent", "M_Core/M_Name/M_Namespace/ModuleIdent"),
        arguments("Prelude/Foo", "M_Prelude/Foo"),
        arguments("Prelude", "main/Prelude"));
    }

    @ParameterizedTest
    @MethodSource
    void getIdrisFunctionName(String moduleName, String functionName, IdrisList idrisClassFunctionName) {
        assertThat(IdrisName.getIdrisFunctionName("main", moduleName, functionName))
            .isEqualTo(idrisClassFunctionName);
    }

    @ParameterizedTest
    @MethodSource
    void getConstructorClassName(String idrisConstructorName, String transformedConstructorName) {
        assertThat(IdrisName.getIdrisConstructorClassName("main", idrisConstructorName))
            .isEqualTo(transformedConstructorName);
    }
}
