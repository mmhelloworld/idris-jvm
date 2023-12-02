package io.github.mmhelloworld.idrisjvm.runtime;

import io.github.mmhelloworld.idrisjvm.runtime.Paths;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

import java.nio.file.Path;

public class PathsTest {

  @Test
  void test() {
    String cwd = Directories.getWorkingDir();
    // File system separator
    String sep = System.getProperty("file.separator");

    assertThat(Paths.createPath(null).toString()).isEqualTo(cwd);
    assertThat(Paths.createPath("").toString()).isEqualTo(cwd);
    assertThat(Paths.createPath(cwd).toString()).isEqualTo(cwd);

    assertThat(Paths.createPath("a").toString()).isEqualTo(
      cwd + sep + "a");
    assertThat(Paths.createPath("a" + sep +"b").toString()).isEqualTo(
      cwd + sep + "a" + sep + "b");

    assertThat(Paths.createPath(".." + sep + "a").toString()).isEqualTo(
      Paths.createPath(cwd).getParent() + sep + "a");
    assertThat(Paths.createPath("." + sep + "a").toString()).isEqualTo(
      Paths.createPath(cwd) + sep + "a");
  }
}
