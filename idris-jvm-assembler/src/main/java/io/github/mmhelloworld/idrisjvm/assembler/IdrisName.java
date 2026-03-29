package io.github.mmhelloworld.idrisjvm.assembler;

import io.github.mmhelloworld.idrisjvm.runtime.IdrisList;
import io.github.mmhelloworld.idrisjvm.runtime.IdrisList.Cons;
import io.github.mmhelloworld.idrisjvm.runtime.IdrisList.Nil;

import java.util.HashMap;
import java.util.Map;

public final class IdrisName {

  private static final Map<Character, String> REPLACEMENTS = new HashMap<>();
  private static final Map<String, String> NORMALIZED_NAMES_CACHE = new HashMap<>();

  static {
    REPLACEMENTS.put(' ', "$s");
    REPLACEMENTS.put('!', "$not");
    REPLACEMENTS.put('"', "$d");
    REPLACEMENTS.put('#', "$hash");
    REPLACEMENTS.put('%', "$mod");
    REPLACEMENTS.put('&', "$and");
    REPLACEMENTS.put('\'', "$q");
    REPLACEMENTS.put('(', "$lpar");
    REPLACEMENTS.put(')', "$rpar");
    REPLACEMENTS.put('*', "$mul");
    REPLACEMENTS.put('+', "$add");
    REPLACEMENTS.put(',', "$com");
    REPLACEMENTS.put('-', "$hyp");
    REPLACEMENTS.put('.', "$dot");
    REPLACEMENTS.put('/', "$div");
    REPLACEMENTS.put('\\', "$bsl");
    REPLACEMENTS.put(':', "$col");
    REPLACEMENTS.put(';', "$scol");
    REPLACEMENTS.put('<', "$lt");
    REPLACEMENTS.put('=', "$eq");
    REPLACEMENTS.put('>', "$gt");
    REPLACEMENTS.put('?', "$ques");
    REPLACEMENTS.put('@', "$at");
    REPLACEMENTS.put('^', "$caret");
    REPLACEMENTS.put('`', "$grave");
    REPLACEMENTS.put('{', "$lbr");
    REPLACEMENTS.put('|', "$or");
    REPLACEMENTS.put('}', "$rbr");
    REPLACEMENTS.put('~', "$tilde");
    REPLACEMENTS.put('[', "$lsqr");
    REPLACEMENTS.put(']', "$rsqr");
  }
  private IdrisName() {
  }

  /**
   * Transforms a constructor name into a class name.
   * Rules:
   * - If the constructor name has no "/" separator, use programName + "/" + constructorName
   * - If it has separators, prefix all segments except the last with "M_"
   * - The last segment remains without "M_" prefix
   */
  public static String getIdrisConstructorClassName(String programName, String idrisConstructorName) {
    if (!idrisConstructorName.contains("/")) {
      // No path separator, use programName prefix
      return programName + "/" + idrisConstructorName;
    }

    // Split by "/" and prefix all but the last segment with "M_"
    String[] segments = idrisConstructorName.split("/");
    StringBuilder builder = new StringBuilder();

    for (int index = 0; index < segments.length; index++) {
      if (index > 0) {
        builder.append("/");
      }
      if (index < segments.length - 1) {
        // All segments except the last get "M_" prefix
        builder.append("M_").append(segments[index]);
      } else {
        // Last segment has no prefix
        builder.append(segments[index]);
      }
    }

    return builder.toString();
  }

  /**
   * Transforms a module name and function name into an IdrisList containing
   * the transformed class name and function name.<br>
   * <b>Rules:</b><br>
   * - If moduleName has no "/" (single segment), use programName + "/" + moduleName <br>
   * - If moduleName has "/", split by "/" and prefix each segment with "M_", joining with "/" <br>
   * - Return an IdrisList with [transformedModuleName, functionName]
   */
  public static IdrisList getIdrisFunctionName(String programName, String idrisNamespace, String idrisFunctionName) {
    String className;
    if (idrisNamespace.startsWith("io/github/mmhelloworld/idrisjvm")) {
      className = idrisNamespace;
    } else if (idrisNamespace.startsWith("nomangle:")) {
      className = idrisNamespace.substring("nomangle:".length());
    } else {
      className = getIdrisFunctionClassName(programName, idrisNamespace);
    }
    return new Cons(className, new Cons(idrisFunctionName, Nil.INSTANCE));
  }

  private static String getIdrisFunctionClassName(String programName, String moduleName) {
    if (!moduleName.contains("/")) {
      // Single segment module gets programName + "/" prefix
      return programName + "/" + moduleName;
    } else {
      // Split by "/" and prefix each segment with "M_"
      String[] segments = moduleName.split("/");
      StringBuilder builder = new StringBuilder();
      for (int index = 0; index < segments.length; index++) {
        if (index > 0) {
          builder.append("/");
        }
        builder.append("M_").append(segments[index]);
      }
      return builder.toString();
    }
  }

  public static String transformCharacters(String value) {
    return NORMALIZED_NAMES_CACHE.computeIfAbsent(value, IdrisName::transformCharactersNoCache);
  }

  private static String transformCharactersNoCache(String value) {
    StringBuilder builder = new StringBuilder();
    for (char c : value.toCharArray()) {
      builder.append(transformCharacter(c));
    }
    return builder.toString();
  }

  private static String transformCharacter(char c) {
    return REPLACEMENTS.getOrDefault(c, String.valueOf(c));
  }

}
